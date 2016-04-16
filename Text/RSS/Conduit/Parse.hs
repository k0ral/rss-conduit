{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
-- | Streaming parsers for the RSS 2.0 standard.
module Text.RSS.Conduit.Parse
  ( -- * Top-level
    rssDocument
    -- * Elements
  , rssCategory
  , rssCloud
  , rssEnclosure
  , rssGuid
  , rssImage
  , rssItem
  , rssSkipDays
  , rssSkipHours
  , rssSource
  , rssTextInput
  ) where

-- {{{ Imports
import           Text.RSS.Types

import           Control.Applicative
import           Control.Foldl                hiding (mconcat)
import           Control.Monad                hiding (foldM)
import           Control.Monad.Catch

import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML
import           Data.Maybe
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Set                     hiding (fold)
import           Data.Text                    as Text hiding (cons, last, map,
                                                       snoc)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Version
import           Data.XML.Types

import           Lens.Simple

import           Prelude                      hiding (last, lookup)

import           Text.Parser.Combinators
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read                    (readMaybe)

import           URI.ByteString
-- }}}

-- {{{ Util
asRssURI :: (MonadThrow m) => Text -> m RssURI
asRssURI t = case (parseURI' t, parseRelativeRef' t) of
  (Right u, _) -> return $ RssURI u
  (_, Right u) -> return $ RssURI u
  (_, Left e) -> throwM $ InvalidURI e
  where parseURI' = parseURI laxURIParserOptions . encodeUtf8
        parseRelativeRef' = parseRelativeRef laxURIParserOptions . encodeUtf8

asInt :: MonadThrow m => Text -> m Int
asInt t = maybe (throwM $ InvalidInt t) return . readMaybe $ unpack t

asBool :: MonadThrow m => Text -> m Bool
asBool "true" = return True
asBool "false" = return False
asBool t = throwM $ InvalidBool t

asVersion :: MonadThrow m => Text -> m Version
asVersion t = maybe (throwM $ InvalidVersion t) return . fmap fst . headMay . readP_to_S parseVersion $ unpack t

asCloudProtocol :: MonadThrow m => Text -> m CloudProtocol
asCloudProtocol "xml-rpc" = return ProtocolXmlRpc
asCloudProtocol "soap" = return ProtocolSoap
asCloudProtocol "http-post" = return ProtocolHttpPost
asCloudProtocol t = throwM $ InvalidProtocol t

unknownTag :: (MonadCatch m) => ConduitParser Event m (Endo a)
unknownTag = anyTag $ \_ _ -> many (void unknownTag <|> void textContent) >> return mempty

-- | Like 'tagName' but ignores the namespace.
tagName' :: (MonadCatch m) => Text -> AttrParser a -> (a -> ConduitParser Event m b) -> ConduitParser Event m b
tagName' t = tagPredicate (\n -> nameLocalName n == t)

-- | Tag which content is a date-time that follows RFC 3339 format.
tagDate :: (MonadCatch m) => Name -> ConduitParser Event m UTCTime
tagDate name = tagIgnoreAttrs name $ content (fmap zonedTimeToUTC . parseTimeRFC822)


lastRequired :: (Monad m, Parsing m) => String -> FoldM m a a
lastRequired e = FoldM (\_ a -> return $ Right a) (return $ Left e) (either unexpected return)
-- }}}


-- | Parse a @\<skipHours\>@ element.
rssSkipHours :: MonadCatch m => ConduitParser Event m (Set Hour)
rssSkipHours = named "Rss <skipHours> element" $ tagIgnoreAttrs "skipHours" $
  fromList <$> many (tagIgnoreAttrs "hour" $ content (asInt >=> asHour))

-- | Parse a @\<skipDays\>@ element.
rssSkipDays :: MonadCatch m => ConduitParser Event m (Set Day)
rssSkipDays = named "Rss <skipDays> element" $ tagIgnoreAttrs "skipDays" $
  fromList <$> many (tagIgnoreAttrs "day" $ content asDay)


data TextInputPiece = TextInputTitle Text | TextInputDescription Text
                    | TextInputName Text | TextInputLink RssURI
                    | TextInputUnknown

makeTraversals ''TextInputPiece

-- | Parse a @\<textInput\>@ element.
rssTextInput :: (MonadCatch m) => ConduitParser Event m RssTextInput
rssTextInput = named "Rss <textInput> element" $ tagIgnoreAttrs "textInput" $ do
  p <- many piece
  flip foldM p $ RssTextInput
    <$> handlesM _TextInputTitle (lastRequired "Missing <title> element.")
    <*> handlesM _TextInputDescription (lastRequired "Missing <description> element.")
    <*> handlesM _TextInputName (lastRequired "Missing <name> element.")
    <*> handlesM _TextInputLink (lastRequired "Missing <link> element.")
  where piece :: MonadCatch m => ConduitParser Event m TextInputPiece
        piece = choice [ TextInputTitle <$> tagIgnoreAttrs "title" textContent
                       , TextInputDescription <$> tagIgnoreAttrs "description" textContent
                       , TextInputName <$> tagIgnoreAttrs "name" textContent
                       , TextInputLink <$> tagIgnoreAttrs "link" (content asRssURI)
                       , TextInputUnknown <$ unknownTag
                       ]


data ImagePiece = ImageUri RssURI | ImageTitle Text | ImageLink RssURI | ImageWidth Int
                | ImageHeight Int | ImageDescription Text | ImageUnknown

makeTraversals ''ImagePiece

-- | Parse an @\<image\>@ element.
rssImage :: (MonadCatch m) => ConduitParser Event m RssImage
rssImage = named "Rss <image> element" $ tagIgnoreAttrs "image" $ do
  p <- many piece
  flip foldM p $ RssImage
    <$> handlesM _ImageUri (lastRequired "Missing <uri> element.")
    <*> handlesM _ImageTitle (lastRequired "Missing <title> element.")
    <*> handlesM _ImageLink (lastRequired "Missing <link> element.")
    <*> generalize (handles _ImageWidth last)
    <*> generalize (handles _ImageHeight last)
    <*> generalize (handles _ImageDescription $ lastDef "")
  where piece = choice [ ImageUri <$> tagIgnoreAttrs "uri" (content asRssURI)
                       , ImageTitle <$> tagIgnoreAttrs "title" textContent
                       , ImageLink <$> tagIgnoreAttrs "link" (content asRssURI)
                       , ImageWidth <$> tagIgnoreAttrs "width" (content asInt)
                       , ImageHeight <$> tagIgnoreAttrs "height" (content asInt)
                       , ImageDescription <$> tagIgnoreAttrs "description" textContent
                       , ImageUnknown <$ unknownTag
                       ]


-- | Parse a @\<category\>@ element.
rssCategory :: MonadCatch m => ConduitParser Event m RssCategory
rssCategory = named "Rss <category> element" $ tagName' "category" (textAttr "domain") $ \domain ->
  RssCategory domain <$> textContent

-- | Parse a @\<cloud\>@ element.
rssCloud :: (MonadCatch m) => ConduitParser Event m RssCloud
rssCloud = named "Rss <cloud> element" $ tagName' "cloud" attributes return where
  attributes = do
    uri <- fmap RssURI $ RelativeRef
      <$> fmap Just (Authority Nothing <$> (Host . encodeUtf8 <$> textAttr "domain") <*> (fmap Port <$> optional (attr "port" asInt)))
      <*> (encodeUtf8 <$> textAttr "path")
      <*> pure (Query [])
      <*> pure Nothing
    RssCloud uri <$> textAttr "registerProcedure" <*> attr "protocol" asCloudProtocol <* ignoreAttrs

-- | Parse a @\<guid\>@ element.
rssGuid :: MonadCatch m => ConduitParser Event m RssGuid
rssGuid = named "RSS <guid> element" $ tagName' "guid" attributes handler where
  attributes = optional (attr "isPermaLink" asBool) <* ignoreAttrs
  handler (Just True) = GuidUri <$> content asRssURI
  handler _ = GuidText <$> textContent

-- | Parse an @\<enclosure\>@ element.
rssEnclosure :: MonadCatch m => ConduitParser Event m RssEnclosure
rssEnclosure = named "Rss <enclosure> element" $ tagName' "enclosure" attributes handler where
  attributes = (,,) <$> attr "url" asRssURI <*> attr "length" asInt <*> textAttr "type" <* ignoreAttrs
  handler (uri, length_, type_) = return $ RssEnclosure uri length_ type_

-- | Parse a @\<source\>@ element.
rssSource :: MonadCatch m => ConduitParser Event m RssSource
rssSource = named "Rss <source> element" $ tagName' "source" attributes handler where
  attributes = attr "url" asRssURI <* ignoreAttrs
  handler uri = RssSource uri <$> textContent


data ItemPiece = ItemTitle Text | ItemLink RssURI | ItemDescription Text
               | ItemAuthor Text | ItemCategory RssCategory | ItemComments RssURI
               | ItemEnclosure RssEnclosure | ItemGuid RssGuid | ItemPubDate UTCTime
               | ItemSource RssSource | ItemUnknown

makeTraversals ''ItemPiece

-- | Parse an @\<item\>@ element.
rssItem :: MonadCatch m => ConduitParser Event m RssItem
rssItem = named "Rss <item> element" $ tagIgnoreAttrs "item" $ do
  p <- many piece
  return . flip fold p $ RssItem
    <$> handles _ItemTitle (lastDef "")
    <*> handles _ItemLink last
    <*> handles _ItemDescription (lastDef "")
    <*> handles _ItemAuthor (lastDef "")
    <*> handles _ItemCategory list
    <*> handles _ItemComments last
    <*> handles _ItemEnclosure list
    <*> handles _ItemGuid last
    <*> handles _ItemPubDate last
    <*> handles _ItemSource last
  where piece = choice [ ItemTitle <$> tagIgnoreAttrs "title" textContent
                       , ItemLink <$> tagIgnoreAttrs "link" (content asRssURI)
                       , ItemDescription <$> tagIgnoreAttrs "description" textContent
                       , ItemAuthor <$> tagIgnoreAttrs "author" textContent
                       , ItemCategory <$> rssCategory
                       , ItemComments <$> tagIgnoreAttrs "comments" (content asRssURI)
                       , ItemEnclosure <$> rssEnclosure
                       , ItemGuid <$> rssGuid
                       , ItemPubDate <$> tagDate "pubDate"
                       , ItemSource <$> rssSource
                       , ItemUnknown <$ unknownTag
                       ]


data ChannelPiece = ChannelTitle Text | ChannelLink RssURI | ChannelDescription Text
                  | ChannelItem RssItem | ChannelLanguage Text | ChannelCopyright Text
                  | ChannelManagingEditor Text | ChannelWebmaster Text | ChannelPubDate UTCTime
                  | ChannelLastBuildDate UTCTime | ChannelCategory RssCategory
                  | ChannelGenerator Text | ChannelDocs RssURI | ChannelCloud RssCloud
                  | ChannelTtl Int | ChannelImage RssImage | ChannelRating Text
                  | ChannelTextInput RssTextInput | ChannelSkipHours (Set Hour)
                  | ChannelSkipDays (Set Day) | ChannelUnknown

makeTraversals ''ChannelPiece

-- | Parse an @\<rss\>@ element.
rssDocument :: MonadCatch m => ConduitParser Event m RssDocument
rssDocument = named "RSS <rss> element" $ tagName' "rss" attributes $ \version -> tagIgnoreAttrs "channel" $ do
  p <- many piece
  flip foldM p $ RssDocument version
    <$> handlesM _ChannelTitle (lastRequired "Missing <title> element.")
    <*> handlesM _ChannelLink (lastRequired "Missing <link> element.")
    <*> handlesM _ChannelDescription (lastRequired "Missing <description> element.")
    <*> generalize (handles _ChannelItem list)
    <*> generalize (handles _ChannelLanguage $ lastDef "")
    <*> generalize (handles _ChannelCopyright $ lastDef "")
    <*> generalize (handles _ChannelManagingEditor $ lastDef "")
    <*> generalize (handles _ChannelWebmaster $ lastDef "")
    <*> generalize (handles _ChannelPubDate last)
    <*> generalize (handles _ChannelLastBuildDate last)
    <*> generalize (handles _ChannelCategory list)
    <*> generalize (handles _ChannelGenerator $ lastDef "")
    <*> generalize (handles _ChannelDocs last)
    <*> generalize (handles _ChannelCloud last)
    <*> generalize (handles _ChannelTtl last)
    <*> generalize (handles _ChannelImage last)
    <*> generalize (handles _ChannelRating $ lastDef "")
    <*> generalize (handles _ChannelTextInput last)
    <*> generalize (handles _ChannelSkipHours $ lastDef mempty)
    <*> generalize (handles _ChannelSkipDays $ lastDef mempty)
  where piece :: MonadCatch m => ConduitParser Event m ChannelPiece
        piece = choice [ ChannelTitle <$> tagIgnoreAttrs "title" textContent
                       , ChannelLink <$> tagIgnoreAttrs "link" (content asRssURI)
                       , ChannelDescription <$> tagIgnoreAttrs "description" textContent
                       , ChannelItem <$> rssItem
                       , ChannelLanguage <$> tagIgnoreAttrs "language" textContent
                       , ChannelCopyright <$> tagIgnoreAttrs "copyright" textContent
                       , ChannelManagingEditor <$> tagIgnoreAttrs "managingEditor" textContent
                       , ChannelWebmaster <$> tagIgnoreAttrs "webMaster" textContent
                       , ChannelPubDate <$> tagDate "pubDate"
                       , ChannelLastBuildDate <$> tagDate "lastBuildDate"
                       , ChannelCategory <$> rssCategory
                       , ChannelGenerator <$> tagIgnoreAttrs "generator" textContent
                       , ChannelDocs <$> tagIgnoreAttrs "docs" (content asRssURI)
                       , ChannelCloud <$> rssCloud
                       , ChannelTtl <$> tagIgnoreAttrs "ttl" (content asInt)
                       , ChannelImage <$> rssImage
                       , ChannelRating <$> tagIgnoreAttrs "rating" textContent
                       , ChannelTextInput <$> rssTextInput
                       , ChannelSkipHours <$> rssSkipHours
                       , ChannelSkipDays <$> rssSkipDays
                       , ChannelUnknown <$ unknownTag
                       ]
        attributes = attr "version" asVersion <* ignoreAttrs
