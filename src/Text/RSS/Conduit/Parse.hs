{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
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
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                      hiding (throwM)
import           Control.Applicative          hiding (many)
import           Control.Exception.Safe       as Exception
import           Control.Monad                (void)
import           Control.Monad.Fix
import           Data.Conduit
import           Data.List.NonEmpty           (NonEmpty (..), nonEmpty)
import           Data.Maybe
import           Data.Monoid
import           Data.Set                     (Set, fromList)
import           Data.Text                    as Text
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.Time.RFC822
import           Data.Version
import           Data.XML.Types
import           Lens.Micro
import           Lens.Micro.TH
import           Safe
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read                    (readMaybe)
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- {{{ Util
asRssURI :: MonadThrow m => Text -> m RssURI
asRssURI t = case (parseURI' t, parseRelativeRef' t) of
  (Right u, _) -> return $ RssURI u
  (_, Right u) -> return $ RssURI u
  (_, Left e)  -> throwM $ InvalidURI e
  where parseURI' = parseURI laxURIParserOptions . encodeUtf8
        parseRelativeRef' = parseRelativeRef laxURIParserOptions . encodeUtf8

nullURI :: RssURI
nullURI = RssURI $ RelativeRef Nothing "" (Query []) Nothing

asInt :: MonadThrow m => Text -> m Int
asInt t = maybe (throwM $ InvalidInt t) return . readMaybe $ unpack t

asBool :: MonadThrow m => Text -> m Bool
asBool "true"  = return True
asBool "false" = return False
asBool t       = throwM $ InvalidBool t

asVersion :: MonadThrow m => Text -> m Version
asVersion t = maybe (throwM $ InvalidVersion t) return . fmap fst . headMay . readP_to_S parseVersion $ unpack t

asCloudProtocol :: MonadThrow m => Text -> m CloudProtocol
asCloudProtocol "xml-rpc"   = return ProtocolXmlRpc
asCloudProtocol "soap"      = return ProtocolSoap
asCloudProtocol "http-post" = return ProtocolHttpPost
asCloudProtocol t           = throwM $ InvalidProtocol t

-- | Like 'tagName' but ignores the namespace.
tagName' :: MonadThrow m => Text -> AttrParser a -> (a -> ConduitM Event o m b) -> ConduitM Event o m (Maybe b)
tagName' t = tag' (matching $ \n -> nameLocalName n == t && isNothing (nameNamespace n))

-- | Tag which content is a date-time that follows RFC 3339 format.
tagDate :: (MonadThrow m) => NameMatcher a -> ConduitM Event o m (Maybe UTCTime)
tagDate name = tagIgnoreAttrs name $ fmap zonedTimeToUTC $ do
  text <- content
  maybe (throw $ InvalidTime text) return $ parseTimeRFC822 text <|> parseTimeRFC3339 text

headRequiredC :: MonadThrow m => Text -> ConduitT a b m a
headRequiredC e = maybe (throw $ MissingElement e) return =<< headC

projectC :: Monad m => Traversal' a b -> ConduitT a b m ()
projectC prism = fix $ \recurse -> do
  item <- await
  case (item, item ^? (_Just . prism)) of
    (_, Just a) -> yield a >> recurse
    (Just _, _) -> recurse
    _           -> return ()
-- }}}


-- | Parse a @\<skipHours\>@ element.
rssSkipHours :: MonadThrow m => ConduitM Event o m (Maybe (Set Hour))
rssSkipHours = tagIgnoreAttrs "skipHours" $
  fromList <$> (manyYield' (tagIgnoreAttrs "hour" $ content >>= asInt >>= asHour) .| sinkList)

-- | Parse a @\<skipDays\>@ element.
rssSkipDays :: MonadThrow m => ConduitM Event o m (Maybe (Set Day))
rssSkipDays = tagIgnoreAttrs "skipDays" $
  fromList <$> (manyYield' (tagIgnoreAttrs "day" $ content >>= asDay) .| sinkList)


data TextInputPiece = TextInputTitle { __textInputTitle :: Text }
                    | TextInputDescription { __textInputDescription :: Text }
                    | TextInputName { __textInputName :: Text }
                    | TextInputLink { __textInputLink :: RssURI }

makeLenses ''TextInputPiece

-- | Parse a @\<textInput\>@ element.
rssTextInput :: MonadThrow m => ConduitM Event o m (Maybe RssTextInput)
rssTextInput = tagIgnoreAttrs "textInput" $ (manyYield' (choose piece) .| parser) <* many ignoreAnyTreeContent where
  parser = getZipConduit $ RssTextInput
    <$> ZipConduit (projectC _textInputTitle .| headRequiredC "Missing <title> element")
    <*> ZipConduit (projectC _textInputDescription .| headRequiredC "Missing <description> element")
    <*> ZipConduit (projectC _textInputName .| headRequiredC "Missing <name> element")
    <*> ZipConduit (projectC _textInputLink .| headRequiredC "Missing <link> element")
  piece = [ fmap TextInputTitle <$> tagIgnoreAttrs "title" content
          , fmap TextInputDescription <$> tagIgnoreAttrs "description" content
          , fmap TextInputName <$> tagIgnoreAttrs "name" content
          , fmap TextInputLink <$> tagIgnoreAttrs "link" (content >>= asRssURI)
          ]



data ImagePiece = ImageUri { __imageUri :: RssURI }
                | ImageTitle { __imageTitle :: Text }
                | ImageLink { __imageLink :: RssURI }
                | ImageWidth { __imageWidth :: Int }
                | ImageHeight { __imageHeight :: Int }
                | ImageDescription { __imageDescription :: Text }

makeLenses ''ImagePiece

-- | Parse an @\<image\>@ element.
rssImage :: (MonadThrow m) => ConduitM Event o m (Maybe RssImage)
rssImage = tagIgnoreAttrs "image" $ (manyYield' (choose piece) .| parser) <* many ignoreAnyTreeContent where
  parser = getZipConduit $ RssImage
    <$> ZipConduit (projectC _imageUri .| headRequiredC "Missing <url> element")
    <*> ZipConduit (projectC _imageTitle .| headDefC "Unnamed image")  -- Lenient
    <*> ZipConduit (projectC _imageLink .| headDefC nullURI)  -- Lenient
    <*> ZipConduit (projectC _imageWidth .| headC)
    <*> ZipConduit (projectC _imageHeight .| headC)
    <*> ZipConduit (projectC _imageDescription .| headDefC "")
  piece = [ fmap ImageUri <$> tagIgnoreAttrs "url" (content >>= asRssURI)
          , fmap ImageTitle <$> tagIgnoreAttrs "title" content
          , fmap ImageLink <$> tagIgnoreAttrs "link" (content >>= asRssURI)
          , fmap ImageWidth <$> tagIgnoreAttrs "width" (content >>= asInt)
          , fmap ImageHeight <$> tagIgnoreAttrs "height" (content >>= asInt)
          , fmap ImageDescription <$> tagIgnoreAttrs "description" content
          ]

-- | Parse a @\<category\>@ element.
rssCategory :: MonadThrow m => ConduitM Event o m (Maybe RssCategory)
rssCategory = tagName' "category" (optional (requireAttr "domain") <* ignoreAttrs) $ \domain ->
  RssCategory (fromMaybe "" domain) <$> content

-- | Parse a @\<cloud\>@ element.
rssCloud :: (MonadThrow m) => ConduitM Event o m (Maybe RssCloud)
rssCloud = tagName' "cloud" attributes return where
  attributes = do
    uri <- fmap RssURI $ RelativeRef
      <$> fmap Just (Authority Nothing <$> (Host . encodeUtf8 <$> requireAttr "domain") <*> (fmap Port <$> optional (requireAttr "port" >>= asInt)))
      <*> (encodeUtf8 <$> requireAttr "path")
      <*> pure (Query [])
      <*> pure Nothing
    RssCloud uri <$> requireAttr "registerProcedure" <*> (requireAttr "protocol" >>= asCloudProtocol) <* ignoreAttrs

-- | Parse a @\<guid\>@ element.
rssGuid :: MonadThrow m => ConduitM Event o m (Maybe RssGuid)
rssGuid = tagName' "guid" attributes handler where
  attributes = optional (requireAttr "isPermaLink" >>= asBool) <* ignoreAttrs
  handler (Just True) = GuidUri <$> (content >>= asRssURI)
  handler _           = GuidText <$> content

-- | Parse an @\<enclosure\>@ element.
rssEnclosure :: MonadThrow m => ConduitM Event o m (Maybe RssEnclosure)
rssEnclosure = tagName' "enclosure" attributes handler where
  attributes = (,,) <$> (requireAttr "url" >>= asRssURI) <*> (requireAttr "length" >>= asInt) <*> requireAttr "type" <* ignoreAttrs
  handler (uri, length_, type_) = return $ RssEnclosure uri length_ type_

-- | Parse a @\<source\>@ element.
rssSource :: MonadThrow m => ConduitM Event o m (Maybe RssSource)
rssSource = tagName' "source" attributes handler where
  attributes = (requireAttr "url" >>= asRssURI) <* ignoreAttrs
  handler uri = RssSource uri <$> content


data ItemPiece = ItemTitle { __itemTitle :: Text }
               | ItemLink { __itemLink :: RssURI }
               | ItemDescription { __itemDescription :: Text }
               | ItemAuthor { __itemAuthor :: Text }
               | ItemCategory { __itemCategory :: RssCategory }
               | ItemComments { __itemComments :: RssURI }
               | ItemEnclosure { __itemEnclosure :: RssEnclosure }
               | ItemGuid { __itemGuid :: RssGuid }
               | ItemPubDate { __itemPubDate :: UTCTime }
               | ItemSource { __itemSource :: RssSource }
               | ItemOther { __itemOther :: NonEmpty Event }

makeLenses ''ItemPiece

-- | Parse an @\<item\>@ element.
--
-- RSS extensions are automatically parsed based on the inferred result type.
rssItem :: ParseRssExtension e => MonadThrow m => ConduitM Event o m (Maybe (RssItem e))
rssItem = tagIgnoreAttrs "item" $ (manyYield' (choose piece) .| parser) <* many ignoreAnyTreeContent where
  parser = getZipConduit $ RssItem
    <$> ZipConduit (projectC _itemTitle .| headDefC "")
    <*> ZipConduit (projectC _itemLink .| headC)
    <*> ZipConduit (projectC _itemDescription .| headDefC "")
    <*> ZipConduit (projectC _itemAuthor .| headDefC "")
    <*> ZipConduit (projectC _itemCategory .| sinkList)
    <*> ZipConduit (projectC _itemComments .| headC)
    <*> ZipConduit (projectC _itemEnclosure .| sinkList)
    <*> ZipConduit (projectC _itemGuid .| headC)
    <*> ZipConduit (projectC _itemPubDate .| headC)
    <*> ZipConduit (projectC _itemSource .| headC)
    <*> ZipConduit (projectC _itemOther .| concatC .| parseRssItemExtension)
  piece = [ fmap ItemTitle <$> tagIgnoreAttrs "title" content
          , fmap ItemLink <$> tagIgnoreAttrs "link" (content >>= asRssURI)
          , fmap ItemDescription <$> tagIgnoreAttrs "description" content
          , fmap ItemAuthor <$> tagIgnoreAttrs "author" content
          , fmap ItemCategory <$> rssCategory
          , fmap ItemComments <$> tagIgnoreAttrs "comments" (content >>= asRssURI)
          , fmap ItemEnclosure <$> rssEnclosure
          , fmap ItemGuid <$> rssGuid
          , fmap ItemPubDate <$> tagDate "pubDate"
          , fmap ItemSource <$> rssSource
          , fmap ItemOther . nonEmpty <$> (void takeAnyTreeContent .| sinkList)
          ]


data ChannelPiece e = ChannelTitle { __channelTitle :: Text }
                    | ChannelLink { __channelLink :: RssURI }
                    | ChannelDescription { __channelDescription :: Text }
                    | ChannelItem { __channelItem :: RssItem e }
                    | ChannelLanguage { __channelLanguage :: Text }
                    | ChannelCopyright { __channelCopyright :: Text }
                    | ChannelManagingEditor { __channelManagingEditor :: Text }
                    | ChannelWebmaster { __channelWebmaster :: Text }
                    | ChannelPubDate { __channelPubDate :: UTCTime }
                    | ChannelLastBuildDate { __channelLastBuildDate :: UTCTime }
                    | ChannelCategory { __channelCategory :: RssCategory }
                    | ChannelGenerator { __channelGenerator :: Text }
                    | ChannelDocs { __channelDocs :: RssURI }
                    | ChannelCloud { __channelCloud :: RssCloud }
                    | ChannelTtl { __channelTtl :: Int }
                    | ChannelImage { __channelImage :: RssImage }
                    | ChannelRating { __channelRating :: Text }
                    | ChannelTextInput { __channelTextInput :: RssTextInput }
                    | ChannelSkipHours { __channelSkipHours :: Set Hour }
                    | ChannelSkipDays { __channelSkipDays :: Set Day }
                    | ChannelOther { __channelOther :: NonEmpty Event }

makeLenses ''ChannelPiece

-- | Parse an @\<rss\>@ element.
--
-- RSS extensions are automatically parsed based on the inferred result type.
rssDocument :: ParseRssExtension e => MonadThrow m => ConduitM Event o m (Maybe (RssDocument e))
rssDocument = tagName' "rss" attributes $ \version -> force "Missing <channel>" $ tagIgnoreAttrs "channel" (manyYield' (choose piece) .| parser version) <* many ignoreAnyTreeContent where
  parser version = getZipConduit $ RssDocument version
    <$> ZipConduit (projectC _channelTitle .| headRequiredC "Missing <title> element")
    <*> ZipConduit (projectC _channelLink .| headRequiredC "Missing <link> element")
    <*> ZipConduit (projectC _channelDescription .| headDefC "")  -- Lenient
    <*> ZipConduit (projectC _channelItem .| sinkList)
    <*> ZipConduit (projectC _channelLanguage .| headDefC "")
    <*> ZipConduit (projectC _channelCopyright .| headDefC "")
    <*> ZipConduit (projectC _channelManagingEditor .| headDefC "")
    <*> ZipConduit (projectC _channelWebmaster .| headDefC "")
    <*> ZipConduit (projectC _channelPubDate .| headC)
    <*> ZipConduit (projectC _channelLastBuildDate .| headC)
    <*> ZipConduit (projectC _channelCategory .| sinkList)
    <*> ZipConduit (projectC _channelGenerator .| headDefC "")
    <*> ZipConduit (projectC _channelDocs .| headC)
    <*> ZipConduit (projectC _channelCloud .| headC)
    <*> ZipConduit (projectC _channelTtl .| headC)
    <*> ZipConduit (projectC _channelImage .| headC)
    <*> ZipConduit (projectC _channelRating .| headDefC "")
    <*> ZipConduit (projectC _channelTextInput .| headC)
    <*> ZipConduit (projectC _channelSkipHours .| headDefC mempty)
    <*> ZipConduit (projectC _channelSkipDays .| headDefC mempty)
    <*> ZipConduit (projectC _channelOther .| concatC .| parseRssChannelExtension)
  piece = [ fmap ChannelTitle <$> tagIgnoreAttrs "title" content
          , fmap ChannelLink <$> tagIgnoreAttrs "link" (content >>= asRssURI)
          , fmap ChannelDescription <$> tagIgnoreAttrs "description" content
          , fmap ChannelItem <$> rssItem
          , fmap ChannelLanguage <$> tagIgnoreAttrs "language" content
          , fmap ChannelCopyright <$> tagIgnoreAttrs "copyright" content
          , fmap ChannelManagingEditor <$> tagIgnoreAttrs "managingEditor" content
          , fmap ChannelWebmaster <$> tagIgnoreAttrs "webMaster" content
          , fmap ChannelPubDate <$> tagDate "pubDate"
          , fmap ChannelLastBuildDate <$> tagDate "lastBuildDate"
          , fmap ChannelCategory <$> rssCategory
          , fmap ChannelGenerator <$> tagIgnoreAttrs "generator" content
          , fmap ChannelDocs <$> tagIgnoreAttrs "docs" (content >>= asRssURI)
          , fmap ChannelCloud <$> rssCloud
          , fmap ChannelTtl <$> tagIgnoreAttrs "ttl" (content >>= asInt)
          , fmap ChannelImage <$> rssImage
          , fmap ChannelRating <$> tagIgnoreAttrs "rating" content
          , fmap ChannelTextInput <$> rssTextInput
          , fmap ChannelSkipHours <$> rssSkipHours
          , fmap ChannelSkipDays <$> rssSkipDays
          , fmap ChannelOther . nonEmpty <$> (void takeAnyTreeContent .| sinkList)
          ]
  attributes = (requireAttr "version" >>= asVersion) <* ignoreAttrs
