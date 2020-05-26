{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Streaming parsers for the RSS 1.0 standard.
module Text.RSS1.Conduit.Parse
  ( -- * Top-level
    rss1Document
    -- * Elements
  , rss1ChannelItems
  , rss1Image
  , rss1Item
  , rss1TextInput
  ) where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                hiding (throwM)
import           Control.Exception.Safe as Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Conduit
import           Data.List.NonEmpty
import           Data.Text              as Text
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.Version
import           Data.XML.Types
import           Lens.Micro
import           Lens.Micro.TH
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- {{{ Util
asDate :: (MonadThrow m) => Text -> m UTCTime
asDate text = maybe (throw $ InvalidTime text) (return . zonedTimeToUTC) $ parseTimeRFC3339 text

asRssURI :: (MonadThrow m) => Text -> m RssURI
asRssURI t = case (parseURI' t, parseRelativeRef' t) of
  (Right u, _) -> return $ RssURI u
  (_, Right u) -> return $ RssURI u
  (_, Left e)  -> throwM $ InvalidURI e
  where parseURI' = parseURI laxURIParserOptions . encodeUtf8
        parseRelativeRef' = parseRelativeRef laxURIParserOptions . encodeUtf8

nullURI :: RssURI
nullURI = RssURI $ RelativeRef Nothing "" (Query []) Nothing

headRequiredC :: MonadThrow m => Text -> ConduitT a b m a
headRequiredC e = maybe (throw $ MissingElement e) return =<< headC

projectC :: Monad m => Traversal' a b -> ConduitT a b m ()
projectC prism = fix $ \recurse -> do
  item <- await
  case (item, item ^? (_Just . prism)) of
    (_, Just a) -> yield a >> recurse
    (Just _, _) -> recurse
    _           -> return ()


contentTag :: MonadThrow m => Text -> AttrParser a -> (a -> ConduitM Event o m b) -> ConduitM Event o m (Maybe b)
contentTag string = tag' (matching (== contentName string))

dcTag :: MonadThrow m => Text -> AttrParser a -> (a -> ConduitM Event o m b) -> ConduitM Event o m (Maybe b)
dcTag string = tag' (matching (== dcName string))

rdfTag :: MonadThrow m => Text -> AttrParser a -> (a -> ConduitM Event o m b) -> ConduitM Event o m (Maybe b)
rdfTag string = tag' (matching (== rdfName string))

rss1Tag :: MonadThrow m => Text -> AttrParser a -> (a -> ConduitM Event o m b) -> ConduitM Event o m (Maybe b)
rss1Tag string = tag' (matching (== rss1Name string))

contentName :: Text -> Name
contentName string = Name string (Just "http://purl.org/rss/1.0/modules/content/") (Just "content")

dcName :: Text -> Name
dcName string = Name string (Just "http://purl.org/dc/elements/1.1/") (Just "dc")

rdfName :: Text -> Name
rdfName string = Name string (Just "http://www.w3.org/1999/02/22-rdf-syntax-ns#") (Just "rdf")

rss1Name :: Text -> Name
rss1Name string = Name string (Just "http://purl.org/rss/1.0/") Nothing
-- }}}


data TextInputPiece = TextInputTitle { __textInputTitle :: Text }
                    | TextInputDescription { __textInputDescription :: Text }
                    | TextInputName { __textInputName :: Text }
                    | TextInputLink { __textInputLink :: RssURI }

makeLenses ''TextInputPiece

-- | Parse a @\<textinput\>@ element.
rss1TextInput :: MonadThrow m => ConduitM Event o m (Maybe RssTextInput)
rss1TextInput = rss1Tag "textinput" attributes $ \uri -> (manyYield' (choose piece) .| parser uri) <* many ignoreAnyTreeContent where
  parser uri = getZipConduit $ RssTextInput
    <$> ZipConduit (projectC _textInputTitle .| headRequiredC "Missing <title> element")
    <*> ZipConduit (projectC _textInputDescription .| headRequiredC "Missing <description> element")
    <*> ZipConduit (projectC _textInputName .| headRequiredC "Missing <name> element")
    <*> ZipConduit (projectC _textInputLink .| headDefC uri)  -- Lenient
  piece = [ fmap TextInputTitle <$> rss1Tag "title" ignoreAttrs (const content)
          , fmap TextInputDescription <$> rss1Tag "description" ignoreAttrs (const content)
          , fmap TextInputName <$> rss1Tag "name" ignoreAttrs (const content)
          , fmap TextInputLink <$> rss1Tag "link" ignoreAttrs (const $ content >>= asRssURI)
          ]
  attributes = (requireAttr (rdfName "about") >>= asRssURI) <* ignoreAttrs


data ItemPiece = ItemTitle { __itemTitle :: Text }
               | ItemLink { __itemLink :: RssURI }
               | ItemDescription { __itemDescription :: Text }
               | ItemCreator { __itemCreator :: Text }
               | ItemDate { __itemDate :: UTCTime }
               | ItemContent { __itemContent :: Text }
               | ItemOther { __itemOther :: NonEmpty Event }

makeLenses ''ItemPiece

-- | Parse an @\<item\>@ element.
--
-- RSS extensions are automatically parsed based on the inferred result type.
rss1Item :: ParseRssExtension e => MonadCatch m => ConduitM Event o m (Maybe (RssItem e))
rss1Item = rss1Tag "item" attributes $ \uri -> (manyYield' (choose piece) .| parser uri) <* many ignoreAnyTreeContent where
  parser uri = getZipConduit $ RssItem
    <$> ZipConduit (projectC _itemTitle .| headDefC mempty)
    <*> (Just <$> ZipConduit (projectC _itemLink .| headDefC uri))
    <*> ZipConduit (projectC _itemDescription .| headDefC mempty)
    <*> ZipConduit (projectC _itemCreator .| headDefC mempty)
    <*> pure mempty
    <*> pure mzero
    <*> pure mempty
    <*> pure mzero
    <*> ZipConduit (projectC _itemDate .| headC)
    <*> pure mzero
    <*> ZipConduit (projectC _itemOther .| concatC .| parseRssItemExtension)
  piece = [ fmap ItemTitle <$> rss1Tag "title" ignoreAttrs (const content)
          , fmap ItemLink <$> rss1Tag "link" ignoreAttrs (const $ content >>= asRssURI)
          , fmap ItemDescription <$> (rss1Tag "description" ignoreAttrs (const content) `orE` contentTag "encoded" ignoreAttrs (const content))
          , fmap ItemCreator <$> dcTag "creator" ignoreAttrs (const content)
          , fmap ItemDate <$> dcTag "date" ignoreAttrs (const $ content >>= asDate)
          , fmap ItemOther . nonEmpty <$> (void takeAnyTreeContent .| sinkList)
          ]
  attributes = (requireAttr (rdfName "about") >>= asRssURI) <* ignoreAttrs


data ImagePiece = ImageUri { __imageUri :: RssURI }
  | ImageTitle { __imageTitle :: Text }
  | ImageLink { __imageLink :: RssURI }

makeLenses ''ImagePiece

-- | Parse an @\<image\>@ element.
rss1Image :: (MonadThrow m) => ConduitM Event o m (Maybe RssImage)
rss1Image = rss1Tag "image" attributes $ \uri -> (manyYield' (choose piece) .| parser uri) <* many ignoreAnyTreeContent where
  parser uri = getZipConduit $ RssImage
    <$> ZipConduit (projectC _imageUri .| headDefC uri)  -- Lenient
    <*> ZipConduit (projectC _imageTitle .| headDefC "Unnamed image")  -- Lenient
    <*> ZipConduit (projectC _imageLink .| headDefC nullURI)  -- Lenient
    <*> pure mzero
    <*> pure mzero
    <*> pure mempty
  piece = [ fmap ImageUri <$> rss1Tag "url" ignoreAttrs (const $ content >>= asRssURI)
          , fmap ImageTitle <$> rss1Tag "title" ignoreAttrs (const content)
          , fmap ImageLink <$> rss1Tag "link" ignoreAttrs (const $ content >>= asRssURI)
          ]
  attributes = (requireAttr (rdfName "about") >>= asRssURI) <* ignoreAttrs


-- | Parse an @\<items\>@ element.
rss1ChannelItems :: MonadThrow m => ConduitM Event o m (Maybe [Text])
rss1ChannelItems = fmap join $ rss1Tag "items" ignoreAttrs $ const $ rdfTag "Seq" ignoreAttrs $ const $ many $ rdfTag "li" attributes return where
  attributes = requireAttr (rdfName "resource") <* ignoreAttrs


data Rss1Channel extensions = Rss1Channel
  { channelId'          :: RssURI
  , channelTitle'       :: Text
  , channelLink'        :: RssURI
  , channelDescription' :: Text
  , channelItems'       :: [Text]
  , channelImage'       :: Maybe RssImage
  , channelTextInput'   :: Maybe RssURI
  , channelExtensions'  :: RssChannelExtension extensions
  }

data ChannelPiece = ChannelTitle { __channelTitle :: Text }
  | ChannelLink { __channelLink :: RssURI }
  | ChannelDescription { __channelDescription :: Text }
  | ChannelImage { __channelImage :: RssImage }
  | ChannelItems { __channelItems :: [Text] }
  | ChannelTextInput { __channelTextInput :: RssURI }
  | ChannelOther { __channelOther :: NonEmpty Event }

makeLenses ''ChannelPiece


-- | Parse a @\<channel\>@ element.
--
-- RSS extensions are automatically parsed based on the inferred result type.
rss1Channel :: ParseRssExtension e => MonadThrow m => ConduitM Event o m (Maybe (Rss1Channel e))
rss1Channel = rss1Tag "channel" attributes $ \channelId -> (manyYield' (choose piece) .| parser channelId) <* many ignoreAnyTreeContent where
  parser channelId = getZipConduit $ Rss1Channel channelId
    <$> ZipConduit (projectC _channelTitle .| headRequiredC "Missing <title> element")
    <*> ZipConduit (projectC _channelLink .| headRequiredC "Missing <link> element")
    <*> ZipConduit (projectC _channelDescription .| headDefC "")  -- Lenient
    <*> ZipConduit (projectC _channelItems .| concatC .| sinkList)
    <*> ZipConduit (projectC _channelImage .| headC)
    <*> ZipConduit (projectC _channelTextInput .| headC)
    <*> ZipConduit (projectC _channelOther .| concatC .| parseRssChannelExtension)
  piece = [ fmap ChannelTitle <$> rss1Tag "title" ignoreAttrs (const content)
          , fmap ChannelLink <$> rss1Tag "link" ignoreAttrs (const $ content >>= asRssURI)
          , fmap ChannelDescription <$> rss1Tag "description" ignoreAttrs (const content)
          , fmap ChannelItems <$> rss1ChannelItems
          , fmap ChannelImage <$> rss1Image
          , fmap ChannelTextInput <$> rss1Tag "textinput" (requireAttr (rdfName "resource") >>= asRssURI) return
          , fmap ChannelOther . nonEmpty <$> (void takeAnyTreeContent .| sinkList)
          ]
  attributes = (requireAttr (rdfName "about") >>= asRssURI) <* ignoreAttrs


data Rss1Document e = Rss1Document (Rss1Channel e) (Maybe RssImage) [RssItem e] (Maybe RssTextInput)

rss1ToRss2 :: Rss1Document e -> RssDocument e
rss1ToRss2 (Rss1Document channel image items textInput) = RssDocument
  (Version [1] [])
  (channelTitle' channel)
  (channelLink' channel)
  (channelDescription' channel)
  items
  mempty
  mempty
  mempty
  mempty
  mzero
  mzero
  mzero
  mempty
  mzero
  mzero
  mzero
  image
  mempty
  textInput
  mempty
  mempty
  (channelExtensions' channel)

data DocumentPiece e = DocumentChannel { __documentChannel :: Rss1Channel e }
  | DocumentImage { __documentImage :: RssImage }
  | DocumentItem { __documentItem :: RssItem e }
  | DocumentTextInput { __documentTextInput :: RssTextInput }

makeLenses ''DocumentPiece


-- | Parse an @\<RDF\>@ element.
--
-- RSS extensions are automatically parsed based on the inferred result type.
rss1Document :: ParseRssExtension e => MonadCatch m => ConduitM Event o m (Maybe (RssDocument e))
rss1Document = fmap (fmap rss1ToRss2) $ rdfTag "RDF" ignoreAttrs $ const $ (manyYield' (choose piece) .| parser) <* many ignoreAnyTreeContent where
  parser = getZipConduit $ Rss1Document
    <$> ZipConduit (projectC _documentChannel .| headRequiredC "Missing <channel> element")
    <*> ZipConduit (projectC _documentImage .| headC)
    <*> ZipConduit (projectC _documentItem .| sinkList)
    <*> ZipConduit (projectC _documentTextInput .| headC)
  piece = [ fmap DocumentChannel <$> rss1Channel
          , fmap DocumentImage <$> rss1Image
          , fmap DocumentItem <$> rss1Item
          , fmap DocumentTextInput <$> rss1TextInput
          ]
