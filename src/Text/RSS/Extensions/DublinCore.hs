{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
-- | __Dublin Core__ extension for RSS.
--  Cf specification at <http://web.resource.org/rss/1.0/modules/dc/>.
module Text.RSS.Extensions.DublinCore
  ( DublinCoreModule(..)
  , RssChannelExtension(..)
  , RssItemExtension(..)
  , DcMetaData(..)
  , mkDcMetaData
  ) where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit
import           Control.Exception.Safe             as Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Maybe
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import qualified Text.XML.DublinCore.Conduit.Parse  as DC
import           Text.XML.DublinCore.Conduit.Render
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- {{{ Utils
projectC :: Monad m => Traversal' a b -> ConduitT a b m ()
projectC prism = fix $ \recurse -> do
  item <- await
  case (item, item ^? (_Just . prism)) of
    (_, Just a) -> yield a >> recurse
    (Just _, _) -> recurse
    _           -> return ()
-- }}}

-- | __Dublin Core__ extension model.
data DcMetaData = DcMetaData
  { elementContributor :: Text
  , elementCoverage    :: Text
  , elementCreator     :: Text
  , elementDate        :: Maybe UTCTime
  , elementDescription :: Text
  , elementFormat      :: Text
  , elementIdentifier  :: Text
  , elementLanguage    :: Text
  , elementPublisher   :: Text
  , elementRelation    :: Text
  , elementRights      :: Text
  , elementSource      :: Text
  , elementSubject     :: Text
  , elementTitle       :: Text
  , elementType        :: Text
  } deriving(Eq, Generic, Ord, Read, Show)

-- | Construct an empty 'DcMetaData'.
mkDcMetaData = DcMetaData mempty mempty mempty Nothing mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty


data ElementPiece = ElementContributor { __elementContributor :: Text }
                  | ElementCoverage { __elementCoverage :: Text }
                  | ElementCreator { __elementCreator :: Text }
                  | ElementDate { __elementDate :: UTCTime }
                  | ElementDescription { __elementDescription :: Text }
                  | ElementFormat { __elementFormat :: Text }
                  | ElementIdentifier { __elementIdentifier :: Text }
                  | ElementLanguage { __elementLanguage :: Text }
                  | ElementPublisher { __elementPublisher :: Text }
                  | ElementRelation { __elementRelation :: Text }
                  | ElementRights { __elementRights :: Text }
                  | ElementSource { __elementSource :: Text }
                  | ElementSubject { __elementSubject :: Text }
                  | ElementTitle { __elementTitle :: Text }
                  | ElementType { __elementType :: Text }

makeLenses ''ElementPiece

-- | Parse a set of Dublin Core metadata elements.
dcMetadata :: MonadThrow m => ConduitT Event o m DcMetaData
dcMetadata = manyYield' (choose piece) .| parser where
  parser = getZipConduit $ DcMetaData
    <$> ZipConduit (projectC _elementContributor .| headDefC "")
    <*> ZipConduit (projectC _elementCoverage .| headDefC "")
    <*> ZipConduit (projectC _elementCreator .| headDefC "")
    <*> ZipConduit (projectC _elementDate .| headC)
    <*> ZipConduit (projectC _elementDescription .| headDefC "")
    <*> ZipConduit (projectC _elementFormat .| headDefC "")
    <*> ZipConduit (projectC _elementIdentifier .| headDefC "")
    <*> ZipConduit (projectC _elementLanguage .| headDefC "")
    <*> ZipConduit (projectC _elementPublisher .| headDefC "")
    <*> ZipConduit (projectC _elementRelation .| headDefC "")
    <*> ZipConduit (projectC _elementRights .| headDefC "")
    <*> ZipConduit (projectC _elementSource .| headDefC "")
    <*> ZipConduit (projectC _elementSubject .| headDefC "")
    <*> ZipConduit (projectC _elementTitle .| headDefC "")
    <*> ZipConduit (projectC _elementType .| headDefC "")
  piece = [ fmap ElementContributor <$> DC.elementContributor
          , fmap ElementCoverage <$> DC.elementCoverage
          , fmap ElementCreator <$> DC.elementCreator
          , fmap ElementDate <$> DC.elementDate
          , fmap ElementDescription <$> DC.elementDescription
          , fmap ElementFormat <$> DC.elementFormat
          , fmap ElementIdentifier <$> DC.elementIdentifier
          , fmap ElementLanguage <$> DC.elementLanguage
          , fmap ElementPublisher <$> DC.elementPublisher
          , fmap ElementRelation <$> DC.elementRelation
          , fmap ElementRights <$> DC.elementRights
          , fmap ElementSource <$> DC.elementSource
          , fmap ElementSubject <$> DC.elementSubject
          , fmap ElementTitle <$> DC.elementTitle
          , fmap ElementType <$> DC.elementType
          ]

-- | Render a set of Dublin Core metadata elements.
renderDcMetadata :: Monad m => DcMetaData -> ConduitT () Event m ()
renderDcMetadata DcMetaData{..} = do
  unless (Text.null elementContributor) $ renderElementContributor elementContributor
  unless (Text.null elementCoverage) $ renderElementCoverage elementCoverage
  unless (Text.null elementCreator) $ renderElementCreator elementCreator
  forM_ elementDate renderElementDate
  unless (Text.null elementDescription) $ renderElementDescription elementDescription
  unless (Text.null elementFormat) $ renderElementFormat elementFormat
  unless (Text.null elementIdentifier) $ renderElementIdentifier elementIdentifier
  unless (Text.null elementLanguage) $ renderElementLanguage elementLanguage
  unless (Text.null elementPublisher) $ renderElementPublisher elementPublisher
  unless (Text.null elementRelation) $ renderElementRelation elementRelation
  unless (Text.null elementRights) $ renderElementRights elementRights
  unless (Text.null elementSource) $ renderElementSource elementSource
  unless (Text.null elementSubject) $ renderElementSubject elementSubject
  unless (Text.null elementTitle) $ renderElementTitle elementTitle
  unless (Text.null elementType) $ renderElementType elementType


-- | __Dublin Core__ tag type.
newtype DublinCoreModule a = DublinCoreModule a

instance ParseRssExtension a => ParseRssExtension (DublinCoreModule a) where
  parseRssChannelExtension = getZipConduit $ DublinCoreChannel
    <$> ZipConduit dcMetadata
    <*> ZipConduit parseRssChannelExtension
  parseRssItemExtension    = getZipConduit $ DublinCoreItem
    <$> ZipConduit dcMetadata
    <*> ZipConduit parseRssItemExtension

instance RenderRssExtension a => RenderRssExtension (DublinCoreModule a) where
  renderRssChannelExtension DublinCoreChannel{..} = do
    renderDcMetadata channelDcMetaData
    renderRssChannelExtension channelDcOther
  renderRssItemExtension DublinCoreItem{..}   = do
    renderDcMetadata itemDcMetaData
    renderRssItemExtension itemDcOther


data instance RssChannelExtension (DublinCoreModule a) = DublinCoreChannel
  { channelDcMetaData :: DcMetaData
  , channelDcOther    :: RssChannelExtension a
  }

deriving instance Eq (RssChannelExtension a) => Eq (RssChannelExtension (DublinCoreModule a))
deriving instance Ord (RssChannelExtension a) => Ord (RssChannelExtension (DublinCoreModule a))
deriving instance Read (RssChannelExtension a) => Read (RssChannelExtension (DublinCoreModule a))
deriving instance Show (RssChannelExtension a) => Show (RssChannelExtension (DublinCoreModule a))
deriving instance Generic (RssChannelExtension a) => Generic (RssChannelExtension (DublinCoreModule a))

data instance RssItemExtension (DublinCoreModule a) = DublinCoreItem
  { itemDcMetaData :: DcMetaData
  , itemDcOther    :: RssItemExtension a
  }

deriving instance Eq (RssItemExtension a) => Eq (RssItemExtension (DublinCoreModule a))
deriving instance Ord (RssItemExtension a) => Ord (RssItemExtension (DublinCoreModule a))
deriving instance Read (RssItemExtension a) => Read (RssItemExtension (DublinCoreModule a))
deriving instance Show (RssItemExtension a) => Show (RssItemExtension (DublinCoreModule a))
deriving instance Generic (RssItemExtension a) => Generic (RssItemExtension (DublinCoreModule a))
