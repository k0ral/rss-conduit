{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
-- | __Syndication__ module for RSS.
-- Cf specification at <http://web.resource.org/rss/1.0/modules/syndication/>.
module Text.RSS.Extensions.Syndication
  ( -- * Types
    SyndicationModule(..)
  , RssChannelExtension(..)
  , RssItemExtension(..)
  , SyndicationInfo(..)
  , mkSyndicationInfo
  , SyndicationPeriod(..)
  , asSyndicationPeriod
    -- * Parsers
  , syndicationInfo
  , syndicationPeriod
  , syndicationFrequency
  , syndicationBase
    -- * Renderers
  , renderSyndicationInfo
  , renderSyndicationPeriod
  , renderSyndicationFrequency
  , renderSyndicationBase
    -- * Misc
  , namespacePrefix
  , namespaceURI
  ) where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                hiding (throwM)
import           Control.Applicative
import           Control.Exception.Safe as Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Maybe
import           Data.Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC2822
import           Data.Time.RFC3339
import           Data.Time.RFC822
import           Data.XML.Types
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import           Text.Read
import           Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as Render
import           URI.ByteString
-- }}}

-- {{{ Utils
tshow :: Show a => a -> Text
tshow = pack . show

asDate :: MonadThrow m => Text -> m UTCTime
asDate text = maybe (throw $ InvalidTime text) (return . zonedTimeToUTC) $
  parseTimeRFC3339 text <|> parseTimeRFC2822 text <|> parseTimeRFC822 text

asInt :: MonadThrow m => Text -> m Int
asInt t = maybe (throwM $ InvalidInt t) return . readMaybe $ unpack t

projectC :: Monad m => Traversal' a b -> ConduitT a b m ()
projectC prism = fix $ \recurse -> do
  item <- await
  case (item, item ^? (_Just . prism)) of
    (_, Just a) -> yield a >> recurse
    (Just _, _) -> recurse
    _           -> return ()
-- }}}

newtype SyndicationException = InvalidSyndicationPeriod Text deriving(Eq, Generic, Ord, Show)

instance Exception SyndicationException where
  displayException (InvalidSyndicationPeriod t) = "Invalid syndication period: " ++ unpack t

-- | XML prefix is @sy@.
namespacePrefix :: Text
namespacePrefix = "sy"

-- | XML namespace is <http://purl.org/rss/1.0/modules/syndication/>.
namespaceURI :: URIRef Absolute
namespaceURI = uri where Right uri = parseURI laxURIParserOptions "http://purl.org/rss/1.0/modules/syndication/"

syndicationName :: Text -> Name
syndicationName string = Name string (Just "http://purl.org/rss/1.0/modules/syndication/") (Just namespacePrefix)

syndicationTag :: MonadThrow m => Text -> ConduitT Event o m a -> ConduitT Event o m (Maybe a)
syndicationTag name = tagIgnoreAttrs (matching (== syndicationName name))

renderSyndicationTag :: Monad m => Text -> Text -> ConduitT () Event m ()
renderSyndicationTag name = Render.tag (syndicationName name) mempty . Render.content


data SyndicationPeriod = Hourly | Daily | Weekly | Monthly | Yearly
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

asSyndicationPeriod :: MonadThrow m => Text -> m SyndicationPeriod
asSyndicationPeriod "hourly"  = pure Hourly
asSyndicationPeriod "daily"   = pure Daily
asSyndicationPeriod "weekly"  = pure Weekly
asSyndicationPeriod "monthly" = pure Monthly
asSyndicationPeriod "yearly"  = pure Yearly
asSyndicationPeriod t         = throw $ InvalidSyndicationPeriod t

fromSyndicationPeriod :: SyndicationPeriod -> Text
fromSyndicationPeriod Hourly  = "hourly"
fromSyndicationPeriod Daily   = "daily"
fromSyndicationPeriod Weekly  = "weekly"
fromSyndicationPeriod Monthly = "monthly"
fromSyndicationPeriod Yearly  = "yearly"


-- | __Syndication__ extension model.
data SyndicationInfo = SyndicationInfo
  { updatePeriod    :: Maybe SyndicationPeriod
  , updateFrequency :: Maybe Int
  , updateBase      :: Maybe UTCTime
  } deriving (Eq, Generic, Ord, Read, Show)

-- | Construct an empty 'SyndicationInfo'.
mkSyndicationInfo :: SyndicationInfo
mkSyndicationInfo = SyndicationInfo mzero mzero mzero


data ElementPiece = ElementPeriod { __elementPeriod :: SyndicationPeriod }
                  | ElementFrequency { __elementFrequency :: Int }
                  | ElementBase { __elementBase :: UTCTime }

makeLenses ''ElementPiece

-- | Parse all __Syndication__ elements.
syndicationInfo :: MonadThrow m => ConduitT Event o m SyndicationInfo
syndicationInfo = manyYield' (choose piece) .| parser where
  parser = getZipConduit $ SyndicationInfo
    <$> ZipConduit (projectC _elementPeriod .| headC)
    <*> ZipConduit (projectC _elementFrequency .| headC)
    <*> ZipConduit (projectC _elementBase .| headC)
  piece = [ fmap ElementPeriod <$> syndicationPeriod
          , fmap ElementFrequency <$> syndicationFrequency
          , fmap ElementBase <$> syndicationBase
          ]

-- | Parse a @\<sy:updatePeriod\>@ element.
syndicationPeriod :: MonadThrow m => ConduitT Event o m (Maybe SyndicationPeriod)
syndicationPeriod = syndicationTag "updatePeriod" (content >>= asSyndicationPeriod)

-- | Parse a @\<sy:updateFrequency\>@ element.
syndicationFrequency :: MonadThrow m => ConduitT Event o m (Maybe Int)
syndicationFrequency = syndicationTag "updateFrequency" (content >>= asInt)

-- | Parse a @\<sy:updateBase\>@ element.
syndicationBase :: MonadThrow m => ConduitT Event o m (Maybe UTCTime)
syndicationBase = syndicationTag "updateBase" (content >>= asDate)

-- | Render all __Syndication__ elements.
renderSyndicationInfo :: Monad m => SyndicationInfo -> ConduitT () Event m ()
renderSyndicationInfo SyndicationInfo{..} = do
  forM_ updatePeriod renderSyndicationPeriod
  forM_ updateFrequency renderSyndicationFrequency
  forM_ updateBase renderSyndicationBase

-- | Render a @\<sy:updatePeriod\>@ element.
renderSyndicationPeriod :: Monad m => SyndicationPeriod -> ConduitT () Event m ()
renderSyndicationPeriod = renderSyndicationTag "updatePeriod" . fromSyndicationPeriod

-- | Render a @\<sy:updateFrequency\>@ element.
renderSyndicationFrequency :: Monad m => Int -> ConduitT () Event m ()
renderSyndicationFrequency = renderSyndicationTag "updateFrequency" . tshow

-- | Render a @\<sy:updateBase\>@ element.
renderSyndicationBase :: Monad m => UTCTime -> ConduitT () Event m ()
renderSyndicationBase = renderSyndicationTag "updateBase" . formatTimeRFC822 . utcToZonedTime utc


-- | __Syndication__ tag type.
newtype SyndicationModule a = SyndicationModule a

instance ParseRssExtension a => ParseRssExtension (SyndicationModule a) where
  parseRssChannelExtension = getZipConduit $ SyndicationChannel
    <$> ZipConduit syndicationInfo
    <*> ZipConduit parseRssChannelExtension
  parseRssItemExtension    = SyndicationItem <$> parseRssItemExtension

instance RenderRssExtension a => RenderRssExtension (SyndicationModule a) where
  renderRssChannelExtension SyndicationChannel{..} = do
    renderSyndicationInfo channelSyndicationInfo
    renderRssChannelExtension channelSyndicationOther
  renderRssItemExtension (SyndicationItem a) = renderRssItemExtension a


data instance RssChannelExtension (SyndicationModule a) = SyndicationChannel
  { channelSyndicationInfo  :: SyndicationInfo
  , channelSyndicationOther :: RssChannelExtension a
  }

deriving instance Eq (RssChannelExtension a) => Eq (RssChannelExtension (SyndicationModule a))
deriving instance Ord (RssChannelExtension a) => Ord (RssChannelExtension (SyndicationModule a))
deriving instance Read (RssChannelExtension a) => Read (RssChannelExtension (SyndicationModule a))
deriving instance Show (RssChannelExtension a) => Show (RssChannelExtension (SyndicationModule a))
deriving instance Generic (RssChannelExtension a) => Generic (RssChannelExtension (SyndicationModule a))

data instance RssItemExtension (SyndicationModule a) = SyndicationItem { itemSyndicationOther :: RssItemExtension a }

deriving instance Eq (RssItemExtension a) => Eq (RssItemExtension (SyndicationModule a))
deriving instance Ord (RssItemExtension a) => Ord (RssItemExtension (SyndicationModule a))
deriving instance Read (RssItemExtension a) => Read (RssItemExtension (SyndicationModule a))
deriving instance Show (RssItemExtension a) => Show (RssItemExtension (SyndicationModule a))
deriving instance Generic (RssItemExtension a) => Generic (RssItemExtension (SyndicationModule a))
