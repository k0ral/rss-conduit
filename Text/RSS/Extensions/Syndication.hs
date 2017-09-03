{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
-- | __Syndication__ module for RSS.
-- Cf specification at <http://web.resource.org/rss/1.0/modules/syndication/>.
module Text.RSS.Extensions.Syndication
  ( -- * Types
    SyndicationModule(..)
  , RssChannelExtension(SyndicationChannel)
  , RssItemExtension(SyndicationItem)
  , SyndicationInfo(..)
  , mkSyndicationInfo
  , SyndicationPeriod(..)
  , asSyndicationPeriod
    -- * Parsers
  , syndicationPeriod
  , syndicationFrequency
  , syndicationBase
    -- * Misc
  , namespacePrefix
  , namespaceURI
  ) where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                           hiding (throwM)
import           Control.Applicative
import           Control.Exception.Safe            as Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Maybe
import           Data.Singletons
import           Data.Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC2822
import           Data.Time.RFC3339
import           Data.Time.RFC822
import           Data.XML.Types
import           GHC.Generics
import           Lens.Simple
import           Text.Read
import qualified Text.XML.DublinCore.Conduit.Parse as DC
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- {{{ Utils
asDate :: MonadThrow m => Text -> m UTCTime
asDate text = maybe (throw $ InvalidTime text) (return . zonedTimeToUTC) $
  parseTimeRFC3339 text <|> parseTimeRFC2822 text <|> parseTimeRFC822 text

asInt :: MonadThrow m => Text -> m Int
asInt t = maybe (throwM $ InvalidInt t) return . readMaybe $ unpack t

projectC :: Monad m => Fold a a' b b' -> Conduit a m b
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

syndicationTag :: MonadThrow m => Text -> ConduitM Event o m a -> ConduitM Event o m (Maybe a)
syndicationTag name = tagIgnoreAttrs (matching (== syndicationName name))


data SyndicationPeriod = Hourly | Daily | Weekly | Monthly | Yearly deriving(Eq, Generic, Ord, Show)

asSyndicationPeriod :: MonadThrow m => Text -> m SyndicationPeriod
asSyndicationPeriod "hourly"  = pure Hourly
asSyndicationPeriod "daily"   = pure Daily
asSyndicationPeriod "weekly"  = pure Weekly
asSyndicationPeriod "monthly" = pure Monthly
asSyndicationPeriod "yearly"  = pure Yearly
asSyndicationPeriod t         = throw $ InvalidSyndicationPeriod t

-- | __Syndication__ extension model.
data SyndicationInfo = SyndicationInfo
  { updatePeriod    :: Maybe SyndicationPeriod
  , updateFrequency :: Maybe Int
  , updateBase      :: Maybe UTCTime
  } deriving(Eq, Generic, Ord, Show)

-- | Construct an empty 'SyndicationInfo'.
mkSyndicationInfo :: SyndicationInfo
mkSyndicationInfo = SyndicationInfo mzero mzero mzero


data ElementPiece = ElementPeriod SyndicationPeriod | ElementFrequency Int | ElementBase UTCTime

makeTraversals ''ElementPiece

-- | Parse __Syndication__ elements.
syndicationInfo :: MonadThrow m => ConduitM Event o m SyndicationInfo
syndicationInfo = manyYield' (choose piece) =$= parser where
  parser = getZipConduit $ SyndicationInfo
    <$> ZipConduit (projectC _ElementPeriod =$= headC)
    <*> ZipConduit (projectC _ElementFrequency =$= headC)
    <*> ZipConduit (projectC _ElementBase =$= headC)
  piece = [ fmap ElementPeriod <$> syndicationPeriod
          , fmap ElementFrequency <$> syndicationFrequency
          , fmap ElementBase <$> syndicationBase
          ]

-- | Parse a @\<sy:updatePeriod\>@ element.
syndicationPeriod :: MonadThrow m => ConduitM Event o m (Maybe SyndicationPeriod)
syndicationPeriod = syndicationTag "updatePeriod" (content >>= asSyndicationPeriod)

-- | Parse a @\<sy:updateFrequency\>@ element.
syndicationFrequency :: MonadThrow m => ConduitM Event o m (Maybe Int)
syndicationFrequency = syndicationTag "updateFrequency" (content >>= asInt)

-- | Parse a @\<sy:updateBase\>@ element.
syndicationBase :: MonadThrow m => ConduitM Event o m (Maybe UTCTime)
syndicationBase = syndicationTag "updateBase" (content >>= asDate)

-- | __Syndication__ tag type.
data SyndicationModule :: *

data instance Sing SyndicationModule = SSyndicationModule

instance SingI SyndicationModule where sing = SSyndicationModule

instance ParseRssExtension SyndicationModule where
  parseRssChannelExtension = SyndicationChannel <$> syndicationInfo
  parseRssItemExtension    = pure SyndicationItem


data instance RssChannelExtension SyndicationModule = SyndicationChannel SyndicationInfo deriving(Eq, Generic, Ord, Show)
data instance RssItemExtension SyndicationModule = SyndicationItem deriving(Eq, Generic, Ord, Show)
