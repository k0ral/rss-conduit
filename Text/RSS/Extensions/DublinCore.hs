{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
-- | __Dublin Core__ extension for RSS.
--  Cf specification at <http://web.resource.org/rss/1.0/modules/dc/>.
module Text.RSS.Extensions.DublinCore
  ( DublinCoreModule(..)
  , RssChannelExtension(DublinCoreChannel)
  , RssItemExtension(DublinCoreItem)
  , DcMetaData(..)
  , mkDcMetaData
  ) where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                           hiding (throwM)
import           Control.Exception.Safe            as Exception
import           Control.Monad.Fix
import           Data.Maybe
import           Data.Singletons
import           Data.Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.XML.Types
import           GHC.Generics
import           Lens.Simple
import qualified Text.XML.DublinCore.Conduit.Parse as DC
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- {{{ Utils
projectC :: Monad m => Fold a a' b b' -> Conduit a m b
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
  } deriving(Eq, Generic, Ord, Show)

-- | Construct an empty 'DcMetaData'.
mkDcMetaData = DcMetaData mempty mempty mempty Nothing mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty


data ElementPiece = ElementContributor Text | ElementCoverage Text | ElementCreator Text
                  | ElementDate UTCTime | ElementDescription Text | ElementFormat Text
                  | ElementIdentifier Text | ElementLanguage Text | ElementPublisher Text
                  | ElementRelation Text | ElementRights Text | ElementSource Text
                  | ElementSubject Text | ElementTitle Text | ElementType Text

makeTraversals ''ElementPiece

-- | Parse a set of Dublin Core metadata elements.
dcMetadata :: MonadThrow m => ConduitM Event o m DcMetaData
dcMetadata = manyYield' (choose piece) =$= parser where
  parser = getZipConduit $ DcMetaData
    <$> ZipConduit (projectC _ElementContributor =$= headDefC "")
    <*> ZipConduit (projectC _ElementCoverage =$= headDefC "")
    <*> ZipConduit (projectC _ElementCreator =$= headDefC "")
    <*> ZipConduit (projectC _ElementDate =$= headC)
    <*> ZipConduit (projectC _ElementDescription =$= headDefC "")
    <*> ZipConduit (projectC _ElementFormat =$= headDefC "")
    <*> ZipConduit (projectC _ElementIdentifier =$= headDefC "")
    <*> ZipConduit (projectC _ElementLanguage =$= headDefC "")
    <*> ZipConduit (projectC _ElementPublisher =$= headDefC "")
    <*> ZipConduit (projectC _ElementRelation =$= headDefC "")
    <*> ZipConduit (projectC _ElementRights =$= headDefC "")
    <*> ZipConduit (projectC _ElementSource =$= headDefC "")
    <*> ZipConduit (projectC _ElementSubject =$= headDefC "")
    <*> ZipConduit (projectC _ElementTitle =$= headDefC "")
    <*> ZipConduit (projectC _ElementType =$= headDefC "")
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


-- | __Dublin Core__ tag type.
data DublinCoreModule :: *

data instance Sing DublinCoreModule = SDublinCoreModule

instance SingI DublinCoreModule where sing = SDublinCoreModule

instance ParseRssExtension DublinCoreModule where
  parseRssChannelExtension = DublinCoreChannel <$> dcMetadata
  parseRssItemExtension    = DublinCoreItem <$> dcMetadata


data instance RssChannelExtension DublinCoreModule = DublinCoreChannel DcMetaData deriving(Eq, Generic, Ord, Show)
data instance RssItemExtension DublinCoreModule = DublinCoreItem DcMetaData deriving(Eq, Generic, Ord, Show)
