{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
-- | RSS is an XML dialect for Web content syndication.
--
-- Example:
--
-- > <?xml version="1.0"?>
-- > <rss version="2.0">
-- >    <channel>
-- >       <title>Liftoff News</title>
-- >       <link>http://liftoff.msfc.nasa.gov/</link>
-- >       <description>Liftoff to Space Exploration.</description>
-- >       <language>en-us</language>
-- >       <pubDate>Tue, 10 Jun 2003 04:00:00 GMT</pubDate>
-- >       <lastBuildDate>Tue, 10 Jun 2003 09:41:01 GMT</lastBuildDate>
-- >       <docs>http://blogs.law.harvard.edu/tech/rss</docs>
-- >       <generator>Weblog Editor 2.0</generator>
-- >       <managingEditor>editor@example.com</managingEditor>
-- >       <webMaster>webmaster@example.com</webMaster>
-- >       <item>
-- >          <title>Star City</title>
-- >          <link>http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp</link>
-- >          <description>How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's &lt;a href="http://howe.iki.rssi.ru/GCTC/gctc_e.htm"&gt;Star City&lt;/a&gt;.</description>
-- >          <pubDate>Tue, 03 Jun 2003 09:39:21 GMT</pubDate>
-- >          <guid>http://liftoff.msfc.nasa.gov/2003/06/03.html#item573</guid>
-- >       </item>
-- >    </channel>
-- > </rss>
module Text.RSS.Types where

-- {{{ Imports
import           Control.Exception.Safe

import           Data.Set
import           Data.Text              hiding (map)
import           Data.Time.Clock
import           Data.Time.LocalTime    ()
import           Data.Version

import           GHC.Generics           hiding ((:+:))

import           Text.Read

import           URI.ByteString
-- }}}


data RssException = InvalidBool Text
                  | InvalidDay Text
                  | InvalidHour Int
                  | InvalidInt Text
                  | InvalidURI URIParseError
                  | InvalidVersion Text
                  | InvalidProtocol Text
                  | InvalidTime Text
                  | MissingElement Text

deriving instance Eq RssException
deriving instance Show RssException

instance Exception RssException where
  displayException (InvalidBool t) = "Invalid bool: " ++ unpack t
  displayException (InvalidDay t) = "Invalid day: " ++ unpack t
  displayException (InvalidHour i) = "Invalid hour: " ++ show i
  displayException (InvalidInt t) = "Invalid int: " ++ unpack t
  displayException (InvalidURI t) = "Invalid URI reference: " ++ show t
  displayException (InvalidVersion t) = "Invalid version: " ++ unpack t
  displayException (InvalidProtocol t) = "Invalid Protocol: expected \"xml-rpc\", \"soap\" or \"http-post\", got \"" ++ unpack t ++ "\""
  displayException (InvalidTime t) = "Invalid time: " ++ unpack t
  displayException (MissingElement t) = "Missing element: " ++ unpack t


data RssURI = forall a . RssURI (URIRef a)

instance Eq RssURI where
  RssURI a@URI{} == RssURI b@URI{} = a == b
  RssURI a@RelativeRef{} == RssURI b@RelativeRef{} = a == b
  _ == _ = False

instance Ord RssURI where
  RssURI a@URI{} `compare` RssURI b@URI{} = a `compare` b
  RssURI a@RelativeRef{} `compare` RssURI b@RelativeRef{} = a `compare` b
  RssURI a@RelativeRef{} `compare` RssURI b@URI{} = LT
  _ `compare` _ = GT

instance Show RssURI where
  show (RssURI a@URI{})         = show a
  show (RssURI a@RelativeRef{}) = show a

withRssURI :: (forall a . URIRef a -> b) -> RssURI -> b
withRssURI f (RssURI a) = f a


-- | The @\<category\>@ element.
data RssCategory = RssCategory
  { categoryDomain :: Text
  , categoryName   :: Text
  }

deriving instance Eq RssCategory
deriving instance Generic RssCategory
deriving instance Ord RssCategory
deriving instance Show RssCategory


-- | The @\<enclosure\>@ element.
data RssEnclosure = RssEnclosure
  { enclosureUrl    :: RssURI
  , enclosureLength :: Int
  , enclosureType   :: Text
  }

deriving instance Eq RssEnclosure
deriving instance Generic RssEnclosure
deriving instance Ord RssEnclosure
deriving instance Show RssEnclosure


-- | The @\<source\>@ element.
data RssSource = RssSource
  { sourceUrl  :: RssURI
  , sourceName :: Text
  }

deriving instance Eq RssSource
deriving instance Generic RssSource
deriving instance Ord RssSource
deriving instance Show RssSource


-- | The @\<guid\>@ element.
data RssGuid = GuidText Text | GuidUri RssURI
  deriving(Eq, Generic, Ord, Show)


-- | The @\<item\>@ element.
data RssItem = RssItem
  { itemTitle       :: Text
  , itemLink        :: Maybe RssURI
  , itemDescription :: Text
  , itemAuthor      :: Text
  , itemCategories  :: [RssCategory]
  , itemComments    :: Maybe RssURI
  , itemEnclosure   :: [RssEnclosure]
  , itemGuid        :: Maybe RssGuid
  , itemPubDate     :: Maybe UTCTime
  , itemSource      :: Maybe RssSource
  }

deriving instance Eq RssItem
deriving instance Generic RssItem
deriving instance Ord RssItem
deriving instance Show RssItem


-- | The @\<textInput\>@ element.
data RssTextInput = RssTextInput
  { textInputTitle       :: Text
  , textInputDescription :: Text
  , textInputName        :: Text
  , textInputLink        :: RssURI
  }

deriving instance Eq RssTextInput
deriving instance Generic RssTextInput
deriving instance Ord RssTextInput
deriving instance Show RssTextInput

data CloudProtocol = ProtocolXmlRpc | ProtocolSoap | ProtocolHttpPost
  deriving(Eq, Generic, Ord, Show)

-- | The @\<cloud\>@ element.
data RssCloud = RssCloud
  { cloudUri               :: RssURI
  , cloudRegisterProcedure :: Text
  , cloudProtocol          :: CloudProtocol
  }

deriving instance Eq RssCloud
deriving instance Generic RssCloud
deriving instance Ord RssCloud
deriving instance Show RssCloud

-- | The @\<image\>@ element.
data RssImage = RssImage
  { imageUri         :: RssURI
  , imageTitle       :: Text
  , imageLink        :: RssURI
  , imageWidth       :: Maybe Int
  , imageHeight      :: Maybe Int
  , imageDescription :: Text
  }

deriving instance Eq RssImage
deriving instance Generic RssImage
deriving instance Ord RssImage
deriving instance Show RssImage


newtype Hour = Hour Int
  deriving(Eq, Generic, Ord, Show)

-- | Smart constructor for 'Hour'
asHour :: MonadThrow m => Int -> m Hour
asHour i
  | i >= 0 && i < 24 = return $ Hour i
  | otherwise = throwM $ InvalidHour i

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving(Enum, Eq, Generic, Ord, Read, Show)

-- | Basic parser for 'Day'.
asDay :: MonadThrow m => Text -> m Day
asDay t = maybe (throwM $ InvalidDay t) return . readMaybe $ unpack t

-- | The @\<rss\>@ element.
data RssDocument = RssDocument
  { documentVersion       :: Version
  , channelTitle          :: Text
  , channelLink           :: RssURI
  , channelDescription    :: Text
  , channelItems          :: [RssItem]
  , channelLanguage       :: Text
  , channelCopyright      :: Text
  , channelManagingEditor :: Text
  , channelWebmaster      :: Text
  , channelPubDate        :: Maybe UTCTime
  , channelLastBuildDate  :: Maybe UTCTime
  , channelCategories     :: [RssCategory]
  , channelGenerator      :: Text
  , channelDocs           :: Maybe RssURI
  , channelCloud          :: Maybe RssCloud
  , channelTtl            :: Maybe Int
  , channelImage          :: Maybe RssImage
  , channelRating         :: Text
  , channelTextInput      :: Maybe RssTextInput
  , channelSkipHours      :: Set Hour
  , channelSkipDays       :: Set Day
  }

deriving instance Eq RssDocument
deriving instance Generic RssDocument
deriving instance Ord RssDocument
deriving instance Show RssDocument
