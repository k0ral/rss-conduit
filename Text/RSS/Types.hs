{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
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
import           Control.Lens.TH
import           Control.Monad.Catch

import           Data.Set
import           Data.Text           hiding (map)
import           Data.Time.Clock
import           Data.Time.LocalTime ()
import           Data.Version

import           GHC.Generics        hiding ((:+:))

import           Network.URI

import           Text.Read
-- }}}


data RssException = InvalidBool Text
                  | InvalidDay Text
                  | InvalidHour Int
                  | InvalidInt Text
                  | InvalidURI Text
                  | InvalidVersion Text
                  | InvalidProtocol Text

deriving instance Eq RssException
instance Show RssException where
  show (InvalidBool t) = "Invalid bool: " ++ unpack t
  show (InvalidDay t) = "Invalid day: " ++ unpack t
  show (InvalidHour i) = "Invalid hour: " ++ show i
  show (InvalidInt t) = "Invalid int: " ++ unpack t
  show (InvalidURI t) = "Invalid URI: " ++ unpack t
  show (InvalidVersion t) = "Invalid version: " ++ unpack t
  show (InvalidProtocol t) = "Invalid Protocol: expected \"xml-rpc\", \"soap\" or \"http-post\", got \"" ++ unpack t ++ "\""
instance Exception RssException


-- | The @\<category\>@ element.
declareLenses [d|
  data RssCategory = RssCategory
    { categoryDomain_ :: Text
    , categoryName_ :: Text
    }
  |]

deriving instance Eq RssCategory
deriving instance Generic RssCategory
deriving instance Show RssCategory


-- | The @\<enclosure\>@ element.
declareLenses [d|
  data RssEnclosure = RssEnclosure
    { enclosureUrl_ :: URI
    , enclosureLength_ :: Int
    , enclosureType_ :: Text
    }
  |]

deriving instance Eq RssEnclosure
deriving instance Generic RssEnclosure
deriving instance Show RssEnclosure


-- | The @\<source\>@ element.
declareLenses [d|
  data RssSource = RssSource
    { sourceUrl_ :: URI
    , sourceName_ :: Text
    }
  |]

deriving instance Eq RssSource
deriving instance Generic RssSource
deriving instance Show RssSource


-- | The @\<guid\>@ element.
data RssGuid = GuidText Text | GuidUri URI
  deriving(Eq, Generic, Show)


-- | The @\<item\>@ element.
declareLenses [d|
  data RssItem = RssItem
    { itemTitle_ :: Text
    , itemLink_ :: Maybe URI
    , itemDescription_ :: Text
    , itemAuthor_ :: Text
    , itemCategories_ :: [RssCategory]
    , itemComments_ :: Maybe URI
    , itemEnclosure_ :: [RssEnclosure]
    , itemGuid_ :: Maybe RssGuid
    , itemPubDate_ :: Maybe UTCTime
    , itemSource_ :: Maybe RssSource
    }
  |]

deriving instance Eq RssItem
deriving instance Generic RssItem
deriving instance Show RssItem


-- | The @\<textInput\>@ element.
declareLenses [d|
  data RssTextInput = RssTextInput
    { textInputTitle_ :: Text
    , textInputDescription_ :: Text
    , textInputName_ :: Text
    , textInputLink_ :: URI
    }
  |]

deriving instance Eq RssTextInput
deriving instance Generic RssTextInput
deriving instance Show RssTextInput

data CloudProtocol = ProtocolXmlRpc | ProtocolSoap | ProtocolHttpPost
  deriving(Eq, Generic, Show)

-- | The @\<cloud\>@ element.
declareLenses [d|
  data RssCloud = RssCloud
    { cloudUri_ :: URI
    , cloudRegisterProcedure_ :: Text
    , cloudProtocol_ :: CloudProtocol
    }
  |]

deriving instance Eq RssCloud
deriving instance Generic RssCloud
deriving instance Show RssCloud

-- | The @\<image\>@ element.
declareLenses [d|
  data RssImage = RssImage
    { imageUri_ :: URI
    , imageTitle_ :: Text
    , imageLink_ :: URI
    , imageWidth_ :: Maybe Int
    , imageHeight_ :: Maybe Int
    , imageDescription_ :: Text
    }
  |]

deriving instance Eq RssImage
deriving instance Generic RssImage
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
declareLenses [d|
  data RssDocument = RssDocument
    { documentVersion_ :: Version
    , channelTitle_ :: Text
    , channelLink_ :: URI
    , channelDescription_ :: Text
    , channelItems_ :: [RssItem]
    , channelLanguage_ :: Text
    , channelCopyright_ :: Text
    , channelManagingEditor_ :: Text
    , channelWebmaster_ :: Text
    , channelPubDate_ :: Maybe UTCTime
    , channelLastBuildDate_ :: Maybe UTCTime
    , channelCategories_ :: [RssCategory]
    , channelGenerator_ :: Text
    , channelDocs_ :: Maybe URI
    , channelCloud_ :: Maybe RssCloud
    , channelTtl_ :: Maybe Int
    , channelImage_ :: Maybe RssImage
    , channelRating_ :: Text
    , channelTextInput_ :: Maybe RssTextInput
    , channelSkipHours_ :: Set Hour
    , channelSkipDays_ :: Set Day
    }
  |]

deriving instance Eq RssDocument
deriving instance Generic RssDocument
deriving instance Show RssDocument
