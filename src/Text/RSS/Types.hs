{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
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
import           Data.Semigroup
import           Data.Set
import           Data.Singletons.Prelude.List
import           Data.Text                    (Text, unpack)
import           Data.Time.Clock
import           Data.Time.LocalTime          ()
import           Data.Version
import           Data.Vinyl.Core
import           GHC.Generics                 hiding ((:+:))
import           Text.Read
import           URI.ByteString
-- }}}

-- * RSS core

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
deriving instance Generic RssException
deriving instance Read RssException
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
--
-- This type is open to extensions.
data RssItem (extensions :: [*]) = RssItem
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
  , itemExtensions  :: RssItemExtensions extensions
  }

deriving instance (Eq (RssItemExtensions e)) => Eq (RssItem e)
deriving instance (Generic (RssItemExtensions e)) => Generic (RssItem e)
deriving instance (Ord (RssItemExtensions e)) => Ord (RssItem e)
deriving instance (Show (RssItemExtensions e)) => Show (RssItem e)

-- | Alias for 'RssItem' with no RSS extensions.
type RssItem' = RssItem '[]

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
  deriving(Eq, Generic, Ord, Read, Show)

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
  deriving(Eq, Generic, Ord, Read, Show)

instance Bounded Hour where
  minBound = Hour 0
  maxBound = Hour 23

instance Enum Hour where
  fromEnum (Hour h) = fromEnum h
  toEnum i = if i >= 0 && i < 24 then Hour i else error $ "Invalid hour: " <> show i


-- | Smart constructor for 'Hour'
asHour :: MonadThrow m => Int -> m Hour
asHour i
  | i >= 0 && i < 24 = return $ Hour i
  | otherwise = throwM $ InvalidHour i

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving(Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Basic parser for 'Day'.
asDay :: MonadThrow m => Text -> m Day
asDay t = maybe (throwM $ InvalidDay t) return . readMaybe $ unpack t

-- | The @\<rss\>@ element.
--
-- This type is open to extensions.
data RssDocument (extensions :: [*]) = RssDocument
  { documentVersion       :: Version
  , channelTitle          :: Text
  , channelLink           :: RssURI
  , channelDescription    :: Text
  , channelItems          :: [RssItem extensions]
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
  , channelExtensions     :: RssChannelExtensions extensions
  }

deriving instance (Eq (RssChannelExtensions e), Eq (RssItemExtensions e)) => Eq (RssDocument e)
deriving instance (Generic (RssChannelExtensions e), Generic (RssItemExtensions e)) => Generic (RssDocument e)
deriving instance (Ord (RssChannelExtensions e), Ord (RssItemExtensions e)) => Ord (RssDocument e)
deriving instance (Show (RssChannelExtensions e), Show (RssItemExtensions e)) => Show (RssDocument e)

-- | Alias for 'RssDocument' with no RSS extensions.
type RssDocument' = RssDocument '[]

-- * RSS extensions
--
-- $doc
-- To implement an RSS extension:
--
-- - Create a void data-type, that will be used as a tag to identify the extension:
--
--   > data MyExtension :: *
--
-- - Implement extension types for @\<channel\>@ and @\<item\>@ elements:
--
--   > data instance RssChannelExtension MyExtension = MyExtensionChannel { {- ... fields -} }
--   > data instance RssItemExtension MyExtension = MyExtensionItem { {- ... fields -} }
--
-- - Implement corresponding parsers (cf "Text.RSS.Extensions").

-- | @\<channel\>@ extension type.
data family RssChannelExtension extensionTag :: *

-- | @\<item\>@ extension type.
data family RssItemExtension extensionTag :: *

-- | Combination of multiple @\<channel\>@ extensions.
data family RssChannelExtensions (extensionTags :: [*]) :: *
data instance RssChannelExtensions a = RssChannelExtensions { rssChannelExtension :: Rec RssChannelExtension a }

deriving instance (Eq (Rec RssChannelExtension a)) => Eq (RssChannelExtensions a)
deriving instance (Generic (Rec RssChannelExtension a)) => Generic (RssChannelExtensions a)
deriving instance (Ord (Rec RssChannelExtension a)) => Ord (RssChannelExtensions a)
deriving instance (Read (Rec RssChannelExtension a)) => Read (RssChannelExtensions a)
deriving instance (Show (Rec RssChannelExtension a)) => Show (RssChannelExtensions a)

-- | Combination of multiple @\<item\>@ extensions.
data family RssItemExtensions (extensionTags :: [*]) :: *
data instance RssItemExtensions (a :: [*]) = RssItemExtensions { rssItemExtension :: Rec RssItemExtension a }

deriving instance (Eq (Rec RssItemExtension a)) => Eq (RssItemExtensions a)
deriving instance (Generic (Rec RssItemExtension a)) => Generic (RssItemExtensions a)
deriving instance (Ord (Rec RssItemExtension a)) => Ord (RssItemExtensions a)
deriving instance (Read (Rec RssItemExtension a)) => Read (RssItemExtensions a)
deriving instance (Show (Rec RssItemExtension a)) => Show (RssItemExtensions a)
