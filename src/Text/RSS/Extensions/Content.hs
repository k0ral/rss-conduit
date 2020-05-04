{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
-- | __Content__ extension for RSS.
-- Cf specification at <http://web.resource.org/rss/1.0/modules/content/>.
--
-- This implementation corresponds to the /updated syntax/ from the specification.
module Text.RSS.Extensions.Content
  ( -- * Types
    ContentModule(..)
  , RssChannelExtension(..)
  , RssItemExtension(..)
    -- * Parser
  , contentEncoded
    -- * Renderer
  , renderContentEncoded
    -- * Misc
  , namespacePrefix
  , namespaceURI
  ) where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                (ConduitT, Source, ZipConduit (..), headDefC, (.|))
import           Control.Exception.Safe as Exception
import           Control.Monad
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.XML.Types
import           GHC.Generics
import           Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as Render
import           URI.ByteString
-- }}}

-- | __Content__ tag type.
newtype ContentModule a = ContentModule a

instance ParseRssExtension a => ParseRssExtension (ContentModule a) where
  parseRssChannelExtension = parseRssChannelExtension
  parseRssItemExtension    = getZipConduit $ ContentItem
    <$> ZipConduit (manyYield' contentEncoded .| headDefC mempty)
    <*> ZipConduit parseRssItemExtension

instance RenderRssExtension a => RenderRssExtension (ContentModule a) where
  renderRssChannelExtension = renderRssChannelExtension
  renderRssItemExtension (ContentItem e a) = do
    unless (Text.null e) $ renderContentEncoded e
    renderRssItemExtension a

data instance RssChannelExtension (ContentModule a) = ContentChannel { channelContentOther :: RssChannelExtension a }

deriving instance Eq (RssChannelExtension a) => Eq (RssChannelExtension (ContentModule a))
deriving instance Ord (RssChannelExtension a) => Ord (RssChannelExtension (ContentModule a))
deriving instance Read (RssChannelExtension a) => Read (RssChannelExtension (ContentModule a))
deriving instance Show (RssChannelExtension a) => Show (RssChannelExtension (ContentModule a))
deriving instance Generic (RssChannelExtension a) => Generic (RssChannelExtension (ContentModule a))

data instance RssItemExtension (ContentModule a) = ContentItem
  { itemContent :: Text
  , itemContentOther   :: RssItemExtension a
  }

deriving instance Eq (RssItemExtension a) => Eq (RssItemExtension (ContentModule a))
deriving instance Ord (RssItemExtension a) => Ord (RssItemExtension (ContentModule a))
deriving instance Read (RssItemExtension a) => Read (RssItemExtension (ContentModule a))
deriving instance Show (RssItemExtension a) => Show (RssItemExtension (ContentModule a))
deriving instance Generic (RssItemExtension a) => Generic (RssItemExtension (ContentModule a))



-- | XML prefix is @content@.
namespacePrefix :: Text
namespacePrefix = "content"

-- | XML namespace is @http://purl.org/rss/1.0/modules/content/@
namespaceURI :: URIRef Absolute
namespaceURI = uri where Right uri = parseURI laxURIParserOptions "http://purl.org/rss/1.0/modules/content/"

contentName :: Text -> Name
contentName string = Name string (Just "http://purl.org/rss/1.0/modules/content/") (Just namespacePrefix)

-- | Parse a @\<content:encoded\>@ element.
contentEncoded :: MonadThrow m => ConduitT Event o m (Maybe Text)
contentEncoded = tagIgnoreAttrs (matching (== contentName "encoded")) content

-- | Render a @\<content:encoded\>@ element.
renderContentEncoded :: Monad m => Text -> ConduitT () Event m ()
renderContentEncoded = Render.tag (contentName "encoded") mempty . Render.content
