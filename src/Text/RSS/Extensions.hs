{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | Support for RSS extensions.
-- Cf specification at <http://web.resource.org/rss/1.0/modules/>.
--
-- To implement an RSS extension:
--
-- - Create a data-type, that will be used as a tag to identify the extension.
--   To allow stacking multiple extensions, your data-type should have kind * -> *
--
--   > data MyExtension otherExtensions = MyExtension otherExtensions
--
-- - Implement extension types for @\<channel\>@ and @\<item\>@ elements:
--
--   > data instance RssChannelExtension (MyExtension e) = MyExtensionChannel
--   >   { -- ... add your fields ...
--   >   , otherChannelExtensions :: RssChannelExtension e
--   >   }
--   >
--   > data instance RssItemExtension (MyExtension e) = MyExtensionItem
--   >   { -- ... add your fields ...
--   >   , otherItemExtensions :: RssItemExtension e
--   >   }
--
-- - Implement 'ParseRssExtension' and 'RenderRssExtension' type classes:
--
--   > -- Parser should rely on ZipConduit to be order-insensitive
--   > instance ParseRssExtension e => ParseRssExtension (MyExtension e) where
--   >   parseRssChannelExtension = getZipConduit $ MyExtensionChannel
--   >     <$> ZipConduit -- ... parse fields
--   >     <*> ZipConduit parseRssChannelExtension
--   >   parseRssItemExtension = -- ... similarly
--   >
--   > instance RenderRssExtension e => RenderRssExtension (MyExtension e) where
--   >   renderRssChannelExtension (MyExtensionChannel {- fields -} otherChannelExtensions) = do
--   >     -- ... render fields
--   >     renderRssChannelExtension otherChannelExtensions
--   >   renderRssItemExtension (MyExtensionItem {- fields -} otherItemExtensions) = -- ... similarly
module Text.RSS.Extensions where

-- {{{ Imports
import           Control.Exception.Safe  as Exception
import           Data.Conduit
import           Data.Maybe
import           Data.Proxy
import           Data.Text
import           Data.XML.Types
import           GHC.Generics
import           Text.Atom.Conduit.Parse
import           Text.Atom.Types
import           Text.Read               (readMaybe)
import           Text.RSS.Types
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- * Parsing

-- | Class of RSS extensions that can be parsed.
class ParseRssExtension a where
  -- | This parser will be fed with all 'Event's within the @\<channel\>@ element.
  -- Therefore, it is expected to ignore 'Event's unrelated to the RSS extension.
  parseRssChannelExtension :: MonadThrow m => ConduitT Event o m (RssChannelExtension a)
  -- | This parser will be fed with all 'Event's within the @\<item\>@ element.
  -- Therefore, it is expected to ignore 'Event's unrelated to the RSS extension.
  parseRssItemExtension :: MonadThrow m => ConduitT Event o m (RssItemExtension a)

instance ParseRssExtension NoExtensions where
  parseRssChannelExtension = pure NoChannelExtensions
  parseRssItemExtension = pure NoItemExtensions


-- * Rendering

-- | Class of RSS extensions that can be rendered.
class RenderRssExtension e where
  -- | Render extension for the @\<channel\>@ element.
  renderRssChannelExtension :: Monad m => RssChannelExtension e -> ConduitT () Event m ()
  -- | Render extension for the @\<item\>@ element.
  renderRssItemExtension :: Monad m => RssItemExtension e -> ConduitT () Event m ()

instance RenderRssExtension NoExtensions where
  renderRssChannelExtension = const $ pure ()
  renderRssItemExtension = const $ pure ()
