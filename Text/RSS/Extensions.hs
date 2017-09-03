{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | Support for RSS extensions.
-- Cf specification at <http://web.resource.org/rss/1.0/modules/>.
--
-- For now, only parsing is implemented. Rendering will be implemented later.
module Text.RSS.Extensions where

-- {{{ Imports
import           Control.Exception.Safe       as Exception
import           Data.Conduit
import           Data.Maybe
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.Prelude.Bool
import           Data.Singletons.Prelude.Eq
import           Data.Singletons.Prelude.List
import           Data.Text
import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel
import           Data.XML.Types
import           Debug.Trace
import           GHC.Generics
import           Text.Atom.Conduit.Parse
import           Text.Atom.Types
import           Text.Read                    (readMaybe)
import           Text.RSS.Types
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- | Class of RSS extensions that can be parsed.
class ParseRssExtension a where
  -- | This parser will be fed with all 'Event's within the @\<channel\>@ element.
  -- Therefore, it is expected to ignore 'Event's unrelated to the RSS extension.
  parseRssChannelExtension :: MonadThrow m => ConduitM Event o m (RssChannelExtension a)
  -- | This parser will be fed with all 'Event's within the @\<item\>@ element.
  -- Therefore, it is expected to ignore 'Event's unrelated to the RSS extension.
  parseRssItemExtension :: MonadThrow m => ConduitM Event o m (RssItemExtension a)

-- | Requirement on a list of extension tags to be able to parse and combine them.
type ParseRssExtensions (e :: [*]) = (AllConstrained ParseRssExtension e, SingI e)

-- | Parse a combination of RSS extensions at @\<channel\>@ level.
parseRssChannelExtensions :: ParseRssExtensions e => MonadThrow m => ConduitM Event o m (RssChannelExtensions e)
parseRssChannelExtensions = f sing where
  f :: AllConstrained ParseRssExtension e => MonadThrow m
    => Sing e -> ConduitM Event o m (RssChannelExtensions e)
  f SNil = return $ RssChannelExtensions RNil
  f (SCons _ es) = fmap RssChannelExtensions $ getZipConduit $ (:&)
    <$> ZipConduit parseRssChannelExtension
    <*> ZipConduit (rssChannelExtension <$> f es)

-- | Parse a combination of RSS extensions at @\<item\>@ level.
parseRssItemExtensions :: ParseRssExtensions e => MonadThrow m => ConduitM Event o m (RssItemExtensions e)
parseRssItemExtensions = f sing where
  f :: AllConstrained ParseRssExtension e => MonadThrow m
    => Sing e -> ConduitM Event o m (RssItemExtensions e)
  f SNil = return $ RssItemExtensions RNil
  f (SCons _ es) = fmap RssItemExtensions $ getZipConduit $ (:&)
    <$> ZipConduit parseRssItemExtension
    <*> ZipConduit (rssItemExtension <$> f es)
