{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- | Streaming parsers for the RSS 2.0 standard.
--
-- This module re-exports a monomorphic version of the parsers from 'Text.RSS.Conduit.Parse' that ignores RSS extensions.
module Text.RSS.Conduit.Parse.Simple
  ( -- * Top-level
    rssDocument
    -- * Elements
  , rssCategory
  , rssCloud
  , rssEnclosure
  , rssGuid
  , rssImage
  , rssItem
  , rssSkipDays
  , rssSkipHours
  , rssSource
  , rssTextInput
  ) where

-- {{{ Imports
import qualified Text.RSS.Conduit.Parse as P
import           Text.RSS.Types

import           Control.Exception.Safe as Exception
import           Data.Conduit
import           Data.Set
import           Data.XML.Types
-- }}}

-- | Parse a @\<skipHours\>@ element.
rssSkipHours :: MonadThrow m => ConduitM Event o m (Maybe (Set Hour))
rssSkipHours = P.rssSkipHours

-- | Parse a @\<skipDays\>@ element.
rssSkipDays :: MonadThrow m => ConduitM Event o m (Maybe (Set Day))
rssSkipDays = P.rssSkipDays

-- | Parse a @\<textInput\>@ element.
rssTextInput :: MonadThrow m => ConduitM Event o m (Maybe RssTextInput)
rssTextInput = P.rssTextInput

-- | Parse an @\<image\>@ element.
rssImage :: MonadThrow m => ConduitM Event o m (Maybe RssImage)
rssImage = P.rssImage

-- | Parse a @\<category\>@ element.
rssCategory :: MonadThrow m => ConduitM Event o m (Maybe RssCategory)
rssCategory = P.rssCategory

-- | Parse a @\<cloud\>@ element.
rssCloud :: MonadThrow m => ConduitM Event o m (Maybe RssCloud)
rssCloud = P.rssCloud

-- | Parse a @\<guid\>@ element.
rssGuid :: MonadThrow m => ConduitM Event o m (Maybe RssGuid)
rssGuid = P.rssGuid

-- | Parse an @\<enclosure\>@ element.
rssEnclosure :: MonadThrow m => ConduitM Event o m (Maybe RssEnclosure)
rssEnclosure = P.rssEnclosure

-- | Parse a @\<source\>@ element.
rssSource :: MonadThrow m => ConduitM Event o m (Maybe RssSource)
rssSource = P.rssSource

-- | Parse an @\<item\>@ element.
--
-- RSS extensions are ignored.
rssItem :: MonadThrow m => ConduitM Event o m (Maybe RssItem')
rssItem = P.rssItem

-- | Parse an @\<rss\>@ element.
--
-- RSS extensions are ignored.
rssDocument :: MonadThrow m => ConduitM Event o m (Maybe RssDocument')
rssDocument = P.rssDocument
