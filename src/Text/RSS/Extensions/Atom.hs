{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
-- | __Atom__ extension for RSS.
-- Cf specification at <http://www.rssboard.org/rss-profile#namespace-elements-atom>.
module Text.RSS.Extensions.Atom where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                  (ZipConduit (..), headC, (.|))
import           GHC.Generics
import           Text.Atom.Conduit.Parse
import           Text.Atom.Conduit.Render
import           Text.Atom.Types
import           Text.XML.Stream.Parse
-- }}}

-- | __Atom__ tag type.
newtype AtomModule a = AtomModule a

instance ParseRssExtension a => ParseRssExtension (AtomModule a) where
  parseRssChannelExtension = getZipConduit $ AtomChannel
    <$> ZipConduit (manyYield' atomLink .| headC)
    <*> ZipConduit parseRssChannelExtension
  parseRssItemExtension    = getZipConduit $ AtomItem
    <$> ZipConduit (manyYield' atomLink .| headC)
    <*> ZipConduit parseRssItemExtension

instance RenderRssExtension a => RenderRssExtension (AtomModule a) where
  renderRssChannelExtension AtomChannel{..} = do
    mapM_ renderAtomLink channelAtomLink
    renderRssChannelExtension channelAtomOther
  renderRssItemExtension AtomItem{..}   = do
    mapM_ renderAtomLink itemAtomLink
    renderRssItemExtension itemAtomOther


data instance RssChannelExtension (AtomModule a) = AtomChannel
  { channelAtomLink :: Maybe AtomLink
  , channelAtomOther :: RssChannelExtension a
  }

deriving instance Eq (RssChannelExtension a) => Eq (RssChannelExtension (AtomModule a))
deriving instance Ord (RssChannelExtension a) => Ord (RssChannelExtension (AtomModule a))
--deriving instance Read (RssChannelExtension a) => Read (RssChannelExtension (AtomModule a))
deriving instance Show (RssChannelExtension a) => Show (RssChannelExtension (AtomModule a))
deriving instance Generic (RssChannelExtension a) => Generic (RssChannelExtension (AtomModule a))


data instance RssItemExtension (AtomModule a) = AtomItem
  { itemAtomLink :: Maybe AtomLink
  , itemAtomOther :: RssItemExtension a
  }

deriving instance Eq (RssItemExtension a) => Eq (RssItemExtension (AtomModule a))
deriving instance Ord (RssItemExtension a) => Ord (RssItemExtension (AtomModule a))
--deriving instance Read (RssItemExtension a) => Read (RssItemExtension (AtomModule a))
deriving instance Show (RssItemExtension a) => Show (RssItemExtension (AtomModule a))
deriving instance Generic (RssItemExtension a) => Generic (RssItemExtension (AtomModule a))
