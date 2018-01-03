{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
-- | __Atom__ extension for RSS.
-- Cf specification at <http://www.rssboard.org/rss-profile#namespace-elements-atom>.
module Text.RSS.Extensions.Atom where

-- {{{ Imports
import           Text.RSS.Extensions
import           Text.RSS.Types

import           Conduit                  (headC, (=$=))
import           Data.Singletons
import           GHC.Generics
import           Text.Atom.Conduit.Parse
import           Text.Atom.Conduit.Render
import           Text.Atom.Types
import           Text.XML.Stream.Parse
-- }}}

-- | __Atom__ tag type.
data AtomModule :: *

data instance Sing AtomModule = SAtomModule

instance SingI AtomModule where sing = SAtomModule

instance ParseRssExtension AtomModule where
  parseRssChannelExtension = AtomChannel <$> (manyYield' atomLink =$= headC)
  parseRssItemExtension    = AtomItem <$> (manyYield' atomLink =$= headC)

instance RenderRssExtension AtomModule where
  renderRssChannelExtension = mapM_ renderAtomLink . channelAtomLink
  renderRssItemExtension    = mapM_ renderAtomLink . itemAtomLink

data instance RssChannelExtension AtomModule = AtomChannel { channelAtomLink :: Maybe AtomLink }
  deriving(Eq, Generic, Show)
data instance RssItemExtension AtomModule = AtomItem { itemAtomLink :: Maybe AtomLink }
  deriving(Eq, Generic, Show)
