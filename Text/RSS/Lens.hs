{-# LANGUAGE TemplateHaskell #-}
module Text.RSS.Lens (module Text.RSS.Lens) where

-- {{{ Imports
import           Lens.Simple

import           Text.RSS.Types

import           URI.ByteString
-- }}}


$(makeLensesBy (\n -> Just (n ++ "L")) ''RssCategory)
$(makeLensesBy (\n -> Just (n ++ "L")) ''RssEnclosure)
$(makeLensesBy (\n -> Just (n ++ "L")) ''RssSource)

$(makeLensesBy
  (let f "itemCategories" = Nothing
       f "itemEnclosure"  = Nothing
       f n                = Just (n ++ "L")
   in f)
  ''RssItem)

itemCategoriesL :: Traversal' (RssItem e) RssCategory
itemCategoriesL inj a@RssItem { itemCategories = c } = (\x -> a { itemCategories = c }) <$> traverse inj c
{-# INLINE itemCategoriesL #-}

itemEnclosureL :: Traversal' (RssItem e) RssEnclosure
itemEnclosureL inj a@RssItem { itemEnclosure = e } = (\x -> a { itemEnclosure = e }) <$> traverse inj e
{-# INLINE itemEnclosureL #-}

$(makeLensesBy (\n -> Just (n ++ "L")) ''RssTextInput)
$(makeLensesBy (\n -> Just (n ++ "L")) ''RssCloud)
$(makeLensesBy (\n -> Just (n ++ "L")) ''RssImage)
$(makeLensesBy
  (let f "channelItems"      = Nothing
       f "channelCategories" = Nothing
       f n                   = Just (n ++ "L")
  in f)
  ''RssDocument)

channelItemsL :: Traversal' (RssDocument e) (RssItem e)
channelItemsL inj a@RssDocument { channelItems = i } = (\x -> a { channelItems = i }) <$> traverse inj i
{-# INLINE channelItemsL #-}

channelCategoriesL :: Traversal' (RssDocument e) RssCategory
channelCategoriesL inj a@RssDocument { channelCategories = c } = (\x -> a { channelCategories = c }) <$> traverse inj c
{-# INLINE channelCategoriesL #-}
