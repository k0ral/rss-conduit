{-# LANGUAGE OverloadedStrings #-}
-- | Streaming renderers for the RSS 2.0 standard.
module Text.RSS.Conduit.Render
  ( -- * Top-level
    renderRssDocument
    -- * Elements
  , renderRssItem
  , renderRssSource
  , renderRssEnclosure
  , renderRssGuid
  , renderRssCloud
  , renderRssCategory
  , renderRssImage
  , renderRssTextInput
  , renderRssSkipDays
  , renderRssSkipHours
  ) where

-- {{{ Imports
import           Text.RSS.Types

import           Control.Lens.Getter
import           Control.Monad

import           Data.Conduit
import           Data.Monoid
import           Data.MonoTraversable
import           Data.Set
import           Data.Text              as Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Version
import           Data.XML.Types

import           Network.URI

import           Safe

import           Text.XML.Stream.Render
-- }}}

-- | Render the top-level @\<rss\>@ element.
renderRssDocument :: (Monad m) => RssDocument -> Source m Event
renderRssDocument d = tag "rss" (attr "version" . pack . showVersion $ d^.documentVersion_) $ do
  textTag "title" $ d^.channelTitle_
  textTag "link" . tshow $ d^.channelLink_
  textTag "description" $ d^.channelDescription_
  optionalTextTag "copyright" $ d^.channelCopyright_
  optionalTextTag "language" $ d^.channelLanguage_
  optionalTextTag "managingEditor" $ d^.channelManagingEditor_
  optionalTextTag "webMaster" $ d^.channelWebmaster_
  forM_ (d^.channelPubDate_) $ dateTag "pubDate"
  forM_ (d^.channelLastBuildDate_) $ dateTag "lastBuildDate"
  forM_ (d^.channelCategories_) renderRssCategory
  optionalTextTag "generator" $ d^.channelGenerator_
  forM_ (d^.channelDocs_) $ optionalTextTag "docs" . tshow
  forM_ (d^.channelCloud_) renderRssCloud
  forM_ (d^.channelTtl_) $ textTag "ttl" . tshow
  forM_ (d^.channelImage_) renderRssImage
  optionalTextTag "rating" $ d^.channelRating_
  forM_ (d^.channelTextInput_) renderRssTextInput
  renderRssSkipHours $ d^.channelSkipHours_
  renderRssSkipDays $ d^.channelSkipDays_

-- | Render an @\<item\>@ element.
renderRssItem :: (Monad m) => RssItem -> Source m Event
renderRssItem i = tag "item" mempty $ do
  optionalTextTag "title" $ i^.itemTitle_
  forM_ (i^.itemLink_) $ textTag "link" . tshow
  optionalTextTag "description" $ i^.itemDescription_
  optionalTextTag "author" $ i^.itemAuthor_
  forM_ (i^.itemCategories_) renderRssCategory
  forM_ (i^.itemComments_) $ textTag "comments" . tshow
  forM_ (i^.itemEnclosure_) renderRssEnclosure
  forM_ (i^.itemGuid_) renderRssGuid
  forM_ (i^.itemPubDate_) $ dateTag "pubDate"
  forM_ (i^.itemSource_) renderRssSource

-- | Render a @\<source\>@ element.
renderRssSource :: (Monad m) => RssSource -> Source m Event
renderRssSource s = tag "source" (attr "url" . tshow $ s^.sourceUrl_) . content $ s^.sourceName_

-- | Render an @\<enclosure\>@ element.
renderRssEnclosure :: (Monad m) => RssEnclosure -> Source m Event
renderRssEnclosure e = tag "enclosure" attributes mempty where
  attributes = attr "url" (tshow $ e^.enclosureUrl_)
    <> attr "length" (tshow $ e^.enclosureLength_)
    <> attr "type" (e^.enclosureType_)

-- | Render a @\<guid\>@ element.
renderRssGuid :: (Monad m) => RssGuid -> Source m Event
renderRssGuid (GuidUri u) = tag "guid" (attr "isPermaLink" "true") . content $ tshow u
renderRssGuid (GuidText t) = tag "guid" mempty $ content t

-- | Render a @\<cloud\>@ element.
renderRssCloud :: (Monad m) => RssCloud -> Source m Event
renderRssCloud c = tag "cloud" attributes $ return () where
  attributes = attr "domain" (pack domain)
    <> attr "port" (pack $ tailDef "" port)
    <> attr "path" (pack $ path ++ query ++ fragment)
    <> attr "registerProcedure" (c^.cloudRegisterProcedure_)
    <> attr "protocol" (describe $ c^.cloudProtocol_)
  URI scheme authority path query fragment = c^.cloudUri_
  domain = maybe "" (\a -> uriUserInfo a ++ uriRegName a) authority
  port = maybe "" uriPort authority
  describe ProtocolXmlRpc = "xml-rpc"
  describe ProtocolSoap = "soap"
  describe ProtocolHttpPost = "http-post"

-- | Render a @\<category\>@ element.
renderRssCategory :: (Monad m) => RssCategory -> Source m Event
renderRssCategory c = tag "category" (attr "domain" $ c^.categoryDomain_) . content $ c^.categoryName_

-- | Render an @\<image\>@ element.
renderRssImage :: (Monad m) => RssImage -> Source m Event
renderRssImage i = tag "image" mempty $ do
  textTag "uri" . tshow $ i^.imageUri_
  textTag "title" $ i^.imageTitle_
  textTag "link" . tshow $ i^.imageLink_
  forM_ (i^.imageWidth_) $ textTag "height" . tshow
  forM_ (i^.imageHeight_) $ textTag "width" . tshow
  optionalTextTag "description" $ i^.imageDescription_

-- | Render a @\<textInput\>@ element.
renderRssTextInput :: (Monad m) => RssTextInput -> Source m Event
renderRssTextInput t = tag "textInput" mempty $ do
  textTag "title" $ t^.textInputTitle_
  textTag "description" $ t^.textInputDescription_
  textTag "name" $ t^.textInputName_
  textTag "link" . tshow $ t^.textInputLink_

-- | Render a @\<skipDays\>@ element.
renderRssSkipDays :: (Monad m) => Set Day -> Source m Event
renderRssSkipDays s = tag "skipDays" mempty $ forM_ s $ textTag "day" . tshow

-- | Render a @\<skipHours\>@ element.
renderRssSkipHours :: (Monad m) => Set Hour -> Source m Event
renderRssSkipHours s = tag "skipHour" mempty $ forM_ s $ textTag "hour" . tshow


-- {{{ Utils
tshow :: (Show a) => a -> Text
tshow = pack . show

textTag :: (Monad m) => Name -> Text -> Source m Event
textTag name = tag name mempty . content

optionalTextTag :: (Monad m) => Name -> Text -> Source m Event
optionalTextTag name value = unless (onull value) $ textTag name value

dateTag :: (Monad m) => Name -> UTCTime -> Source m Event
dateTag name = tag name mempty . content . formatTimeRFC822 . utcToZonedTime utc
-- }}}
