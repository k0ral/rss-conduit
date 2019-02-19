{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
-- {{{ Imports
import           Text.RSS.Conduit.Parse          as Parser
import           Text.RSS.Conduit.Render         as Renderer
import           Text.RSS.Extensions
import           Text.RSS.Extensions.Atom
import           Text.RSS.Extensions.Content
import           Text.RSS.Extensions.DublinCore
import           Text.RSS.Extensions.Syndication
import           Text.RSS.Lens
import           Text.RSS.Types
import           Text.RSS1.Conduit.Parse         as Parser

import Arbitrary
import           Blaze.ByteString.Builder        (toByteString)
import           Conduit
import           Control.Monad
import           Data.Char
import           Data.Conduit
import           Data.Conduit.List
import           Data.Maybe
import           Data.String
import           Data.Text                       (Text)
import           Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding         as Lazy
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.Version
import           Data.Vinyl.Core
import           Data.Void
import           Data.XML.Types
import           System.FilePath
import           System.IO
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Atom.Conduit.Parse
import           Text.Atom.Types
import           Text.XML.Stream.Parse           as XML hiding (choose)
import           Text.XML.Stream.Render
import           URI.ByteString
import           URI.ByteString.QQ
-- }}}

main :: IO ()
main = defaultMain $ testGroup "Property tests"
  [ roundtripProperty "RssTextInput" renderRssTextInput rssTextInput
  , roundtripProperty "RssImage" renderRssImage rssImage
  , roundtripProperty "RssCategory" renderRssCategory rssCategory
  , roundtripProperty "RssEnclosure" renderRssEnclosure rssEnclosure
  , roundtripProperty "RssSource" renderRssSource rssSource
  , roundtripProperty "RssGuid" renderRssGuid rssGuid
--   , roundtripProperty "RssItem"
--       (renderRssItem :: RssItem '[] -> ConduitT () Event Maybe ())
--       rssItem
  , roundtripProperty "DublinCore"
      (renderRssChannelExtension @DublinCoreModule)
      (Just <$> parseRssChannelExtension)
  , roundtripProperty "Syndication"
      (renderRssChannelExtension @SyndicationModule)
      (Just <$> parseRssChannelExtension)
  , roundtripProperty "Atom"
      (renderRssChannelExtension @AtomModule)
      (Just <$> parseRssChannelExtension)
  , roundtripProperty "Content"
      (renderRssItemExtension @ContentModule)
      (Just <$> parseRssItemExtension)
  ]


roundtripProperty :: Eq a => Arbitrary a => Show a
                  => TestName -> (a -> ConduitT () Event Maybe ()) -> ConduitT Event Void Maybe (Maybe a) -> TestTree
roundtripProperty name render parse = testProperty ("parse . render = id (" <> name <> ")") $ do
  input <- arbitrary
  let intermediate = fmap (decodeUtf8 . toByteString) $ runConduit $ render input .| renderBuilder def .| foldC
      output = join $ runConduit $ render input .| parse
  return $ counterexample (show input <> " | " <> show intermediate <> " | " <> show output) $ Just input == output

roundtripTextInputProperty :: TestTree
roundtripTextInputProperty = testProperty "parse . render = id (RssTextInput)" $ \t -> either (const False) (t ==) (runConduit $ renderRssTextInput t .| force "ERROR" rssTextInput)

roundtripImageProperty :: TestTree
roundtripImageProperty = testProperty "parse . render = id (RssImage)" $ \t -> either (const False) (t ==) (runConduit $ renderRssImage t .| force "ERROR" rssImage)

roundtripCategoryProperty :: TestTree
roundtripCategoryProperty = testProperty "parse . render = id (RssCategory)" $ \t -> either (const False) (t ==) (runConduit $ renderRssCategory t .| force "ERROR" rssCategory)

roundtripEnclosureProperty :: TestTree
roundtripEnclosureProperty = testProperty "parse . render = id (RssEnclosure)" $ \t -> either (const False) (t ==) (runConduit $ renderRssEnclosure t .| force "ERROR" rssEnclosure)

roundtripSourceProperty :: TestTree
roundtripSourceProperty = testProperty "parse . render = id (RssSource)" $ \t -> either (const False) (t ==) (runConduit $ renderRssSource t .| force "ERROR" rssSource)

roundtripGuidProperty :: TestTree
roundtripGuidProperty = testProperty "parse . render = id (RssGuid)" $ \t -> either (const False) (t ==) (runConduit $ renderRssGuid t .| force "ERROR" rssGuid)

roundtripItemProperty :: TestTree
roundtripItemProperty = testProperty "parse . render = id (RssItem)" $ \(t :: RssItem '[]) -> either (const False) (t ==) (runConduit $ renderRssItem t .| force "ERROR" rssItem)
