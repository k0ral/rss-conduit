{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- {{{ Imports
import           Arbitrary

import           Control.Monad.Catch.Pure
import           Control.Monad.Trans.Resource

import           Data.Char
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML      as XML
import           Data.Default
import           Data.Version

import qualified Language.Haskell.HLint       as HLint (hlint)

import           Lens.Simple

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Text.Parser.Combinators
import           Text.RSS.Conduit.Parse       as Parser
import           Text.RSS.Conduit.Render      as Renderer
import           Text.RSS.Lens
import           Text.RSS.Types
import           Text.XML.Stream.Render

import           URI.ByteString

import           System.IO
-- }}}

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  , properties
  , hlint
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ skipHoursCase
  , skipDaysCase
  , textInputCase
  , imageCase
  , categoryCase
  , cloudCase
  , guidCase
  , enclosureCase
  , sourceCase
  , itemCase
  , documentCase
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ roundtripTextInputProperty
  , roundtripImageProperty
  , roundtripCategoryProperty
  , roundtripEnclosureProperty
  , roundtripSourceProperty
  , roundtripGuidProperty
  , roundtripItemProperty
  ]


skipHoursCase :: TestTree
skipHoursCase = testCase "<skipHours> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssSkipHours
  result @?= [Hour 0, Hour 9, Hour 18, Hour 21]
  where input = [ "<skipHours>"
                , "<hour>21</hour>"
                , "<hour>9</hour>"
                , "<hour>0</hour>"
                , "<hour>18</hour>"
                , "<hour>9</hour>"
                , "</skipHours>"
                ]

skipDaysCase :: TestTree
skipDaysCase = testCase "<skipDays> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssSkipDays
  result @?= [Monday, Saturday, Friday]
  where input = [ "<skipDays>"
                , "<day>Monday</day>"
                , "<day>Monday</day>"
                , "<day>Friday</day>"
                , "<day>Saturday</day>"
                , "</skipDays>"
                ]

textInputCase :: TestTree
textInputCase = testCase "<textInput> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssTextInput
  result^.textInputTitleL @?= "Title"
  result^.textInputDescriptionL @?= "Description"
  result^.textInputNameL @?= "Name"
  result^.textInputLinkL @=? RssURI (URI (Scheme "http") (Just (Authority Nothing (Host "link.ext") Nothing)) "" (Query []) Nothing)
  where input = [ "<textInput>"
                , "<title>Title</title>"
                , "<description>Description</description>"
                , "<name>Name</name>"
                , "<link>http://link.ext</link>"
                , "</textInput>"
                ]

imageCase :: TestTree
imageCase = testCase "<image> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssImage
  result^.imageUriL @?= RssURI (URI (Scheme "http") (Just (Authority Nothing (Host "image.ext") Nothing)) "" (Query []) Nothing)
  result^.imageTitleL @?= "Title"
  result^.imageLinkL @?= RssURI (URI (Scheme "http") (Just (Authority Nothing (Host "link.ext") Nothing)) "" (Query []) Nothing)
  result^.imageWidthL @?= Just 100
  result^.imageHeightL @?= Just 200
  result^.imageDescriptionL @?= "Description"
  where input = [ "<image>"
                , "<uri>http://image.ext</uri>"
                , "<title>Title</title>"
                , "<link>http://link.ext</link>"
                , "<width>100</width>"
                , "<height>200</height>"
                , "<description>Description</description>"
                , "</image>"
                ]

categoryCase :: TestTree
categoryCase = testCase "<category> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssCategory
  result @?= RssCategory "Domain" "Name"
  where input = [ "<category domain=\"Domain\">"
                , "Name"
                , "</category>"
                ]

cloudCase :: TestTree
cloudCase = testCase "<cloud> element" $ do
  (result1, result2) <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser ((,) <$> rssCloud <*> rssCloud)
  result1 @?= RssCloud uri "pingMe" ProtocolSoap
  result2 @?= RssCloud uri "myCloud.rssPleaseNotify" ProtocolXmlRpc
  where input = [ "<cloud domain=\"rpc.sys.com\" port=\"80\" path=\"/RPC2\" registerProcedure=\"pingMe\" protocol=\"soap\"/>"
                , "<cloud domain=\"rpc.sys.com\" port=\"80\" path=\"/RPC2\" registerProcedure=\"myCloud.rssPleaseNotify\" protocol=\"xml-rpc\" />"
                ]
        uri = RssURI (RelativeRef (Just (Authority Nothing (Host "rpc.sys.com") (Just $ Port 80))) "/RPC2" (Query []) Nothing)

guidCase :: TestTree
guidCase = testCase "<guid> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser (some rssGuid)
  result @?= [GuidUri uri, GuidText "1", GuidText "2"]
  where input = [ "<guid isPermaLink=\"true\">//guid.ext</guid>"
                , "<guid isPermaLink=\"false\">1</guid>"
                , "<guid>2</guid>"
                ]
        uri = RssURI (RelativeRef (Just (Authority Nothing (Host "guid.ext") Nothing)) "" (Query []) Nothing)

enclosureCase :: TestTree
enclosureCase = testCase "<enclosure> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssEnclosure
  result @?= RssEnclosure uri 12216320 "audio/mpeg"
  where input = [ "<enclosure url=\"http://www.scripting.com/mp3s/weatherReportSuite.mp3\" length=\"12216320\" type=\"audio/mpeg\" />"
                ]
        uri = RssURI (URI (Scheme "http") (Just (Authority Nothing (Host "www.scripting.com") Nothing)) "/mp3s/weatherReportSuite.mp3" (Query []) Nothing)

sourceCase :: TestTree
sourceCase = testCase "<source> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssSource
  result @?= RssSource uri "Tomalak's Realm"
  where input = [ "<source url=\"http://www.tomalak.org/links2.xml\">Tomalak's Realm</source>"
                ]
        uri = RssURI (URI (Scheme "http") (Just (Authority Nothing (Host "www.tomalak.org") Nothing)) "/links2.xml" (Query []) Nothing)

itemCase :: TestTree
itemCase = testCase "<item> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssItem
  result^.itemTitleL @?= "Example entry"
  result^.itemLinkL @?= Just link
  result^.itemDescriptionL @?= "Here is some text containing an interesting description."
  result^.itemGuidL @?= Just (GuidText "7bd204c6-1655-4c27-aeee-53f933c5395f")
  -- isJust (result^.itemPubDate_) @?= True
  where input = [ "<item>"
                , "<title>Example entry</title>"
                , "<description>Here is some text containing an interesting description.</description>"
                , "<link>http://www.example.com/blog/post/1</link>"
                , "<guid isPermaLink=\"false\">7bd204c6-1655-4c27-aeee-53f933c5395f</guid>"
                , "<pubDate>Sun, 06 Sep 2009 16:20:00 +0000</pubDate>"
                , "<sometag>Some content in unknown tag, should be ignored.</sometag>"
                , "</item>"
                ]
        link = RssURI (URI (Scheme "http") (Just (Authority Nothing (Host "www.example.com") Nothing)) "/blog/post/1" (Query []) Nothing)

documentCase :: TestTree
documentCase = testCase "<rss> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssDocument
  result^.documentVersionL @?= Version [2] []
  result^.channelTitleL @?= "RSS Title"
  result^.channelDescriptionL @?= "This is an example of an RSS feed"
  result^.channelLinkL @?= link
  result^.channelTtlL @?= Just 1800
  length (result^..channelItemsL) @?= 1
  where input = [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                , "<rss version=\"2.0\">"
                , "<channel>"
                , "<title>RSS Title</title>"
                , "<description>This is an example of an RSS feed</description>"
                , "<link>http://www.example.com/main.html</link>"
                , "<lastBuildDate>Mon, 06 Sep 2010 00:01:00 +0000 </lastBuildDate>"
                , "<pubDate>Sun, 06 Sep 2009 16:20:00 +0000</pubDate>"
                , "<ttl>1800</ttl>"
                , "<item>"
                , "<title>Example entry</title>"
                , "<description>Here is some text containing an interesting description.</description>"
                , "<link>http://www.example.com/blog/post/1</link>"
                , "<guid isPermaLink=\"true\">7bd204c6-1655-4c27-aeee-53f933c5395f</guid>"
                , "<pubDate>Sun, 06 Sep 2009 16:20:00 +0000</pubDate>"
                , "</item>"
                , "</channel>"
                , "</rss>"
                ]
        link = RssURI (URI (Scheme "http") (Just (Authority Nothing (Host "www.example.com") Nothing)) "/main.html" (Query []) Nothing)


hlint :: TestTree
hlint = testCase "HLint check" $ do
  result <- HLint.hlint [ "test/", "Text/" ]
  Prelude.null result @?= True


roundtripTextInputProperty :: TestTree
roundtripTextInputProperty = testProperty "parse . render = id (RssTextInput)" $ \t -> either (const False) (t ==) (runIdentity . runCatchT . runConduit $ renderRssTextInput t =$= runConduitParser rssTextInput)

roundtripImageProperty :: TestTree
roundtripImageProperty = testProperty "parse . render = id (RssImage)" $ \t -> either (const False) (t ==) (runIdentity . runCatchT . runConduit $ renderRssImage t =$= runConduitParser rssImage)

roundtripCategoryProperty :: TestTree
roundtripCategoryProperty = testProperty "parse . render = id (RssCategory)" $ \t -> either (const False) (t ==) (runIdentity . runCatchT . runConduit $ renderRssCategory t =$= runConduitParser rssCategory)

roundtripEnclosureProperty :: TestTree
roundtripEnclosureProperty = testProperty "parse . render = id (RssEnclosure)" $ \t -> either (const False) (t ==) (runIdentity . runCatchT . runConduit $ renderRssEnclosure t =$= runConduitParser rssEnclosure)

roundtripSourceProperty :: TestTree
roundtripSourceProperty = testProperty "parse . render = id (RssSource)" $ \t -> either (const False) (t ==) (runIdentity . runCatchT . runConduit $ renderRssSource t =$= runConduitParser rssSource)

roundtripGuidProperty :: TestTree
roundtripGuidProperty = testProperty "parse . render = id (RssGuid)" $ \t -> either (const False) (t ==) (runIdentity . runCatchT . runConduit $ renderRssGuid t =$= runConduitParser rssGuid)

roundtripItemProperty :: TestTree
roundtripItemProperty = testProperty "parse . render = id (RssItem)" $ \t -> either (const False) (t ==) (runIdentity . runCatchT . runConduit $ renderRssItem t =$= runConduitParser rssItem)


letter = choose ('a', 'z')
digit = arbitrary `suchThat` isDigit
alphaNum = oneof [letter, digit]
