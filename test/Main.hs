{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens.Getter
import           Control.Monad.Trans.Resource

import           Data.Conduit
import           Data.Conduit.List
import           Data.Conduit.Parser
import           Data.Conduit.Parser.XML      as XML
import           Data.Default
import           Data.Version

import           Network.URI

import qualified Language.Haskell.HLint       as HLint (hlint)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.RSS.Conduit.Parse       as Parser
-- import           Text.RSS.Conduit.Render     as Renderer
import           Text.Parser.Combinators
import           Text.RSS.Types


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  -- , properties
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
  result^.textInputTitle_ @?= "Title"
  result^.textInputDescription_ @?= "Description"
  result^.textInputName_ @?= "Name"
  show (result^.textInputLink_) @=? "http://link.ext"
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
  show (result^.imageUri_) @?= "http://image.ext"
  result^.imageTitle_ @?= "Title"
  show (result^.imageLink_) @?= "http://link.ext"
  result^.imageWidth_ @?= Just 100
  result^.imageHeight_ @?= Just 200
  result^.imageDescription_ @?= "Description"
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
        Just uri = parseURIReference "rpc.sys.com:80/RPC2"

guidCase :: TestTree
guidCase = testCase "<guid> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser (some rssGuid)
  result @?= [GuidUri uri, GuidText "1", GuidText "2"]
  where input = [ "<guid isPermaLink=\"true\">guid.ext</guid>"
                , "<guid isPermaLink=\"false\">1</guid>"
                , "<guid>2</guid>"
                ]
        Just uri = parseURIReference "guid.ext"

enclosureCase :: TestTree
enclosureCase = testCase "<enclosure> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssEnclosure
  result @?= RssEnclosure uri 12216320 "audio/mpeg"
  where input = [ "<enclosure url=\"http://www.scripting.com/mp3s/weatherReportSuite.mp3\" length=\"12216320\" type=\"audio/mpeg\" />"
                ]
        Just uri = parseURIReference "http://www.scripting.com/mp3s/weatherReportSuite.mp3"

sourceCase :: TestTree
sourceCase = testCase "<source> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssSource
  result @?= RssSource uri "Tomalak's Realm"
  where input = [ "<source url=\"http://www.tomalak.org/links2.xml\">Tomalak's Realm</source>"
                ]
        Just uri = parseURIReference "http://www.tomalak.org/links2.xml"

itemCase :: TestTree
itemCase = testCase "<item> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssItem
  result^.itemTitle_ @?= "Example entry"
  result^.itemLink_ @?= Just link
  result^.itemDescription_ @?= "Here is some text containing an interesting description."
  result^.itemGuid_ @?= Just (GuidUri guid)
  -- isJust (result^.itemPubDate_) @?= True
  where input = [ "<item>"
                , "<title>Example entry</title>"
                , "<description>Here is some text containing an interesting description.</description>"
                , "<link>http://www.example.com/blog/post/1</link>"
                , "<guid isPermaLink=\"true\">7bd204c6-1655-4c27-aeee-53f933c5395f</guid>"
                , "<pubDate>Sun, 06 Sep 2009 16:20:00 +0000</pubDate>"
                , "</item>"
                ]
        Just link = parseURIReference "http://www.example.com/blog/post/1"
        Just guid = parseURIReference "7bd204c6-1655-4c27-aeee-53f933c5395f"

documentCase :: TestTree
documentCase = testCase "<rss> element" $ do
  result <- runResourceT . runConduit $ sourceList input =$= XML.parseText def =$= runConduitParser rssDocument
  result^.documentVersion_ @?= Version [2] []
  result^.channelTitle_ @?= "RSS Title"
  result^.channelDescription_ @?= "This is an example of an RSS feed"
  result^.channelLink_ @?= link
  result^.channelTtl_ @?= Just 1800
  length (result^.channelItems_) @?= 1
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
        Just link = parseURIReference "http://www.example.com/main.html"


hlint :: TestTree
hlint = testCase "HLint check" $ do
  result <- HLint.hlint [ "test/", "Text/" ]
  Prelude.null result @?= True
