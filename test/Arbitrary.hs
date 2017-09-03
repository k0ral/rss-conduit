{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
-- | 'Arbitrary' instances used by RSS types.
module Arbitrary (module Arbitrary) where

-- {{{ Imports
import           Data.ByteString           (ByteString)
import           Data.Char
import           Data.Maybe
import           Data.MonoTraversable      (Element)
import           Data.NonNull
import           Data.Sequences            (SemiSequence)
import           Data.Text                 (Text, find, pack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Version
import           Data.Vinyl.Core

import           GHC.Generics

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Text.RSS.Types

import           URI.ByteString
-- }}}


-- | Reasonable enough 'URI' generator.
instance Arbitrary (URIRef Absolute) where
  arbitrary = URI <$> arbitrary <*> arbitrary <*> genPath <*> arbitrary <*> (Just <$> genFragment)
  shrink (URI a b c d e) = URI <$> shrink a <*> shrink b <*> shrink c <*> shrink d <*> shrink e

-- | Reasonable enough 'RelativeRef' generator.
instance Arbitrary (URIRef Relative) where
  arbitrary = RelativeRef <$> arbitrary <*> genPath <*> arbitrary <*> (Just <$> genFragment)
  shrink (RelativeRef a b c d) = RelativeRef <$> shrink a <*> shrink b <*> shrink c <*> shrink d

-- | Reasonable enough 'Authority' generator.
instance Arbitrary Authority where
  arbitrary = Authority <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

genFragment :: Gen ByteString
genFragment = encodeUtf8 . pack <$> listOf1 genAlphaNum

instance Arbitrary Host where
  arbitrary = Host . encodeUtf8 . pack <$> listOf1 genAlphaNum
  shrink = genericShrink

genPath :: Gen ByteString
genPath = encodeUtf8 . pack . ("/" ++) <$> listOf1 genAlphaNum

instance Arbitrary Port where
  arbitrary = do
    Positive port <- arbitrary
    return $ Port port

instance Arbitrary Query where
  arbitrary = do
    a <- listOf1 (encodeUtf8 . pack <$> listOf1 genAlphaNum)
    b <- listOf1 (encodeUtf8 . pack <$> listOf1 genAlphaNum)
    return $ Query $ Prelude.zip a b
  shrink = genericShrink

instance Arbitrary Scheme where
  arbitrary = Scheme . encodeUtf8 . pack <$> listOf1 (choose('a', 'z'))
  shrink = genericShrink

instance Arbitrary UserInfo where
  arbitrary = do
    a <- encodeUtf8 . pack <$> listOf1 genAlphaNum
    b <- encodeUtf8 . pack <$> listOf1 genAlphaNum
    return $ UserInfo a b
  shrink = genericShrink


instance Arbitrary RssCategory where
  arbitrary = RssCategory <$> (pack <$> listOf genAlphaNum) <*> (pack <$> listOf genAlphaNum)

instance Arbitrary CloudProtocol where
  arbitrary = oneof $ map pure [ProtocolXmlRpc, ProtocolSoap, ProtocolHttpPost]

instance Arbitrary RssCloud where
  arbitrary = RssCloud <$> arbitrary <*> (pack <$> listOf genAlphaNum) <*> arbitrary

instance Arbitrary RssEnclosure where
  arbitrary = do
    Positive l <- arbitrary
    RssEnclosure <$> arbitrary <*> pure l <*> (pack <$> listOf genAlphaNum)

instance Arbitrary RssGuid where
  arbitrary = oneof [GuidText <$> (pack <$> listOf genAlphaNum), GuidUri <$> arbitrary]

instance Arbitrary RssImage where
  arbitrary = RssImage <$> arbitrary <*> (pack <$> listOf genAlphaNum) <*> arbitrary <*> fmap (fmap abs) arbitrary <*> fmap (fmap abs) arbitrary <*> (pack <$> listOf genAlphaNum)

instance Arbitrary (RssItem '[]) where
  arbitrary = RssItem
    <$> (pack <$> listOf genAlphaNum)
    <*> arbitrary
    <*> (pack <$> listOf genAlphaNum)
    <*> (pack <$> listOf genAlphaNum)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> oneof [Just <$> genTime, pure Nothing]
    <*> arbitrary
    <*> pure (RssItemExtensions RNil)

instance Arbitrary RssSource where
  arbitrary = RssSource <$> arbitrary <*> (pack <$> listOf genAlphaNum)

instance Arbitrary RssTextInput where
  arbitrary = RssTextInput <$> (pack <$> listOf genAlphaNum) <*> (pack <$> listOf genAlphaNum) <*> (pack <$> listOf genAlphaNum) <*> arbitrary

instance Arbitrary (RssDocument '[]) where
  arbitrary = RssDocument
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> vectorOf 1 arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> oneof [Just <$> genTime, pure Nothing]
    <*> oneof [Just <$> genTime, pure Nothing]
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure (RssChannelExtensions RNil)

instance Arbitrary Day where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance Arbitrary Hour where
  arbitrary = Hour <$> suchThat arbitrary (\x -> x >= 0 && x < 24)

-- | Alpha-numeric generator.
genAlphaNum :: Gen Char
genAlphaNum = oneof [choose('a', 'z'), arbitrary `suchThat` isDigit]

-- | Generates 'UTCTime' with rounded seconds.
genTime :: Gen UTCTime
genTime = do
  (UTCTime d s) <- arbitrary
  return $ UTCTime d $ fromIntegral (round s :: Int)

instance Arbitrary RssURI where
  arbitrary = oneof [RssURI <$> (arbitrary :: Gen (URIRef Absolute)), RssURI <$> (arbitrary :: Gen (URIRef Relative))]
  shrink (RssURI a@URI{})         = RssURI <$> shrink a
  shrink (RssURI a@RelativeRef{}) = RssURI <$> shrink a
