{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Common.BaseUrlSpec where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.DeepSeq
import           Test.Hspec
import           Test.QuickCheck

import           Servant.Common.BaseUrl

spec :: Spec
spec = do
  describe "showBaseUrl" $ do
    it "shows a BaseUrl" $ do
      showBaseUrl (BaseUrl Http "foo.com" 80) `shouldBe` "http://foo.com"

    it "shows a https BaseUrl" $ do
      showBaseUrl (BaseUrl Https "foo.com" 443) `shouldBe` "https://foo.com"

  describe "httpBaseUrl" $ do
    it "allows to construct default http BaseUrls" $ do
      BaseUrl Http "bar" 80 `shouldBe` BaseUrl Http "bar" 80

  describe "parseBaseUrl" $ do
    it "is total" $ do
      property $ \ string ->
        deepseq (fmap show (parseBaseUrl string)) True

    it "is the inverse of showBaseUrl" $ do
      property $ \ baseUrl ->
        counterexample (showBaseUrl baseUrl) $
        parseBaseUrl (showBaseUrl baseUrl) ===
          Right baseUrl

    it "allows trailing slashes" $ do
      parseBaseUrl "foo.com/" `shouldBe` Right (BaseUrl Http "foo.com" 80)

    context "urls without scheme" $ do
      it "assumes http" $ do
        parseBaseUrl "foo.com" `shouldBe` Right (BaseUrl Http "foo.com" 80)

      it "allows port numbers" $ do
        parseBaseUrl "foo.com:8080" `shouldBe` Right (BaseUrl Http "foo.com" 8080)

    it "rejects ftp urls" $ do
      parseBaseUrl "ftp://foo.com" `shouldSatisfy` isLeft

instance Arbitrary BaseUrl where
  arbitrary = BaseUrl <$>
    elements [Http, Https] <*>
    hostNameGen <*>
    portGen
   where
    -- this does not perfectly mirror the url standard, but I hope it's good
    -- enough.
    hostNameGen = do
      let letters = ['a' .. 'z'] ++ ['A' .. 'Z']
      first <- elements letters
      middle <- listOf1 $ elements (letters ++ ['0' .. '9'] ++ ['.', '-'])
      last <- elements letters
      return (first : middle ++ [last])
    portGen = frequency $
      (1, return 80) :
      (1, return 443) :
      (1, choose (1, 20000)) :
      []

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
