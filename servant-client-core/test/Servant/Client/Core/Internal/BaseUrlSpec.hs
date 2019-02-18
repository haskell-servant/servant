{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Client.Core.Internal.BaseUrlSpec (spec) where


import           Control.DeepSeq
import           Prelude ()
import           Prelude.Compat
import           Test.Hspec
import           Test.QuickCheck

import           Servant.Client.Core.BaseUrl

spec :: Spec
spec = do
  let parse = parseBaseUrl :: String -> Maybe BaseUrl
  describe "showBaseUrl" $ do
    it "shows a BaseUrl" $ do
      showBaseUrl (BaseUrl Http "foo.com" 80 "") `shouldBe` "http://foo.com"
    it "shows a https BaseUrl" $ do
      showBaseUrl (BaseUrl Https "foo.com" 443 "") `shouldBe` "https://foo.com"
    it "shows the path of a BaseUrl" $ do
      showBaseUrl (BaseUrl Http "foo.com" 80 "api") `shouldBe` "http://foo.com/api"
    it "shows the path of an https BaseUrl" $ do
      showBaseUrl (BaseUrl Https "foo.com" 443 "api") `shouldBe` "https://foo.com/api"
    it "handles leading slashes in path" $ do
      showBaseUrl (BaseUrl Https "foo.com" 443 "/api") `shouldBe` "https://foo.com/api"

  describe "httpBaseUrl" $ do
    it "allows to construct default http BaseUrls" $ do
      BaseUrl Http "bar" 80 "" `shouldBe` BaseUrl Http "bar" 80 ""

  describe "parseBaseUrl" $ do
    it "is total" $ do
      property $ \ string ->
        deepseq (fmap show (parse string )) True

    it "is the inverse of showBaseUrl" $ do
      property $ \ baseUrl -> counterexample (showBaseUrl baseUrl) $
        parse (showBaseUrl baseUrl) === Just baseUrl

    context "trailing slashes" $ do
      it "allows trailing slashes" $ do
        parse "foo.com/" `shouldBe` Just (BaseUrl Http "foo.com" 80 "")

      it "allows trailing slashes in paths" $ do
        parse "foo.com/api/" `shouldBe` Just (BaseUrl Http "foo.com" 80 "api")

    context "urls without scheme" $ do
      it "assumes http" $ do
        parse "foo.com" `shouldBe` Just (BaseUrl Http "foo.com" 80 "")

      it "allows port numbers" $ do
        parse "foo.com:8080" `shouldBe` Just (BaseUrl Http "foo.com" 8080 "")

    it "can parse paths" $ do
      parse "http://foo.com/api" `shouldBe` Just (BaseUrl Http "foo.com" 80 "api")

    it "rejects ftp urls" $ do
      parse "ftp://foo.com" `shouldBe` Nothing

instance Arbitrary BaseUrl where
  arbitrary = BaseUrl <$>
    elements [Http, Https] <*>
    hostNameGen <*>
    portGen <*>
    pathGen
   where
    letters = ['a' .. 'z'] ++ ['A' .. 'Z']
    -- this does not perfectly mirror the url standard, but I hope it's good
    -- enough.
    hostNameGen = do
      first <- elements letters
      middle <- listOf1 $ elements (letters ++ ['0' .. '9'] ++ ['.', '-'])
      last' <- elements letters
      return (first : middle ++ [last'])
    portGen = frequency $
      (1, return 80) :
      (1, return 443) :
      (1, choose (1, 20000)) :
      []
    pathGen = listOf1 . elements $ letters
