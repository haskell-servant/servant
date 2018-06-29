{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.API.ResponseHeadersSpec where

import           Test.Hspec

import           Servant.API.Header
import           Servant.API.ResponseHeaders

spec :: Spec
spec = describe "Servant.API.ResponseHeaders" $ do
  describe "addHeader" $ do

    it "adds a header to a value" $ do
      let val = addHeader "hi" 5 :: Headers '[Header "test" String] Int
      getHeaders val `shouldBe` [("test", "hi")]

    it "maintains the value" $ do
      let val = addHeader "hi" 5 :: Headers '[Header "test" String] Int
      getResponse val `shouldBe` 5

    it "adds headers to the front of the list" $ do
      let val = addHeader 10 $ addHeader "b" 5 :: Headers '[Header "first" Int, Header "second" String] Int
      getHeaders val `shouldBe` [("first", "10"), ("second", "b")]

  describe "noHeader" $ do

    it "does not add a header" $ do
      let val = noHeader 5 :: Headers '[Header "test" Int] Int
      getHeaders val `shouldBe` []
