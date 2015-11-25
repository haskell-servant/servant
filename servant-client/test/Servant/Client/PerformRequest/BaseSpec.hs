{-# LANGUAGE OverloadedStrings #-}

module Servant.Client.PerformRequest.BaseSpec where

import           Test.Hspec

import           Servant.Client.PerformRequest.Base

spec :: Spec
spec = do
  describe "parseHeaders" $ do
    it "parses single headers" $ do
      parseHeaders "key: value" `shouldBe` [("key", "value")]

    it "parses multiple headers" $ do
      parseHeaders "foo: bar\r\nnext: yay" `shouldBe`
        [("foo", "bar"), ("next", "yay")]

    it "handles colons in header values correctly" $ do
      parseHeaders "foo: bar:baz" `shouldBe` [("foo", "bar:baz")]
