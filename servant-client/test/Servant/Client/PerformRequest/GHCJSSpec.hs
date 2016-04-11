{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Client.PerformRequest.GHCJSSpec where

import           Test.Hspec

#ifdef __GHCJS__

import           Servant.Client.PerformRequest.GHCJS

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

#else

spec :: Spec
spec = return ()
#endif
