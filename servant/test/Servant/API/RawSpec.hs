{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.API.RawSpec where

import Test.Hspec

import Servant.API.Raw

spec :: Spec
spec = describe "Servant.API.Raw" $ do
  describe "unRaw" $ do
    it "unRaw returns proper value" $ do
      let p = Raw "testing" :: Raw String IO
      p `shouldBe` (Raw "testing")
      (unRaw p) `shouldBe` "testing"
