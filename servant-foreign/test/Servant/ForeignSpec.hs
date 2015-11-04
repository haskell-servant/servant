{-# LANGUAGE OverloadedStrings     #-}

module Servant.ForeignSpec where

import Servant.Foreign (camelCase)

import Test.Hspec

spec :: Spec
spec = describe "Servant.Foreign" $ do
  camelCaseSpec

camelCaseSpec :: Spec
camelCaseSpec = describe "camelCase" $ do
  it "converts FunctionNames to camelCase" $ do
    camelCase ["post", "counter", "inc"] `shouldBe` "postCounterInc"
    camelCase ["get", "hyphen-ated", "counter"] `shouldBe` "getHyphenatedCounter"
