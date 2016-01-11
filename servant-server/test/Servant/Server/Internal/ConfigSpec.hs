{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Servant.Server.Internal.ConfigSpec (spec) where

import           Data.Proxy                     (Proxy (..))
import           Test.Hspec                     (Spec, describe, it, shouldBe, pending, context)
import           Test.ShouldNotTypecheck        (shouldNotTypecheck)

import           Servant.API
import           Servant.Server.Internal.Config

spec :: Spec
spec = do
  getConfigEntrySpec

getConfigEntrySpec :: Spec
getConfigEntrySpec = describe "getConfigEntry" $ do

  let cfg1 = (0 :: Int) :. EmptyConfig
      cfg2 = (1 :: Int) :. cfg1

  it "gets the config if a matching one exists" $ do
    getConfigEntry (Proxy :: Proxy ()) cfg1 `shouldBe` (0 :: Int)

  it "gets the first matching config" $ do
    getConfigEntry (Proxy :: Proxy ()) cfg2 `shouldBe` (1 :: Int)

  it "allows to distinguish between different config entries with the same type by tag" $ do
    let cfg = 'a' :. (Tag 'b' :: Tagged "second" Char) :. EmptyConfig
    getConfigEntry (Proxy :: Proxy ()) cfg `shouldBe` 'a'
    getConfigEntry (Proxy :: Proxy "second") cfg `shouldBe` 'b'

  it "does not typecheck if type does not exist" $ do
    let x = getConfigEntry (Proxy :: Proxy ()) cfg1 :: Bool
    shouldNotTypecheck x

  it "does not typecheck if tag does not exist" $ do
    let cfg = (Tag 'a' :: Tagged "foo" Char) :. EmptyConfig
        x = getConfigEntry (Proxy :: Proxy "bar") cfg :: Char
    shouldNotTypecheck x

  context "Show instance" $ do
    let cfg = 1 :. 2 :. EmptyConfig
    it "has a Show instance" $ do
      show cfg `shouldBe` "1 :. 2 :. EmptyConfig"

    it "bracketing works" $ do
      show (Just cfg) `shouldBe` "Just (1 :. 2 :. EmptyConfig)"

    it "bracketing works with operators" $ do
      let cfg = (1 :. 'a' :. EmptyConfig) :<|> ('b' :. True :. EmptyConfig)
      show cfg `shouldBe` "(1 :. 'a' :. EmptyConfig) :<|> ('b' :. True :. EmptyConfig)"
