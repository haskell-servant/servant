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

  let cfg1 = 0 .:. EmptyConfig :: Config '[ConfigEntry "a" Int]
      cfg2 = 1 .:. cfg1 :: Config '[ConfigEntry "a" Int, ConfigEntry "a" Int]

  it "gets the config if a matching one exists" $ do

    getConfigEntry (Proxy :: Proxy "a") cfg1 `shouldBe` 0

  it "gets the first matching config" $ do

    getConfigEntry (Proxy :: Proxy "a") cfg2 `shouldBe` 1

  it "allows to distinguish between different config entries with the same type by tag" $ do
    let cfg = 'a' .:. 'b' .:. EmptyConfig :: Config '[ConfigEntry 1 Char, ConfigEntry 2 Char]
    getConfigEntry (Proxy :: Proxy 1) cfg `shouldBe` 'a'

  context "Show instance" $ do
    let cfg = 1 .:. 2 .:. EmptyConfig
    it "has a Show instance" $ do
      show cfg `shouldBe` "1 .:. 2 .:. EmptyConfig"

    it "bracketing works" $ do
      show (Just cfg) `shouldBe` "Just (1 .:. 2 .:. EmptyConfig)"

    it "bracketing works with operators" $ do
      let cfg = (1 .:. 'a' .:. EmptyConfig) :<|> ('b' .:. True .:. EmptyConfig)
      show cfg `shouldBe` "(1 .:. 'a' .:. EmptyConfig) :<|> ('b' .:. True .:. EmptyConfig)"

  it "does not typecheck if key does not exist" $ do
    let x = getConfigEntry (Proxy :: Proxy "b") cfg1 :: Int
    shouldNotTypecheck x

  it "does not typecheck if key maps to a different type" $ do
    let x = getConfigEntry (Proxy :: Proxy "a") cfg1 :: String
    shouldNotTypecheck x
