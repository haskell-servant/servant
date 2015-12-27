{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Servant.Server.Internal.ConfigSpec (spec) where

import           Data.Proxy                     (Proxy (..))
import           Test.Hspec                     (Spec, describe, it, shouldBe)
import           Test.ShouldNotTypecheck        (shouldNotTypecheck)

import           Servant.Server.Internal.Config

spec :: Spec
spec = do
  getConfigEntrySpec

getConfigEntrySpec :: Spec
getConfigEntrySpec = describe "getConfigEntry" $ do

  let cfg1 = 0 .: EmptyConfig :: Config '[ConfigEntry "a" Int]
      cfg2 = 1 .: cfg1 :: Config '[ConfigEntry "a" Int, ConfigEntry "a" Int]

  it "gets the config if a matching one exists" $ do

    getConfigEntry (Proxy :: Proxy "a") cfg1 `shouldBe` 0

  it "gets the first matching config" $ do

    getConfigEntry (Proxy :: Proxy "a") cfg2 `shouldBe` 1

  it "does not typecheck if key does not exist" $ do

    let x = getConfigEntry (Proxy :: Proxy "b") cfg1 :: Int
    shouldNotTypecheck x

  it "does not typecheck if key maps to a different type" $ do

    let x = getConfigEntry (Proxy :: Proxy "a") cfg1 :: String
    shouldNotTypecheck x
