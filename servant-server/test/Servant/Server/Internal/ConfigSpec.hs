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

newtype Wrapped a = Wrap { unwrap :: a }

getConfigEntrySpec :: Spec
getConfigEntrySpec = describe "getConfigEntry" $ do

  let cfg1 = 0 :. EmptyConfig :: Config '[Int]
      cfg2 = 1 :. cfg1 :: Config '[Int, Int]

  it "gets the config if a matching one exists" $ do

    getConfigEntry cfg1 `shouldBe` (0 :: Int)

  it "gets the first matching config" $ do

    getConfigEntry cfg2 `shouldBe` (1 :: Int)

  it "allows to distinguish between different config entries with the same type by tag" $ do
    let cfg = 'a' :. Wrap 'b' :. EmptyConfig :: Config '[Char, Wrapped Char]
    getConfigEntry cfg `shouldBe` 'a'

  context "Show instance" $ do
    let cfg = 1 :. 2 :. EmptyConfig
    it "has a Show instance" $ do
      show cfg `shouldBe` "1 :. 2 :. EmptyConfig"

    it "bracketing works" $ do
      show (Just cfg) `shouldBe` "Just (1 :. 2 :. EmptyConfig)"

    it "bracketing works with operators" $ do
      let cfg = (1 :. 'a' :. EmptyConfig) :<|> ('b' :. True :. EmptyConfig)
      show cfg `shouldBe` "(1 :. 'a' :. EmptyConfig) :<|> ('b' :. True :. EmptyConfig)"

  it "does not typecheck if type does not exist" $ do

    let x = getConfigEntry cfg1 :: Bool
    shouldNotTypecheck x
