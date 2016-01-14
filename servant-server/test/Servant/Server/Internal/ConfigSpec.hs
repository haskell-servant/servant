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
  describe "getConfigEntry" $ do
    it "gets the config if a matching one exists" $ do
      let config = 'a' :. EmptyConfig
      getConfigEntry config `shouldBe` 'a'

    it "gets the first matching config" $ do
      let config = 'a' :. 'b' :. EmptyConfig
      getConfigEntry config `shouldBe` 'a'

    it "does not typecheck if type does not exist" $ do
      let config = 'a' :. EmptyConfig
          x = getConfigEntry config :: Bool
      shouldNotTypecheck x

    context "Show instance" $ do
      let config = 'a' :. True :. EmptyConfig
      it "has a Show instance" $ do
        show config `shouldBe` "'a' :. True :. EmptyConfig"

      context "bracketing" $ do
        it "works" $ do
          show (Just config) `shouldBe` "Just ('a' :. True :. EmptyConfig)"

        it "works with operators" $ do
          let config = (1 :. 'a' :. EmptyConfig) :<|> ('b' :. True :. EmptyConfig)
          show config `shouldBe` "(1 :. 'a' :. EmptyConfig) :<|> ('b' :. True :. EmptyConfig)"

  describe "descendIntoNamedConfig" $ do
    let config :: Config [Char, NamedConfig "sub" '[Char]]
        config =
          'a' :.
          (NamedConfig subConfig :: NamedConfig "sub" '[Char])
          :. EmptyConfig
        subConfig = 'b' :. EmptyConfig
    it "allows extracting subconfigs" $ do
      descendIntoNamedConfig (Proxy :: Proxy "sub") config `shouldBe` subConfig

    it "allows extracting entries from subconfigs" $ do
      getConfigEntry (descendIntoNamedConfig (Proxy :: Proxy "sub") config :: Config '[Char])
        `shouldBe` 'b'

    it "does not typecheck if subConfig has the wrong type" $ do
      let x = descendIntoNamedConfig (Proxy :: Proxy "sub") config :: Config '[Int]
      shouldNotTypecheck (show x)

    it "does not typecheck if subConfig with that name doesn't exist" $ do
      let x = descendIntoNamedConfig (Proxy :: Proxy "foo") config :: Config '[Char]
      shouldNotTypecheck (show x)
