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
      getConfigEntry (Proxy :: Proxy ()) config `shouldBe` 'a'

    it "gets the first matching config" $ do
      let config = 'a' :. 'b' :. EmptyConfig
      getConfigEntry (Proxy :: Proxy ()) config `shouldBe` 'a'

    it "allows to distinguish between different config entries with the same type by tag" $ do
      let config = 'a' :. (Tag 'b' :: Tagged "second" Char) :. EmptyConfig
      getConfigEntry (Proxy :: Proxy ()) config `shouldBe` 'a'
      getConfigEntry (Proxy :: Proxy "second") config `shouldBe` 'b'

    it "does not typecheck if type does not exist" $ do
      let config = 'a' :. EmptyConfig
          x = getConfigEntry (Proxy :: Proxy ()) config :: Bool
      shouldNotTypecheck x

    it "does not typecheck if tag does not exist" $ do
      let config = (Tag 'a' :: Tagged "foo" Char) :. EmptyConfig
          x = getConfigEntry (Proxy :: Proxy "bar") config :: Char
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
