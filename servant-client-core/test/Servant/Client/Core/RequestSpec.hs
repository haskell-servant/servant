{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Client.Core.RequestSpec (spec) where


import           Prelude ()
import           Prelude.Compat
import           Control.Monad
import           Data.List (isInfixOf)
import           Servant.Client.Core.Request
import           Test.Hspec

spec :: Spec
spec = do
  describe "Request" $ do
    describe "show" $ do
      it "redacts the authorization header" $ do
        let request = void $ defaultRequest { requestHeaders = pure ("authorization", "secret") }
        isInfixOf "secret" (show request) `shouldBe` False
