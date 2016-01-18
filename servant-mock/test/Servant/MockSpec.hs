{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.MockSpec where

import           Data.Aeson as Aeson
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai
import           Test.QuickCheck

import           Servant
import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Mock

_ = mock comprehensiveAPI

data Body
  = Body
  | ArbitraryBody
  deriving (Generic, ToJSON)

instance Arbitrary Body where
  arbitrary = return ArbitraryBody

spec :: Spec
spec = do
  describe "mock" $ do
    context "Get" $ do
      let api :: Proxy (Get '[JSON] Body)
          api = Proxy
          app = serve api (mock api)
      with (return app) $ do
        it "serves arbitrary response bodies" $ do
          get "/" `shouldRespondWith` 200{
            matchBody = Just $ Aeson.encode ArbitraryBody
          }
