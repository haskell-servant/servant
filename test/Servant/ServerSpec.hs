{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.ServerSpec where


import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.Wai.Test
import Test.Hspec
import Test.Hspec.Wai

import Servant.API.Get
import Servant.Server


data Person = Person {
  name :: String,
  age :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

alice :: Person
alice = Person "Alice" 103


spec :: Spec
spec = do
  getSpec


type GetApi = Get Person
getApi :: Proxy GetApi
getApi = Proxy

getSpec = do
  describe "Servant.API.Get" $ do
    with (return (serve getApi (return alice))) $ do
      it "serves a Person" $ do
        response <- get "/"
        return response `shouldRespondWith` 200
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just alice
