{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Servant.ServerSpec where


import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.Wai.Test
import Test.Hspec
import Test.Hspec.Wai

import Servant.API.Get
import Servant.API.Post
import Servant.API.RQBody
import Servant.API.Sub
import Servant.Server


data Person = Person {
  name :: String,
  age :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

alice :: Person
alice = Person "Alice" 42


spec :: Spec
spec = do
  getSpec
  postSpec


type GetApi = Get Person
getApi :: Proxy GetApi
getApi = Proxy

getSpec :: Spec = do
  describe "Servant.API.Get" $ do
    with (return (serve getApi (return alice))) $ do
      it "allows to GET a Person" $ do
        response <- get "/"
        return response `shouldRespondWith` 200
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just alice

      it "throws 404 on POSTs" $ do
        post "/" "" `shouldRespondWith` 404


type PostApi = RQBody Person :> (Post Integer)
postApi :: Proxy PostApi
postApi = Proxy

postSpec :: Spec
postSpec = do
  describe "Servant.API.Post and .RQBody" $ do
    with (return (serve postApi (return . age))) $ do
      it "allows to POST a Person" $ do
        post "/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 201
         }
