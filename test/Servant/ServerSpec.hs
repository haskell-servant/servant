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
import Test.Hspec.Wai as Wai

import Servant.API.Get
import Servant.API.Post
import Servant.API.RQBody
import Servant.API.Sub
import Servant.API.Union
import Servant.Server


-- * test data types

data Person = Person {
  name :: String,
  age :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

alice :: Person
alice = Person "Alice" 42

data Animal = Animal {
  species :: String,
  numberOfLegs :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Animal
instance FromJSON Animal

jerry :: Animal
jerry = Animal "Mouse" 4


-- * specs

spec :: Spec
spec = do
  getSpec
  postSpec
  unionSpec


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


type UnionApi =
       "foo" :> Get Person
  :<|> "bar" :> Get Animal
unionApi :: Proxy UnionApi
unionApi = Proxy

unionServer :: Server UnionApi
unionServer =
       return alice
  :<|> return jerry

unionSpec :: Spec
unionSpec = do
  describe "Servant.API.Union" $ do
    with (return $ serve unionApi unionServer) $ do
      it "unions endpoints" $ do
        response <- get "/foo"
        liftIO $ do
          decode' (simpleBody response) `shouldBe`
            Just alice
        response <- get "/bar"
        liftIO $ do
          decode' (simpleBody response) `shouldBe`
            Just jerry
