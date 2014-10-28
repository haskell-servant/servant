{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Servant.ServerSpec where


import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import Data.String
import Data.String.Conversions
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.Hspec
import Test.Hspec.Wai

import Servant.API.Capture
import Servant.API.Get
import Servant.API.GetParam
import Servant.API.Post
import Servant.API.Raw
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

tweety :: Animal
tweety = Animal "Bird" 2


-- * specs

spec :: Spec
spec = do
  captureSpec
  getSpec
  getParamSpec
  postSpec
  rawSpec
  unionSpec


type CaptureApi = Capture "legs" Integer :> Get Animal
captureApi :: Proxy CaptureApi
captureApi = Proxy
captureServer :: Integer -> EitherT (Int, String) IO Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> left (404, "not found")

captureSpec :: Spec
captureSpec = do
  describe "Servant.API.Capture" $ do
    with (return (serve captureApi captureServer)) $ do
      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just tweety

    with (return (serve
        (Proxy :: Proxy (Capture "captured" String :> Raw))
        (\ "captured" request respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request)))) $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" `shouldRespondWith` (fromString (show ["foo" :: String]))


type GetApi = Get Person
getApi :: Proxy GetApi
getApi = Proxy

getSpec :: Spec
getSpec = do
  describe "Servant.API.Get" $ do
    with (return (serve getApi (return alice))) $ do
      it "allows to GET a Person" $ do
        response <- get "/"
        return response `shouldRespondWith` 200
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just alice

      it "throws 405 (wrong method) on POSTs" $ do
        post "/" "" `shouldRespondWith` 405


type GetParamApi = GetParam "name" String :> Get Person
getParamApi :: Proxy GetParamApi
getParamApi = Proxy

getParamServer :: Server GetParamApi
getParamServer (Just name) = return alice{name = name}
getParamServer Nothing = return alice

getParamSpec :: Spec
getParamSpec = do
  describe "Servant.API.GetParam" $ do
    it "allows to retrieve GET parameters" $ do
      (flip runSession) (serve getParamApi getParamServer) $ do
        let params = "?name=bob"
        response <- Network.Wai.Test.request defaultRequest{
          rawQueryString = params,
          queryString = parseQuery params
         }
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just alice{
            name = "bob"
           }


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


type RawApi = "foo" :> Raw
rawApi :: Proxy RawApi
rawApi = Proxy
rawApplication :: Show a => (Request -> a) -> Application
rawApplication f request respond = respond $ responseLBS ok200 [] (cs $ show $ f request)

rawSpec :: Spec
rawSpec = do
  describe "Servant.API.Raw" $ do
    it "runs applications" $ do
      (flip runSession) (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo"]
         }
        liftIO $ do
          simpleBody response `shouldBe` "42"

    it "gets the pathInfo modified" $ do
      (flip runSession) (serve rawApi (rawApplication pathInfo)) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo", "bar"]
         }
        liftIO $ do
          simpleBody response `shouldBe` cs (show ["bar" :: String])


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
