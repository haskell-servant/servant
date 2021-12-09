{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE TypeOperators               #-}
{-# OPTIONS_GHC -freduction-depth=100    #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.BrokenSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Data.Monoid ()
import           Data.Proxy
import qualified Network.HTTP.Types as HTTP
import           Test.Hspec

import           Servant.API
                 ((:<|>) ((:<|>)), (:>), JSON, Verb, Get, StdMethod (GET))
import           Servant.Client
import           Servant.ClientTestUtils
import           Servant.Server

-- * api for testing inconsistencies between client and server

type Get201 = Verb 'GET 201
type Get301 = Verb 'GET 301

type BrokenAPI =
  -- the server should respond with 200, but returns 201
       "get200" :> Get201 '[JSON] ()
  -- the server should respond with 307, but returns 301
  :<|> "get307" :> Get301 '[JSON] ()

brokenApi :: Proxy BrokenAPI
brokenApi = Proxy

brokenServer :: Application
brokenServer = serve brokenApi (pure () :<|> pure ())

type PublicAPI =
  -- the client expects 200
       "get200" :> Get '[JSON] ()
  -- the client expects 307
  :<|> "get307" :> Get307 '[JSON] ()

publicApi :: Proxy PublicAPI
publicApi = Proxy

get200Client :: ClientM ()
get307Client :: ClientM ()
get200Client :<|> get307Client = client publicApi


spec :: Spec
spec = describe "Servant.BrokenSpec" $ do
    brokenSpec

brokenSpec :: Spec
brokenSpec = beforeAll (startWaiApp brokenServer) $ afterAll endWaiApp $ do
    context "client returns errors for inconsistencies between client and server api" $ do
      it "reports FailureResponse with wrong 2xx status code" $ \(_, baseUrl) -> do
        res <- runClient get200Client baseUrl
        case res of
          Left (FailureResponse _ r) | responseStatusCode r == HTTP.status201 -> return ()
          _ -> fail $ "expected 201 broken response, but got " <> show res

      it "reports FailureResponse with wrong 3xx status code" $ \(_, baseUrl) -> do
        res <- runClient get307Client baseUrl
        case res of
          Left (FailureResponse _ r) | responseStatusCode r == HTTP.status301 -> return ()
          _ -> fail $ "expected 301 broken response, but got " <> show res
