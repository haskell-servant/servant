{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.FailSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Data.Monoid ()
import qualified Network.HTTP.Types                   as HTTP
import           Test.Hspec

import           Servant.API
                 ((:<|>) ((:<|>)))
import           Servant.Client
import           Servant.ClientTestUtils

spec :: Spec
spec = describe "Servant.FailSpec" $ do
    failSpec

failSpec :: Spec
failSpec = beforeAll (startWaiApp failServer) $ afterAll endWaiApp $ do

    context "client returns errors appropriately" $ do
      it "reports FailureResponse" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> getDeleteEmpty :<|> _) = client api
        Left res <- runClient getDeleteEmpty baseUrl
        case res of
          FailureResponse _ r | responseStatusCode r == HTTP.status404 -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> _ :<|> getCapture :<|> _) = client api
        Left res <- runClient (getCapture "foo") baseUrl
        case res of
          DecodeFailure _ _ -> return ()
          _ -> fail $ "expected DecodeFailure, but got " <> show res

      it "reports ConnectionError" $ \_ -> do
        let (getGetWrongHost :<|> _) = client api
        Left res <- runClient getGetWrongHost (BaseUrl Http "127.0.0.1" 19872 "")
        case res of
          ConnectionError _ -> return ()
          _ -> fail $ "expected ConnectionError, but got " <> show res

      it "reports UnsupportedContentType" $ \(_, baseUrl) -> do
        let (_ :<|> getGet :<|> _ ) = client api
        Left res <- runClient getGet baseUrl
        case res of
          UnsupportedContentType "application/octet-stream" _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports UnsupportedContentType when there are response headers" $ \(_, baseUrl) -> do
        Left res <- runClient getRespHeaders baseUrl
        case res of
          UnsupportedContentType "application/x-www-form-urlencoded" _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> getBody :<|> _) = client api
        Left res <- runClient (getBody alice) baseUrl
        case res of
          InvalidContentTypeHeader _ -> return ()
          _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res
