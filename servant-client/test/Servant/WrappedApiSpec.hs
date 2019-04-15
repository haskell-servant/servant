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

module Servant.WrappedApiSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
                 (bracket)
import           Control.Monad.Error.Class
                 (throwError)
import           Data.Monoid ()
import           Data.Proxy
import qualified Network.HTTP.Types                   as HTTP
import           Test.Hspec

import           Servant.API
                 (Delete, Get, JSON, Post, Put)
import           Servant.Client
import           Servant.Server
import           Servant.ClientTestUtils

spec :: Spec
spec = describe "Servant.WrappedApiSpec" $ do
    wrappedApiSpec

data WrappedApi where
  WrappedApi :: (HasServer (api :: *) '[], Server api ~ Handler a,
                 HasClient ClientM api, Client ClientM api ~ ClientM ()) =>
    Proxy api -> WrappedApi

wrappedApiSpec :: Spec
wrappedApiSpec = describe "error status codes" $ do
  let serveW api = serve api $ throwError $ ServerError 500 "error message" "" []
  context "are correctly handled by the client" $
    let test :: (WrappedApi, String) -> Spec
        test (WrappedApi api, desc) =
          it desc $ bracket (startWaiApp $ serveW api) endWaiApp $ \(_, baseUrl) -> do
            let getResponse :: ClientM ()
                getResponse = client api
            Left (FailureResponse _ r) <- runClient getResponse baseUrl
            responseStatusCode r `shouldBe` HTTP.Status 500 "error message"
    in mapM_ test $
        (WrappedApi (Proxy :: Proxy (Delete '[JSON] ())), "Delete") :
        (WrappedApi (Proxy :: Proxy (Get '[JSON] ())), "Get") :
        (WrappedApi (Proxy :: Proxy (Post '[JSON] ())), "Post") :
        (WrappedApi (Proxy :: Proxy (Put '[JSON] ())), "Put") :
        []
