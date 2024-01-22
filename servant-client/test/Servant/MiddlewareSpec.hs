{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

module Servant.MiddlewareSpec (spec) where

import Control.Arrow
  ( left,
  )
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class
import Data.ByteString.Builder (toLazyByteString)
import Data.Monoid ()
import Prelude.Compat
import Servant.Client
import Servant.Client.Core (RequestF (..))
import Servant.Client.Internal.HttpClient (ClientMiddleware)
import Servant.ClientTestUtils
import Test.Hspec
import Prelude ()
import Control.Exception (Exception, throwIO, try)

spec :: Spec
spec = describe "Servant.MiddlewareSpec" $ do
  middlewareSpec

runClientWithMiddleware :: ClientM a -> ClientMiddleware -> BaseUrl -> IO (Either ClientError a)
runClientWithMiddleware x mid baseUrl' =
  runClientM x ((mkClientEnv manager' baseUrl') {middleware = mid})

data CustomException = CustomException deriving (Show, Eq)
instance Exception CustomException

middlewareSpec :: Spec
middlewareSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do
  describe "middleware" $ do
    it "Raw request and response can be accessed in middleware" $ \(_, baseUrl) -> do
      mvarReq <- newEmptyMVar
      mvarResp <- newEmptyMVar

      let mid :: ClientMiddleware
          mid oldApp req = do
            -- "Log" request
            liftIO $ putMVar mvarReq req
            -- perform request
            resp <- oldApp req
            -- "Log" response
            liftIO $ putMVar mvarResp resp
            pure resp

      -- Same as without middleware
      left show <$> runClientWithMiddleware getGet mid baseUrl `shouldReturn` Right alice

      -- Access some raw request data
      req <- takeMVar mvarReq
      toLazyByteString (requestPath req) `shouldBe` "/get"

      -- Access some raw response data
      resp <- takeMVar mvarResp
      responseBody resp `shouldBe` "{\"_age\":42,\"_name\":\"Alice\"}"
  describe "error in middleware" $ do
    it "errors can be thrown in middleware" $ \(_, baseUrl) -> do
      
      let mid :: ClientMiddleware
          mid oldApp req = do
            -- perform request
            resp <- oldApp req
            -- throw error
            liftIO $ throwIO CustomException
            pure resp

      try (runClientWithMiddleware getGet mid baseUrl) `shouldReturn` Left CustomException
