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

import Control.Arrow (left)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, throwIO, try)
import Control.Monad.IO.Class
import Data.ByteString.Builder (toLazyByteString)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Monoid ()
import Prelude.Compat
import Servant.Client
import Servant.Client.Core (RequestF (..))
import Servant.Client.Internal.HttpClient (ClientMiddleware)
import Servant.ClientTestUtils
import Test.Hspec
import Prelude ()

runClientWithMiddleware :: ClientM a -> ClientMiddleware -> BaseUrl -> IO (Either ClientError a)
runClientWithMiddleware x mid baseUrl' =
  runClientM x ((mkClientEnv manager' baseUrl') {middleware = mid})

data CustomException = CustomException deriving (Show, Eq)

instance Exception CustomException

spec :: Spec
spec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do
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

  it "errors can be thrown in middleware" $ \(_, baseUrl) -> do
    let mid :: ClientMiddleware
        mid oldApp req = do
          -- perform request
          resp <- oldApp req
          -- throw error
          _ <- liftIO $ throwIO CustomException
          pure resp

    try (runClientWithMiddleware getGet mid baseUrl) `shouldReturn` Left CustomException

  it "runs in the expected order" $ \(_, baseUrl) -> do
    ref <- newIORef []

    let mid1 :: ClientMiddleware
        mid1 oldApp req = do
          liftIO $ modifyIORef ref (\xs -> xs <> ["req1"])
          resp <- oldApp req
          liftIO $ modifyIORef ref (\xs -> xs <> ["resp1"])
          pure resp

    let mid2 :: ClientMiddleware
        mid2 oldApp req = do
          liftIO $ modifyIORef ref (\xs -> xs <> ["req2"])
          resp <- oldApp req
          liftIO $ modifyIORef ref (\xs -> xs <> ["resp2"])
          pure resp

    let mid3 :: ClientMiddleware
        mid3 oldApp req = do
          liftIO $ modifyIORef ref (\xs -> xs <> ["req3"])
          resp <- oldApp req
          liftIO $ modifyIORef ref (\xs -> xs <> ["resp3"])
          pure resp

    let mid :: ClientMiddleware
        mid = mid1 . mid2 . mid3
        -- \^ Composition in "reverse order".
        -- It is equivalent to the following, which is more intuitive:
        -- mid :: ClientMiddleware
        -- mid oldApp = mid1 (mid2 (mid3 oldApp))

    -- Same as without middleware
    left show <$> runClientWithMiddleware getGet mid baseUrl `shouldReturn` Right alice

    ref <- readIORef ref
    ref `shouldBe` ["req1", "req2", "req3", "resp3", "resp2", "resp1"]
