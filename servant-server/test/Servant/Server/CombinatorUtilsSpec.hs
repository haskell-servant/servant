{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Server.CombinatorUtilsSpec where

import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Data.ByteString.Lazy
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Internal
import           Test.Hspec

import           Servant.API
import           Servant.Server
import           Servant.Server.CombinatorUtils

runApp :: Application -> Request -> IO Response
runApp app req = do
  mvar <- newMVar Nothing
  app req $ \ response -> do
    modifyMVar mvar $ \ Nothing ->
      return $ (Just response, ResponseReceived)
  modifyMVar mvar $ \mResponse -> do
    case mResponse of
      Nothing -> error "shouldn't happen"
      Just response -> return (Just response, response)

responseBodyLbs :: Response -> IO ByteString
responseBodyLbs response = do
  let (_, _, action) = responseToStream response
  action $ \ streamingBody -> do
    mvar <- newMVar ""
    streamingBody
      (\ builder -> modifyMVar_ mvar $ \ acc ->
        return $ acc <> toLazyByteString builder)
      (return ())
    readMVar mvar

spec :: Spec
spec = do
  it "allows to write a combinator by providing a function (Request -> a)" $ do
    let server = return
        app = serve (Proxy :: Proxy (Custom :> Get' String)) server
        request = defaultRequest{
          requestHeaders =
            ("Custom", "foo") :
            requestHeaders defaultRequest
        }
    response <- runApp app request
    responseBodyLbs response `shouldReturn` "\"foo\""

  it "allows to write a combinator the errors out" $ do
    let server = return
        app = serve (Proxy :: Proxy (Custom :> Get' String)) server
        request = defaultRequest
    response <- runApp app request
    responseStatus response `shouldBe` status400

  it "allows to pick the phase of request consumption" $ do
    pending

type Get' = Get '[JSON]

data Custom

instance HasServer api context => HasServer (Custom :> api) context where
  type ServerT (Custom :> api) m = String -> ServerT api m
  route = argumentCombinator getCustom

getCustom :: Request -> RouteResult String
getCustom request = case lookup "Custom" (requestHeaders request) of
  Nothing -> FailFatal err400
  Just l -> Route $ cs l
