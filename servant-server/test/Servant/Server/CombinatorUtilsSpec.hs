{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Server.CombinatorUtilsSpec where

import           Control.Concurrent
import           Data.ByteString.Builder
import           Data.ByteString.Lazy
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text
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
  ResponseReceived <- app req $ \ response -> do
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
  it "allows to write capture combinators" $ do
    let server = return
        app = serve (Proxy :: Proxy (StringCapture :> Get' String)) server
        request = defaultRequest{
          rawPathInfo = "/foo",
          pathInfo = ["foo"]
        }
    response <- runApp app request
    responseBodyLbs response `shouldReturn` "\"foo\""

  it "allows to write request check combinators" $ do
    let server = return ()
        app = serve (Proxy :: Proxy (CheckFooHeader :> Get' ())) server
        request = defaultRequest{
          requestHeaders =
            ("Foo", "foo") :
            requestHeaders defaultRequest
        }
    response <- runApp app request
    responseBodyLbs response `shouldReturn` "[]"

  it "allows to write a combinator that errors out" $ do
    let server = return
        app = serve (Proxy :: Proxy (StringCapture :> Get' String)) server
        request = defaultRequest {
          rawPathInfo = "/error",
          pathInfo = ["error"]
        }
    response <- runApp app request
    responseStatus response `shouldBe` status418

  it "allows to write a combinator using IO" $ do
    pending

  it "allows to write a combinator by providing a function (Request -> a)" $ do
    let server = return
        app = serve (Proxy :: Proxy (FooHeader :> Get' String)) server
        request = defaultRequest{
          requestHeaders =
            ("Foo", "foo") :
            requestHeaders defaultRequest
        }
    response <- runApp app request
    responseBodyLbs response `shouldReturn` "\"foo\""

  it "allows to write an auth combinator" $ do
    let server (User name) = return name
        app = serve (Proxy :: Proxy (AuthCombinator :> Get' String)) server
        request = defaultRequest{
          requestHeaders =
            ("Auth", "secret") :
            requestHeaders defaultRequest
        }
    response <- runApp app request
    responseStatus response `shouldBe` ok200
    responseBodyLbs response `shouldReturn` "\"Alice\""

  it "allows to pick the request check phase" $ do
    pending

  it "allows to write streaming combinators for request bodies" $ do
    pending

  it "disallows to access the request body unless in the checkBody phase" $ do
    pending

  it "allows to access the context" $ do
    pending

  it "allows to write combinators without args" $ do
    pending

  it "allows to implement combinators based in terms of existing combinators" $ do
    pending

type Get' = Get '[JSON]

data StringCapture

instance HasServer api context => HasServer (StringCapture :> api) context where
  type ServerT (StringCapture :> api) m = String -> ServerT api m
  route = runCI $ implementCaptureCombinator getCapture

getCapture :: Text -> RouteResult String
getCapture = \case
  "error" -> FailFatal $ ServantErr 418 "I'm a teapot" "" []
  text -> Route $ cs text

data CheckFooHeader

instance HasServer api context => HasServer (CheckFooHeader :> api) context where
  type ServerT (CheckFooHeader :> api) m = ServerT api m
  route = runCI $ implementRequestCheck checkFooHeader

checkFooHeader :: Request -> RouteResult ()
checkFooHeader request = case lookup "Foo" (requestHeaders request) of
  Just _ -> Route ()
  Nothing -> FailFatal err400

data AuthCombinator

data User = User String
  deriving (Eq, Show)

instance HasServer api context => HasServer (AuthCombinator :> api) context where
  type ServerT (AuthCombinator :> api) m = User -> ServerT api m
  route = runCI $ implementAuthCombinator checkAuth

checkAuth :: Request -> RouteResult User
checkAuth request = case lookup "Auth" (requestHeaders request) of
  Just "secret" -> Route $ User "Alice"
  Just _ -> FailFatal err401
  Nothing -> FailFatal err400

data FooHeader

instance HasServer api context => HasServer (FooHeader :> api) context where
  type ServerT (FooHeader :> api) m = String -> ServerT api m
  route = runCI $ argumentCombinator getCustom

getCustom :: Request -> RouteResult String
getCustom request = case lookup "Foo" (requestHeaders request) of
  Nothing -> FailFatal err400
  Just l -> Route $ cs l
