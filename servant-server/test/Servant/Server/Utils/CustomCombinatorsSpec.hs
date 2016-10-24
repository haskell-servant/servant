{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Server.Utils.CustomCombinatorsSpec where

import           Control.Concurrent
import           Data.ByteString.Builder
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.ByteString as SBS hiding (map)
import           Data.ByteString.Lazy as LBS hiding (map)
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text hiding (map)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Internal
import           Test.Hspec

import           Servant.API
import           Servant.Server
import           Servant.Server.Utils.CustomCombinators

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

responseBodyLbs :: Response -> IO LBS.ByteString
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
  describe "makeCaptureCombinator" $ do
    it "allows to write capture combinators" $ do
      let server = return
          app = serve (Proxy :: Proxy (StringCapture :> Get' String)) server
          request = defaultRequest{
            rawPathInfo = "/foo",
            pathInfo = ["foo"]
          }
      response <- runApp app request
      responseBodyLbs response `shouldReturn` "\"foo\""

    it "allows to write a combinator that errors out" $ do
      let server = return
          app = serve (Proxy :: Proxy (StringCapture :> Get' String)) server
          request = defaultRequest {
            rawPathInfo = "/error",
            pathInfo = ["error"]
          }
      response <- runApp app request
      responseStatus response `shouldBe` status418

  describe "makeRequestCheckCombinator" $ do
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

    it "disallows to access the request body" $ do
      let server = return ()
          app = serve (Proxy :: Proxy (InvalidRequestCheckCombinator :> Get' ())) server
          request = defaultRequest
      runApp app request `shouldThrow`
        errorCall "ERROR: makeRequestCheckCombinator: combinator must not access the request body"

  describe "makeAuthCombinator" $ do
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

    it "disallows to access the request body" $ do
      let server _user = return "foo"
          app = serve (Proxy :: Proxy (InvalidAuthCombinator :> Get' String)) server
          request = defaultRequest
      runApp app request `shouldThrow`
        errorCall "ERROR: makeAuthCombinator: combinator must not access the request body"

  describe "makeCombinator" $ do
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

  describe "makeReqBodyCombinator" $ do
    let toBody :: [IO SBS.ByteString] -> IO (IO SBS.ByteString)
        toBody list = do
          mvar <- newMVar list
          return $ do
            modifyMVar mvar $ \case
              (a : r) -> do
                chunk <- a
                return (r, chunk)
              [] -> return ([], "")

    it "allows to write combinators" $ do
      body <- toBody $ map return ["foo", "bar"]
      let server (Source b) = liftIO $ cs <$> fromBody b
          app = serve (Proxy :: Proxy (StreamRequest :> Get' String)) server
          request = defaultRequest{
            requestBody = body
          }
      response <- runApp app request
      responseBodyLbs response `shouldReturn` "\"foobar\""

    it "allows to stream lazily" $ do
      mvar <- newEmptyMVar
      body <- toBody [return "foo", takeMVar mvar >> return "bar"]
      let server (Source b) = liftIO $ do
            first <- b
            deepseq first (return ())
            putMVar mvar ()
            cs <$> (first <>) <$> fromBody b
          app = serve (Proxy :: Proxy (StreamRequest :> Get' String)) server
          request = defaultRequest{
            requestBody = body
          }
      response <- runApp app request
      responseBodyLbs response `shouldReturn` "\"foobar\""

  it "allows to access the context" $ do
    pending

  it "allows to implement combinators in terms of existing combinators" $ do
    pending

type Get' = Get '[JSON]

-- * capture combinators

data StringCapture

instance HasServer api context => HasServer (StringCapture :> api) context where
  type ServerT (StringCapture :> api) m = String -> ServerT api m
  route = runCI $ makeCaptureCombinator getCapture

getCapture :: Text -> IO (RouteResult String)
getCapture snippet = return $ case snippet of
  "error" -> FailFatal $ ServantErr 418 "I'm a teapot" "" []
  text -> Route $ cs text

-- * request check combinators

data CheckFooHeader

instance HasServer api context => HasServer (CheckFooHeader :> api) context where
  type ServerT (CheckFooHeader :> api) m = ServerT api m
  route = runCI $ makeRequestCheckCombinator checkFooHeader

checkFooHeader :: Request -> IO (RouteResult ())
checkFooHeader request = return $
  case lookup "Foo" (requestHeaders request) of
    Just _ -> Route ()
    Nothing -> FailFatal err400

-- | a combinator that tries to access the request body in an invalid way
data InvalidRequestCheckCombinator

instance HasServer api context => HasServer (InvalidRequestCheckCombinator :> api) context where
  type ServerT (InvalidRequestCheckCombinator :> api) m = ServerT api m
  route = runCI $ makeRequestCheckCombinator accessReqBody

accessReqBody :: Request -> IO (RouteResult ())
accessReqBody request = do
  body <- fromBody $ requestBody request
  deepseq body (return $ Route ())

-- * auth combinators

data AuthCombinator

data User = User String
  deriving (Eq, Show)

instance HasServer api context => HasServer (AuthCombinator :> api) context where
  type ServerT (AuthCombinator :> api) m = User -> ServerT api m
  route = runCI $ makeAuthCombinator checkAuth

checkAuth :: Request -> IO (RouteResult User)
checkAuth request = return $ case lookup "Auth" (requestHeaders request) of
  Just "secret" -> Route $ User "Alice"
  Just _ -> FailFatal err401
  Nothing -> FailFatal err400

-- | a combinator that tries to access the request body in an invalid way
data InvalidAuthCombinator

instance HasServer api context => HasServer (InvalidAuthCombinator :> api) context where
  type ServerT (InvalidAuthCombinator :> api) m = User -> ServerT api m
  route = runCI $ makeAuthCombinator authWithReqBody

authWithReqBody :: Request -> IO (RouteResult User)
authWithReqBody request = do
  body <- fromBody $ requestBody request
  deepseq body (return $ Route $ User $ cs body)

-- * general combinators

data FooHeader

instance HasServer api context => HasServer (FooHeader :> api) context where
  type ServerT (FooHeader :> api) m = String -> ServerT api m
  route = runCI $ makeCombinator getCustom

getCustom :: Request -> IO (RouteResult String)
getCustom request = return $ case lookup "Foo" (requestHeaders request) of
  Nothing -> FailFatal err400
  Just l -> Route $ cs l

-- * streaming combinators

data StreamRequest

data Source = Source (IO SBS.ByteString)

instance HasServer api context => HasServer (StreamRequest :> api) context where
  type ServerT (StreamRequest :> api) m = Source -> ServerT api m
  route = runCI $ makeReqBodyCombinator getSource

getSource :: IO SBS.ByteString -> Source
getSource = Source

-- * utils

fromBody :: IO SBS.ByteString -> IO SBS.ByteString
fromBody getChunk = do
  chunk <- getChunk
  if chunk == ""
    then return ""
    else do
      rest <- fromBody getChunk
      return $ chunk <> rest
