{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- | This module tests whether streaming works from client to server
-- with a server implemented with servant-server.
module Servant.Server.StreamingSpec where

import           Control.Concurrent
import           Control.Exception      hiding
                 (Handler)
import           Control.Monad.IO.Class
import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Lazy   as Lazy
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Internal
import           Prelude ()
import           Prelude.Compat
import           Servant
import qualified System.Timeout
import           Test.Hspec

type TestAPI =
  ReqBody '[OctetStream] Lazy.ByteString :> Get '[JSON] NoContent

testAPI :: Proxy TestAPI
testAPI = Proxy

spec :: Spec
spec = do
  -- The idea of this test is this:
  --
  -- - The mock client will
  --   - send some data in the request body, but not all,
  --   - wait for the server to acknowledge (outside of http, through an MVar)
  --     that the server received some data,
  --   - send the rest of the request body.
  -- - The mock server will
  --   - receive some data,
  --   - notify the client that it received some data,
  --   - receive the rest of the data,
  --   - respond with an empty result.
  it "client to server can stream lazy ByteStrings" $ timeout $ do
    serverReceivedFirstChunk <- newWaiter

    -- - streams some test data
    -- - waits for serverReceivedFirstChunk
    -- - streams some more test data
    streamTestData <- do
      mvar :: MVar [IO Strict.ByteString] <- newMVar $
        map return (replicate 1000 "foo") ++
        (waitFor serverReceivedFirstChunk >> return "foo") :
        map return (replicate 1000 "foo")
      return $ modifyMVar mvar $ \ actions -> case actions of
        (a : r) -> (r, ) <$> a
        [] -> return ([], "")

    let request = defaultRequest {
          requestBody = streamTestData,
          requestBodyLength = ChunkedBody
        }

    -- - receives the first chunk
    -- - notifies serverReceivedFirstChunk
    -- - receives the rest of the request
    let handler :: Lazy.ByteString -> Handler NoContent
        handler input = liftIO $ do
          let prefix = Lazy.take 3 input
          prefix `shouldBe` "foo"
          notify serverReceivedFirstChunk ()
          input `shouldBe` mconcat (replicate 2001 "foo")
          return NoContent

        app = serve testAPI handler
    response <- executeRequest app request
    statusCode (responseStatus response) `shouldBe` 200

executeRequest :: Application -> Request -> IO Response
executeRequest app request = do
  responseMVar <- newEmptyMVar
  let respond response = do
        putMVar responseMVar response
        return ResponseReceived
  ResponseReceived <- app request respond
  takeMVar responseMVar

timeout :: IO a -> IO a
timeout action = do
  result <- System.Timeout.timeout 1000000 action
  maybe (throwIO $ ErrorCall "timeout") return result

-- * waiter

data Waiter a
  = Waiter {
    notify :: a -> IO (),
    waitFor :: IO a
  }

newWaiter :: IO (Waiter a)
newWaiter = do
  mvar <- newEmptyMVar
  return $ Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }
