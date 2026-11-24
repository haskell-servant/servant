{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Serving 'ServerSentEvents'' endpoints.
module Servant.Server.ServerSentEventsSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types (hContentType, methodGet, methodPost)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.API
  ( EventFields (..)
  , EventKind (..)
  , Header
  , Headers
  , ServerEvent (..)
  , ServerSentEvents
  , ServerSentEvents'
  , SourceIO
  , StdMethod (POST)
  , addHeader
  , noEventFields
  , serverEvent
  , (:<|>) (..)
  , (:>)
  )
import qualified Servant.Types.SourceT as S
import qualified System.Timeout
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Wai
  ( matchHeaders
  , matchStatus
  , request
  , shouldRespondWith
  , with
  , (<:>)
  )

import Servant.Server (Application, Handler, serve)

type SseApi =
  "json" :> ServerSentEvents 'JsonEvent [Int]
    :<|> "raw" :> ServerSentEvents 'RawEvent BSL.ByteString
    :<|> "headers" :> ServerSentEvents 'JsonEvent (Headers '[Header "X-Accel-Buffering" Text] Int)
    :<|> "post" :> ServerSentEvents' 'POST 201 'JsonEvent Int

sseApi :: Proxy SseApi
sseApi = Proxy

sseServer :: Application
sseServer =
  serve sseApi $
    jsonEvents
      :<|> rawEvents
      :<|> pure (addHeader "no" (S.source [serverEvent 1]))
      :<|> pure (S.source [serverEvent 2])
  where
    jsonEvents :: Handler (SourceIO (ServerEvent [Int]))
    jsonEvents =
      pure $
        S.source
          [ ServerComment "keepalive"
          , serverEvent [1, 2]
          , ServerEvent
              noEventFields
                { eventFieldName = Just "update"
                , eventFieldId = Just "7"
                , eventFieldRetry = Just 3000
                }
              [3]
          ]

    rawEvents :: Handler (SourceIO (ServerEvent BSL.ByteString))
    rawEvents =
      pure $
        S.source
          [ serverEvent "one\ntwo\r\nthree"
          , ServerEvent noEventFields{eventFieldId = Just "a\nb"} "four"
          ]

spec :: Spec
spec = describe "ServerSentEvents'" $ do
  with (pure sseServer) $ do
    it "streams JSON events as text/event-stream" $
      request methodGet "/json" [("Accept", "text/event-stream")] ""
        `shouldRespondWith` ": keepalive\ndata: [1,2]\n\nevent: update\nid: 7\nretry: 3000\ndata: [3]\n\n"
          { matchHeaders = ["Content-Type" <:> "text/event-stream"]
          }

    it "sends raw data as it is, one data line per line" $
      request methodGet "/raw" [("Accept", "text/event-stream")] ""
        `shouldRespondWith` "data: one\ndata: two\ndata: three\n\nid: ab\ndata: four\n\n"

    it "serves a request that accepts any type" $
      request methodGet "/json" [] "" `shouldRespondWith` 200

    it "rejects a request that does not accept text/event-stream" $
      request methodGet "/json" [("Accept", "application/json")] "" `shouldRespondWith` 406

    it "adds the headers the handler returns" $
      request methodGet "/headers" [] ""
        `shouldRespondWith` "data: 1\n\n"
          { matchHeaders = ["X-Accel-Buffering" <:> "no", "Content-Type" <:> "text/event-stream"]
          }

    it "uses the method and status of the endpoint" $ do
      request methodPost "/post" [] "" `shouldRespondWith` "data: 2\n\n"{matchStatus = 201}
      request methodGet "/post" [] "" `shouldRespondWith` 405

  it "sends each event over Warp as soon as the handler yields it" $ do
    release <- newEmptyMVar
    let events :: SourceIO (ServerEvent Int)
        events =
          S.SourceT $ \k ->
            k $
              S.Yield (serverEvent 1) $
                S.Effect $ do
                  takeMVar release
                  pure (S.Yield (serverEvent 2) S.Stop)
        app = serve (Proxy :: Proxy ("live" :> ServerSentEvents 'JsonEvent Int)) (pure events)
    testWithApplication (pure app) $ \port -> do
      manager <- C.newManager C.defaultManagerSettings
      req <- C.parseRequest ("http://127.0.0.1:" ++ show port ++ "/live")
      let req' = req{C.requestHeaders = [("Accept", "text/event-stream")]}
      C.withResponse req' manager $ \response -> do
        lookup hContentType (C.responseHeaders response) `shouldBe` Just "text/event-stream"
        -- The second event waits for the first to arrive, so this read only
        -- ends if the first event is flushed on its own.
        first <- timeout $ readEvent (C.responseBody response) ""
        first `shouldBe` "data: 1\n\n"
        putMVar release ()
        rest <- timeout $ BS.concat <$> C.brConsume (C.responseBody response)
        rest `shouldBe` "data: 2\n\n"
  where
    readEvent body acc
      | "\n\n" `BS.isSuffixOf` acc = pure acc
      | otherwise = do
          chunk <- C.brRead body
          if BS.null chunk
            then pure acc
            else readEvent body (acc <> chunk)

    timeout action =
      System.Timeout.timeout 5000000 action
        >>= maybe (throwIO (ErrorCall "timed out waiting for an event")) pure
