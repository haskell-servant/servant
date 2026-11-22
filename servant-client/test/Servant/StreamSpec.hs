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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

module Servant.StreamSpec (spec) where

import Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import Data.Proxy
import qualified Network.HTTP.Client as C
import Prelude.Compat
import Servant.API
  ( EventFields (..)
  , EventKind (JsonEvent)
  , JSON
  , NetstringFraming
  , NewlineFraming
  , NoFraming
  , OctetStream
  , ServerEvent (..)
  , ServerSentEvents
  , SourceIO
  , StreamBody
  , StreamGet
  , noEventFields
  , serverEvent
  , (:<|>) ((:<|>))
  , (:>)
  )
import Servant.Client.Core.ServerSentEvents (Event (..), JsonEventStreamT (..))
import Servant.Server
import Servant.Test.ComprehensiveAPI
import Servant.Types.SourceT
import System.Entropy (getEntropy, getHardwareEntropy)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Prelude ()

import Servant.Client.Streaming
import Servant.ClientTestUtils (Person (..))
import qualified Servant.ClientTestUtils as CT

-- This declaration simply checks that all instances are in place.
-- Note: this is streaming client
_ = client comprehensiveAPI

spec :: Spec
spec = describe "Servant.Client.Streaming" $ do
  streamSpec

type StreamApi =
  "streamGetNewline" :> StreamGet NewlineFraming JSON (SourceIO Person)
    :<|> "streamGetNetstring" :> StreamGet NetstringFraming JSON (SourceIO Person)
    :<|> "streamALot" :> StreamGet NoFraming OctetStream (SourceIO BS.ByteString)
    :<|> "streamBody" :> StreamBody NoFraming OctetStream (SourceIO BS.ByteString) :> StreamGet NoFraming OctetStream (SourceIO BS.ByteString)
    :<|> "sse" :> ServerSentEvents 'JsonEvent Person

api :: Proxy StreamApi
api = Proxy

getGetNL, getGetNS :: ClientM (SourceIO Person)
getGetALot :: ClientM (SourceIO BS.ByteString)
getStreamBody :: SourceT IO BS.ByteString -> ClientM (SourceIO BS.ByteString)
getSse :: ClientM (JsonEventStreamT IO Person)
getGetNL :<|> getGetNS :<|> getGetALot :<|> getStreamBody :<|> getSse = client api

alice :: Person
alice = Person "Alice" 42

bob :: Person
bob = Person "Bob" 25

server :: Application
server =
  serve api $
    pure (source [alice, bob, alice])
      :<|> pure (source [alice, bob, alice])
      -- 2 ^ (18 + 10) = 256M
      :<|> pure (SourceT ($ lots (powerOfTwo 18)))
      :<|> pure
      :<|> pure
        ( source
            [ ServerComment "keepalive"
            , serverEvent alice
            , ServerEvent noEventFields{eventFieldName = Just "update", eventFieldId = Just "2"} bob
            ]
        )
  where
    lots n
      | n < 0 = Stop
      | otherwise = Effect $ do
          let size = powerOfTwo 10
          mbs <- getHardwareEntropy size
          bs <- maybe (getEntropy size) pure mbs
          pure (Yield bs (lots (n - 1)))

powerOfTwo :: Int -> Int
powerOfTwo = (2 ^)

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

withClient :: ClientM a -> BaseUrl -> (Either ClientError a -> IO r) -> IO r
withClient x baseUrl' = withClientM x (mkClientEnv manager' baseUrl')

testRunSourceIO
  :: SourceIO a
  -> IO (Either String [a])
testRunSourceIO = runExceptT . runSourceT

streamSpec :: Spec
streamSpec = beforeAll (CT.startWaiApp server) $ afterAll CT.endWaiApp $ do
  it "works with Servant.API.StreamGet.Newline" $ \(_, baseUrl) -> do
    withClient getGetNL baseUrl $ \(Right res) ->
      testRunSourceIO res `shouldReturn` Right [alice, bob, alice]

  it "works with Servant.API.StreamGet.Netstring" $ \(_, baseUrl) -> do
    withClient getGetNS baseUrl $ \(Right res) ->
      testRunSourceIO res `shouldReturn` Right [alice, bob, alice]

  it "receives the events of a ServerSentEvents server" $ \(_, baseUrl) -> do
    withClient getSse baseUrl $ \(Right res) ->
      testRunSourceIO (unJsonEventStreamT res)
        `shouldReturn` Right [Event Nothing alice, Event (Just "update") bob]

  it "works with Servant.API.StreamBody" $ \(_, baseUrl) -> do
    withClient (getStreamBody (source input)) baseUrl $ \(Right res) ->
      testRunSourceIO res `shouldReturn` Right output
  where
    input = ["foo", "", "bar"]
    output = ["foo", "bar"]
