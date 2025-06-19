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
  ( JSON
  , NetstringFraming
  , NewlineFraming
  , NoFraming
  , OctetStream
  , SourceIO
  , StreamBody
  , StreamGet
  , (:<|>) ((:<|>))
  , (:>)
  )
import Servant.Client.Streaming
import Servant.ClientTestUtils (Person (..))
import qualified Servant.ClientTestUtils as CT
import Servant.Server
import Servant.Test.ComprehensiveAPI
import Servant.Types.SourceT
import System.Entropy
  ( getEntropy
  , getHardwareEntropy
  )
import System.IO.Unsafe
  ( unsafePerformIO
  )
import Test.Hspec
import Prelude ()

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

api :: Proxy StreamApi
api = Proxy

getGetNL, getGetNS :: ClientM (SourceIO Person)
getGetALot :: ClientM (SourceIO BS.ByteString)
getStreamBody :: SourceT IO BS.ByteString -> ClientM (SourceIO BS.ByteString)
getGetNL :<|> getGetNS :<|> getGetALot :<|> getStreamBody = client api

alice :: Person
alice = Person "Alice" 42

bob :: Person
bob = Person "Bob" 25

server :: Application
server =
  serve api $
    return (source [alice, bob, alice])
      :<|> return (source [alice, bob, alice])
      -- 2 ^ (18 + 10) = 256M
      :<|> return (SourceT ($ lots (powerOfTwo 18)))
      :<|> return
  where
    lots n
      | n < 0 = Stop
      | otherwise = Effect $ do
          let size = powerOfTwo 10
          mbs <- getHardwareEntropy size
          bs <- maybe (getEntropy size) pure mbs
          return (Yield bs (lots (n - 1)))

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

  it "works with Servant.API.StreamBody" $ \(_, baseUrl) -> do
    withClient (getStreamBody (source input)) baseUrl $ \(Right res) ->
      testRunSourceIO res `shouldReturn` Right output
  where
    input = ["foo", "", "bar"]
    output = ["foo", "bar"]
