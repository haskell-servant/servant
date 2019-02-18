{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.StreamSpec (spec) where

import           Control.Monad.Trans.Except
import qualified Data.ByteString               as BS
import           Data.Proxy
import           Prelude ()
import           Prelude.Compat
import           Servant.API
                 ((:<|>) ((:<|>)), (:>), JSON, NetstringFraming,
                 NewlineFraming, NoFraming, OctetStream, SourceIO, StreamBody,
                 StreamGet)
import           Servant.ClientSpec
                 (Person (..))
import qualified Servant.ClientSpec            as CS
import           Servant.HttpStreams
import           Servant.Server
import           Servant.Types.SourceT
import           System.Entropy
                 (getEntropy, getHardwareEntropy)
import           Test.Hspec


spec :: Spec
spec = describe "Servant.HttpStreams streaming" $ do
    streamSpec

type StreamApi =
         "streamGetNewline" :> StreamGet NewlineFraming JSON (SourceIO Person)
    :<|> "streamGetNetstring" :> StreamGet NetstringFraming JSON (SourceIO Person)
    :<|> "streamALot" :> StreamGet NoFraming OctetStream (SourceIO BS.ByteString)
    :<|> "streamBody" :> StreamBody NoFraming OctetStream (SourceIO BS.ByteString) :> StreamGet NoFraming OctetStream (SourceIO BS.ByteString)

api :: Proxy StreamApi
api = Proxy

getGetNL, getGetNS :: ClientM (SourceIO Person)
_getGetALot :: ClientM (SourceIO BS.ByteString)
getStreamBody :: SourceT IO BS.ByteString -> ClientM (SourceIO BS.ByteString)
getGetNL :<|> getGetNS :<|> _getGetALot :<|> getStreamBody = client api

alice :: Person
alice = Person "Alice" 42

bob :: Person
bob = Person "Bob" 25

server :: Application
server = serve api
    $    return (source [alice, bob, alice])
    :<|> return (source [alice, bob, alice])
    -- 2 ^ (18 + 10) = 256M
    :<|> return (SourceT ($ lots (powerOfTwo 18)))
    :<|> return
  where
    lots n
        | n < 0     = Stop
        | otherwise = Effect $ do
            let size = powerOfTwo 10
            mbs <- getHardwareEntropy size
            bs <- maybe (getEntropy size) pure mbs
            return (Yield bs (lots (n - 1)))

powerOfTwo :: Int -> Int
powerOfTwo = (2 ^)

withClient :: ClientM a -> BaseUrl -> (Either ClientError a -> IO r) -> IO r
withClient x burl k = do
    withClientEnvIO burl $ \env -> withClientM x env k

testRunSourceIO :: SourceIO a
    -> IO (Either String [a])
testRunSourceIO = runExceptT . runSourceT

streamSpec :: Spec
streamSpec = beforeAll (CS.startWaiApp server) $ afterAll CS.endWaiApp $ do
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
