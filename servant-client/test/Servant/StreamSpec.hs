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

import           Control.Monad
                 (when)
import           Control.Monad.Codensity
                 (Codensity (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Trans.Except
import qualified Data.ByteString               as BS
import           Data.Proxy
import qualified Data.TDigest                  as TD
import qualified Network.HTTP.Client           as C
import           Prelude ()
import           Prelude.Compat
import           Servant.API
                 ((:<|>) ((:<|>)), (:>), JSON, NetstringFraming, StreamBody,
                 NewlineFraming, NoFraming, OctetStream, SourceIO, StreamGet,
                 )
import           Servant.Client.Streaming
import           Servant.Server
import           Servant.Test.ComprehensiveAPI
import           Servant.Types.SourceT
import           System.Entropy
                 (getEntropy, getHardwareEntropy)
import           System.IO.Unsafe
                 (unsafePerformIO)
import           System.Mem
                 (performGC)
import           Test.Hspec
import           Servant.ClientTestUtils (Person(..))
import qualified Servant.ClientTestUtils as CT

#if MIN_VERSION_base(4,10,0)
import           GHC.Stats
                 (gc, gcdetails_live_bytes, getRTSStats)
#else
import           GHC.Stats
                 (currentBytesUsed, getGCStats)
#endif

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

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

withClient :: ClientM a -> BaseUrl -> (Either ClientError a -> IO r) -> IO r
withClient x baseUrl' = withClientM x (mkClientEnv manager' baseUrl')

testRunSourceIO :: SourceIO a
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

{-
    it "streams in constant memory" $ \(_, baseUrl) -> do
        Right rs <- runClient getGetALot baseUrl
        performGC
        -- usage0 <- getUsage
        -- putStrLn $ "Start:  " ++ show usage0
        tdigest <- memoryUsage $ joinCodensitySourceT rs

        -- putStrLn $ "Median: " ++ show (TD.median tdigest)
        -- putStrLn $ "Mean:   " ++ show (TD.mean tdigest)
        -- putStrLn $ "Stddev: " ++ show (TD.stddev tdigest)

        -- forM_ [0.01, 0.1, 0.2, 0.5, 0.8, 0.9, 0.99] $ \q ->
        --    putStrLn $ "q" ++ show q ++ ": " ++ show (TD.quantile q tdigest)

        let Just stddev = TD.stddev tdigest

        -- standard deviation of 100k is ok, we generate 256M of data after all.
        -- On my machine deviation is 40k-50k
        stddev `shouldSatisfy` (< 100000)

memoryUsage :: SourceT IO BS.ByteString -> IO (TD.TDigest 25)
memoryUsage src = unSourceT src $ loop mempty (0 :: Int)
  where
    loop !acc !_ Stop          = return acc
    loop !_   !_ (Error err)   = fail err -- !
    loop !acc !n (Skip s)      = loop acc n s
    loop !acc !n (Effect ms)   = ms >>= loop acc n
    loop !acc !n (Yield _bs s) =  do
        usage  <- liftIO getUsage
        -- We perform GC in between as we generate garbage.
        when (n `mod` 1024 == 0) $ liftIO performGC
        loop (TD.insert usage acc) (n + 1) s

getUsage :: IO Double
getUsage = fromIntegral .
#if MIN_VERSION_base(4,10,0)
    gcdetails_live_bytes . gc <$> getRTSStats
#else
    currentBytesUsed <$> getGCStats
#endif
       memUsed `shouldSatisfy` (< megabytes 22)

megabytes :: Num a => a -> a
megabytes n = n * (1000 ^ (2 :: Int))
-}
