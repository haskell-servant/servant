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
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=100 #-}
#else
{-# OPTIONS_GHC -fcontext-stack=100 #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

#include "overlapping-compat.h"
module Servant.StreamSpec (spec) where

import           Control.Monad       (replicateM_, void)
import qualified Data.ByteString     as BS
import           Data.Proxy
import           GHC.Stats           (currentBytesUsed, getGCStats)
import qualified Network.HTTP.Client as C
import           Prelude             ()
import           Prelude.Compat
import           System.IO           (IOMode (ReadMode), withFile)
import           System.IO.Unsafe    (unsafePerformIO)
import           Test.Hspec
import           Test.QuickCheck

import           Servant.API         ((:<|>) ((:<|>)), (:>), JSON,
                                      NetstringFraming, NewlineFraming,
                                      OctetStream, ResultStream (..),
                                      StreamGenerator (..), StreamGet)
import           Servant.Client
import           Servant.ClientSpec  (Person (..))
import qualified Servant.ClientSpec  as CS
import           Servant.Server


spec :: Spec
spec = describe "Servant.Stream" $ do
    streamSpec

type StreamApi f =
       "streamGetNewline" :> StreamGet NewlineFraming JSON (f Person)
  :<|> "streamGetNetstring" :> StreamGet  NetstringFraming JSON (f Person)
  :<|> "streamALot" :> StreamGet NewlineFraming OctetStream (f BS.ByteString)


capi :: Proxy (StreamApi ResultStream)
capi = Proxy

sapi :: Proxy (StreamApi StreamGenerator)
sapi = Proxy

getGetNL, getGetNS :: ClientM (ResultStream Person)
getGetALot :: ClientM (ResultStream BS.ByteString)
getGetNL :<|> getGetNS :<|> getGetALot = client capi

alice :: Person
alice = Person "Alice" 42

bob :: Person
bob = Person "Bob" 25

server :: Application
server = serve sapi
  $    return (StreamGenerator (\f r -> f alice >> r bob >> r alice))
  :<|> return (StreamGenerator (\f r -> f alice >> r bob >> r alice))
  :<|> return (StreamGenerator lotsGenerator)
  where
    lotsGenerator f r = do
      f ""
      withFile "/dev/urandom" ReadMode $
        \handle -> streamFiveMBNTimes handle 1000 r
      return ()

    streamFiveMBNTimes handle left sink
      | left <= 0 = return ""
      | otherwise = do
          msg <- BS.hGet handle (megabytes 5)
          sink msg
          streamFiveMBNTimes handle (left - 1) sink



{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: ClientM a -> BaseUrl -> IO (Either ServantError a)
runClient x baseUrl' = runClientM x (mkClientEnv manager' baseUrl')

runResultStream :: ResultStream a
  -> IO ( Maybe (Either String a)
        , Maybe (Either String a)
        , Maybe (Either String a)
        , Maybe (Either String a))
runResultStream (ResultStream k)
  = k $ \act -> (,,,) <$> act <*> act <*> act <*> act

streamSpec :: Spec
streamSpec = beforeAll (CS.startWaiApp server) $ afterAll CS.endWaiApp $ do

    it "works with Servant.API.StreamGet.Newline" $ \(_, baseUrl) -> do
       Right res <- runClient getGetNL baseUrl
       let jra = Just (Right alice)
           jrb = Just (Right bob)
       runResultStream res `shouldReturn` (jra, jrb, jra, Nothing)

    it "works with Servant.API.StreamGet.Netstring" $ \(_, baseUrl) -> do
       Right res <- runClient getGetNS baseUrl
       let jra = Just (Right alice)
           jrb = Just (Right bob)
       runResultStream res `shouldReturn` (jra, jrb, jra, Nothing)

    it "streams in constant memory" $ \(_, baseUrl) -> do
       Right (ResultStream res) <- runClient getGetALot baseUrl
       let consumeNChunks n = replicateM_ n (res void)
       consumeNChunks 900
       memUsed <- currentBytesUsed <$> getGCStats
       memUsed `shouldSatisfy` (< (megabytes 20))

megabytes :: Num a => a -> a
megabytes n = n * (1000 ^ 2)
