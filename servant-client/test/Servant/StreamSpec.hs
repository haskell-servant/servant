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

import           Prelude                                    ()
import           Prelude.Compat
import           Data.Proxy
import qualified Network.HTTP.Client                        as C
import           System.IO.Unsafe                           (unsafePerformIO)
import           Test.Hspec

import           Servant.API                                ((:<|>) ((:<|>)),
                                                             (:>),
                                                             EmptyAPI, JSON,
                                                             StreamGet,
                                                             NewlineFraming,
                                                             NetstringFraming,
                                                             ResultStream(..),
                                                             StreamGenerator(..))
import           Servant.Client
import           Servant.Server
import qualified Servant.ClientSpec                          as CS
import           Servant.ClientSpec                          (Person(..))


spec :: Spec
spec = describe "Servant.Stream" $ do
    streamSpec

type StreamApi f =
       "streamGetNewline" :> StreamGet NewlineFraming JSON (f Person)
  :<|> "streamGetNetstring" :> StreamGet  NetstringFraming JSON (f Person)
  :<|> EmptyAPI


capi :: Proxy (StreamApi ResultStream)
capi = Proxy

sapi :: Proxy (StreamApi StreamGenerator)
sapi = Proxy


getGetNL :<|> getGetNS :<|> EmptyClient = client capi


getGetNL :: ClientM (ResultStream Person)
getGetNS :: ClientM (ResultStream Person)

alice :: Person
alice = Person "Alice" 42

bob :: Person
bob = Person "Bob" 25

server :: Application
server = serve sapi (
       (return (StreamGenerator (\f r -> f alice >> r bob >> r alice))
       :: Handler (StreamGenerator Person))
       :<|>
       (return (StreamGenerator (\f r -> f alice >> r bob >> r alice))
       :: Handler (StreamGenerator Person))
       :<|>
       emptyServer)


{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: ClientM a -> BaseUrl -> IO (Either ServantError a)
runClient x baseUrl' = runClientM x (mkClientEnv manager' baseUrl')

runResultStream :: ResultStream a -> IO (Maybe (Either String a), Maybe (Either String a), Maybe (Either String a), Maybe (Either String a))
runResultStream (ResultStream k) = k $ \act -> (,,,) <$> act <*> act <*> act <*> act

streamSpec :: Spec
streamSpec = beforeAll (CS.startWaiApp server) $ afterAll CS.endWaiApp $ do

    it "Servant.API.StreamGet.Newline" $ \(_, baseUrl) -> do
       Right res <- runClient getGetNL baseUrl
       let jra = Just (Right alice)
           jrb = Just (Right bob)
       runResultStream res `shouldReturn` (jra, jrb, jra, Nothing)

    it "Servant.API.StreamGet.Netstring" $ \(_, baseUrl) -> do
       Right res <- runClient getGetNS baseUrl
       let jra = Just (Right alice)
           jrb = Just (Right bob)
       runResultStream res `shouldReturn` (jra, jrb, jra, Nothing)
