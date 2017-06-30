{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.FreerClient where

import           Servant.API
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Http
import           Control.Monad.Freer.Reader
import           Data.ByteString.Lazy hiding (pack, filter, map, null, elem, any)
import           Data.Proxy
import           Network.HTTP.Client (Response)
import           Network.HTTP.Media
import qualified Network.HTTP.Types         as H
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant.Client hiding (ClientM, clientWithRoute)
import           Servant.Common.Req hiding (ClientM, runClientM', performRequest)
import           Servant.FreerReq

freeClient :: HasFreeClient r api => Proxy r -> Proxy api -> FreeClient r api
freeClient r p = clientWithRoute r p defReq

class HasFreeClient r api where
  type FreeClient (r :: [* -> *]) api :: *
  clientWithRoute :: Proxy r -> Proxy api -> Req -> FreeClient r api

instance ( Member Http r
         , Member (Reader ClientEnv) r
         , Member (Exc ServantError) r) => HasFreeClient r Raw where
  type FreeClient r Raw
    = H.Method -> ClientM r ( Int
                            , ByteString
                            , MediaType
                            , [HTTP.Header]
                            , Response ByteString)

  clientWithRoute :: Proxy r -> Proxy Raw -> Req -> FreeClient r Raw
  clientWithRoute Proxy Proxy req httpMethod = do
    performRequest httpMethod req
