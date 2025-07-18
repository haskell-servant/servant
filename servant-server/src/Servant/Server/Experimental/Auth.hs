{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Server.Experimental.Auth where

import Control.Monad.Trans (liftIO)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Wai (Request)
import Servant.API.Experimental.Auth

import Servant ((:>))
import Servant.Server.Internal
  ( DelayedIO
  , Handler
  , HasContextEntry
  , HasServer (..)
  , addAuthCheck
  , delayedFailFatal
  , getContextEntry
  , runHandler
  , withRequest
  )

-- * General Auth

-- | Specify the type of data returned after we've authenticated a request.
-- quite often this is some `User` datatype.
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
type family AuthServerData a :: Type

-- | Handlers for AuthProtected resources
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
newtype AuthHandler r usr = AuthHandler
  {unAuthHandler :: r -> Handler usr}
  deriving (Functor, Generic, Typeable)

-- | NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
mkAuthHandler :: (r -> Handler usr) -> AuthHandler r usr
mkAuthHandler = AuthHandler

-- | Known orphan instance.
instance
  ( HasContextEntry context (AuthHandler Request (AuthServerData (AuthProtect tag)))
  , HasServer api context
  )
  => HasServer (AuthProtect tag :> api) context
  where
  type
    ServerT (AuthProtect tag :> api) m =
      AuthServerData (AuthProtect tag) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler :: Request -> Handler (AuthServerData (AuthProtect tag))
      authHandler = unAuthHandler (getContextEntry context)
      authCheck :: Request -> DelayedIO (AuthServerData (AuthProtect tag))
      authCheck = (>>= either delayedFailFatal return) . liftIO . runHandler . authHandler
