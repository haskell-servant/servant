{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Server.Experimental.Auth where

import           Control.Monad.Trans
                 (liftIO)
import           Data.Proxy
                 (Proxy (Proxy))
import           Data.Typeable
                 (Typeable)
import           GHC.Generics
                 (Generic)
import           Network.Wai
                 (Request)

import           Servant
                 ((:>))
import           Servant.API.Experimental.Auth
import           Servant.Server.Internal
                 (HasServer (..))
import           Servant.Server.Internal.Handler
                 (Handler, runHandler)
import           Servant.Server.Internal.RoutingApplication
                 (DelayedIO, addAuthCheck, delayedFailFatal, withRequest)

-- * General Auth

-- | Specify the type of data returned after we've authenticated a request.
-- quite often this is some `User` datatype.
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
type family AuthServerData a :: *

-- | Handlers for AuthProtected resources
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
newtype AuthHandler r usr = AuthHandler
  { unAuthHandler :: r -> Handler usr }
  deriving (Generic, Typeable)

-- | NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
mkAuthHandler :: (r -> Handler usr) -> AuthHandler r usr
mkAuthHandler = AuthHandler

-- | "TODO": We'd like to have functiona dependency here
class HasAuthHandler context tag where
    getAuthHandler :: context -> Proxy tag -> AuthHandler Request (AuthServerData (AuthProtect tag))

instance d ~ AuthServerData (AuthProtect tag)  => HasAuthHandler (AuthHandler Request d) tag where
    getAuthHandler ctx _ = ctx

-- | Known orphan instance.
instance ( HasServer api context
         , HasAuthHandler context tag
         )
  => HasServer (AuthProtect tag :> api) context where

  type ServerT (AuthProtect tag :> api) m =
    AuthServerData (AuthProtect tag) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` withRequest authCheck)
      where
        authHandler :: Request -> Handler (AuthServerData (AuthProtect tag))
        authHandler = unAuthHandler (getAuthHandler context (Proxy :: Proxy tag))
        authCheck :: Request -> DelayedIO (AuthServerData (AuthProtect tag))
        authCheck = (>>= either delayedFailFatal return) . liftIO . runHandler . authHandler
