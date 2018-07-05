{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | @since 0.14.1
module Servant.Server.Generic (
    AsServerT,
    AsServer,
    genericServe,
    genericServer,
    genericServerT,
  ) where

import           Data.Proxy
                 (Proxy (..))

import           Servant.API.Generic
import           Servant.Server

-- | A type that specifies that an API record contains a server implementation.
data AsServerT (m :: * -> *)
instance GenericMode (AsServerT m) where
    type AsServerT m :- api = ServerT api m

type AsServer = AsServerT Handler

-- | Transform record of routes into a WAI 'Application'.
genericServe
    :: forall routes.
       ( HasServer (ToServantApi routes) '[]
       , GenericServant routes AsServer
       , Server (ToServantApi routes) ~ ToServant routes AsServer
       )
    => routes AsServer -> Application
genericServe = serve (Proxy :: Proxy (ToServantApi routes))  . genericServer

-- | Transform record of endpoints into a 'Server'.
genericServer
    :: GenericServant routes AsServer
    => routes AsServer
    -> ToServant routes AsServer
genericServer = toServant

genericServerT
    :: GenericServant routes (AsServerT m)
    => routes (AsServerT m)
    -> ToServant routes (AsServerT m)
genericServerT = toServant
