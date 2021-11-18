{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | @since 0.14.1
module Servant.Server.Generic (
    AsServerT,
    AsServer,
    genericServe,
    genericServeT,
    genericServeTWithContext,
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

-- | Transform a record of routes into a WAI 'Application'.
genericServe
    :: forall routes.
       ( HasServer (ToServantApi routes) '[]
       , GenericServant routes AsServer
       , Server (ToServantApi routes) ~ ToServant routes AsServer
       )
    => routes AsServer -> Application
genericServe = serve (Proxy :: Proxy (ToServantApi routes))  . genericServer

-- | Transform a record of routes with custom monad into a WAI 'Application',
--   by providing a transformation to bring each handler back in the 'Handler'
--   monad.
genericServeT
  :: forall (routes :: * -> *) (m :: * -> *).
     ( GenericServant routes (AsServerT m)
     , GenericServant routes AsApi
     , HasServer (ToServantApi routes) '[]
     , ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
     )
  => (forall a. m a -> Handler a) -- ^ 'hoistServer' argument to come back to 'Handler'
  -> routes (AsServerT m)         -- ^ your record full of request handlers
  -> Application
genericServeT f server = serve p $ hoistServer p f (genericServerT server)
  where
    p = genericApi (Proxy :: Proxy routes)

-- | Transform a record of routes with custom monad into a WAI 'Application',
--   while using the given 'Context' to serve the application (contexts are typically
--   used by auth-related combinators in servant, e.g to hold auth checks) and the given
--   transformation to map all the handlers back to the 'Handler' monad.
genericServeTWithContext
  :: forall (routes :: * -> *) (m :: * -> *) (ctx :: [*]).
     ( GenericServant routes (AsServerT m)
     , GenericServant routes AsApi
     , HasServer (ToServantApi routes) ctx
     , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
     , ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
     )
  => (forall a. m a -> Handler a) -- ^ 'hoistServer' argument to come back to 'Handler'
  -> routes (AsServerT m)         -- ^ your record full of request handlers
  -> Context ctx                  -- ^ the 'Context' to serve the application with
  -> Application
genericServeTWithContext f server ctx =
  serveWithContext p ctx $
  hoistServerWithContext p pctx f (genericServerT server)
  where
    p = genericApi (Proxy :: Proxy routes)
    pctx = Proxy :: Proxy ctx

-- | Transform a record of endpoints into a 'Server'.
genericServer
    :: GenericServant routes AsServer
    => routes AsServer
    -> ToServant routes AsServer
genericServer = toServant

-- | Transform a record of endpoints into a @'ServerT' m@.
--
--  You can see an example usage of this function
--  <https://docs.servant.dev/en/stable/cookbook/generic/Generic.html#using-generics-together-with-a-custom-monad in the Servant Cookbook>.
genericServerT
    :: GenericServant routes (AsServerT m)
    => routes (AsServerT m)
    -> ToServant routes (AsServerT m)
genericServerT = toServant
