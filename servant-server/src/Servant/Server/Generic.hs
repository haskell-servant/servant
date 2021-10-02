{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints  #-}
#endif

-- | @since 0.14.1
module Servant.Server.Generic (
    AsServerT,
    AsServer,
    genericServe,
    genericServeT,
    genericServeTWithContext,
    genericServer,
    genericServerT,
    -- * Internal machinery
    GServerConstraints,
    GServer,
    -- * Re-exports
    NamedRoutes
  ) where

import           Data.Constraint
import           Data.Proxy
                 (Proxy (..))

import           Servant.API.Generic
import           Servant.Server
import           Servant.Server.Internal

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

#if __GLASGOW_HASKELL__ >= 806

-- | Set of constraints required to convert to / from vanilla server types.
type GServerConstraints api m =
  ( ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  , GServantProduct (Rep (api (AsServerT m)))
  )

-- | This class is a necessary evil: in the implementation of 'HasServer' for
--  @'NamedRoutes' api@, we essentially need the quantified constraint @forall
--  m. 'GServerConstraints' m@ to hold.
--
-- We cannot require do that directly as the definition of 'GServerConstraints'
-- contains type family applications ('Rep' and 'ServerT'). The trick is to hide
-- those type family applications behind a typeclass providing evidence for
-- @'GServerConstraints' api m@ in the form of a dictionary, and require that
-- @forall m. 'GServer' api m@ instead.
--
-- Users shouldn't have to worry about this class, as the only possible instance
-- is provided in this module for all record APIs.

class GServer (api :: * -> *) (m :: * -> *) where
  proof :: Dict (GServerConstraints api m)

instance
  ( ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  , GServantProduct (Rep (api (AsServerT m)))
  ) => GServer api m where
  proof = Dict

instance
  ( HasServer (ToServantApi api) context
  , forall m. Generic (api (AsServerT m))
  , forall m. GServer api m
  ) => HasServer (NamedRoutes api) context where

  type ServerT (NamedRoutes api) m = api (AsServerT m)

  route
    :: Proxy (NamedRoutes api)
    -> Context context
    -> Delayed env (api (AsServerT Handler))
    -> Router env
  route _ ctx delayed =
    case proof @api @Handler of
      Dict -> route (Proxy @(ToServantApi api)) ctx (toServant <$> delayed)

  hoistServerWithContext
    :: forall m n. Proxy (NamedRoutes api)
    -> Proxy context
    -> (forall x. m x -> n x)
    -> api (AsServerT m)
    -> api (AsServerT n)
  hoistServerWithContext _ pctx nat server =
    case (proof @api @m, proof @api @n) of
      (Dict, Dict) ->
        fromServant servantSrvN
        where
          servantSrvM :: ServerT (ToServantApi api) m =
            toServant server
          servantSrvN :: ServerT (ToServantApi api) n =
            hoistServerWithContext (Proxy @(ToServantApi api)) pctx nat servantSrvM

#endif
