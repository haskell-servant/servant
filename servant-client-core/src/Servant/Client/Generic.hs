{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module  Servant.Client.Generic (
    AsClientT,
    genericClient,
    genericClientHoist,
    ) where

import           Data.Constraint (Dict(..))
import           Data.Proxy
                 (Proxy (..))

import           Servant.API.Generic
import           Servant.Client.Core

-- | A type that specifies that an API record contains a client implementation.
data AsClientT (m :: * -> *)
instance GenericMode (AsClientT m) where
    type AsClientT m :- api = Client m api

-- | Generate a record of client functions.
genericClient
    :: forall routes m.
       ( HasClient m (ToServantApi routes)
       , GenericServant routes (AsClientT m)
       , Client m (ToServantApi routes) ~ ToServant routes (AsClientT m)
       )
    => routes (AsClientT m)
genericClient
    = fromServant
    $ clientIn (Proxy :: Proxy (ToServantApi routes)) (Proxy :: Proxy m)

-- | 'genericClient' but with 'hoistClientMonad' in between.
genericClientHoist
    :: forall routes m n.
       ( HasClient m (ToServantApi routes)
       , GenericServant routes (AsClientT n)
       , Client n (ToServantApi routes) ~ ToServant routes (AsClientT n)
       )
    => (forall x. m x -> n x)  -- ^ natural transformation
    -> routes (AsClientT n)
genericClientHoist nt
    = fromServant
    $ hoistClientMonad m api nt
    $ clientIn api m
  where
    m = Proxy :: Proxy m
    api = Proxy :: Proxy (ToServantApi routes)

type GClientConstraints api m =
  ( GenericServant api (AsClientT m)
  , Client m (ToServantApi api) ~ ToServant api (AsClientT m)
  )

class GClient (api :: * -> *) m where
  proof :: Dict (GClientConstraints api m)

instance GClientConstraints api m => GClient api m where
  proof = Dict

instance
  ( forall n. GClient api n
  , HasClient m (ToServantApi api)
  , RunClient m
  )
  => HasClient m (NamedRoutes api) where
  type Client m (NamedRoutes api) = api (AsClientT m)

  clientWithRoute :: Proxy m -> Proxy (NamedRoutes api) -> Request -> Client m (NamedRoutes api)
  clientWithRoute pm _ request =
    case proof @api @m of
      Dict -> fromServant $ clientWithRoute  pm (Proxy @(ToServantApi api)) request

  hoistClientMonad
    :: forall ma mb.
       Proxy m
    -> Proxy (NamedRoutes api)
    -> (forall x. ma x -> mb x)
    -> Client ma (NamedRoutes api)
    -> Client mb (NamedRoutes api)
  hoistClientMonad _ _ nat clientA =
    case (proof @api @ma, proof @api @mb) of
      (Dict, Dict) ->
        fromServant @api @(AsClientT mb) $
        hoistClientMonad @m @(ToServantApi api) @ma @mb Proxy Proxy nat $
        toServant @api @(AsClientT ma) clientA
