{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module  Servant.Client.Generic (
    AsClientT,
    genericClient,
    genericClientHoist,
    ) where

import           Data.Proxy
                 (Proxy (..))

import           Servant.API.Generic
import           Servant.Client.Core
import           Servant.Client.Core.HasClient (AsClientT)

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
