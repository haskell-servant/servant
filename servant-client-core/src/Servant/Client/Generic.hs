{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Client.Generic
  ( AsClientT
  , genericClient
  , genericClientHoist
  )
where

import Data.Proxy (Proxy (..))
import Servant.API.Generic

import Servant.Client.Core
import Servant.Client.Core.HasClient (AsClientT)

-- | Generate a record of client functions.
genericClient
  :: forall routes m
   . ( Client m (ToServantApi routes) ~ ToServant routes (AsClientT m)
     , GenericServant routes (AsClientT m)
     , HasClient m (ToServantApi routes)
     )
  => routes (AsClientT m)
genericClient =
  fromServant $
    clientIn (Proxy :: Proxy (ToServantApi routes)) (Proxy :: Proxy m)

-- | 'genericClient' but with 'hoistClientMonad' in between.
genericClientHoist
  :: forall routes m n
   . ( Client n (ToServantApi routes) ~ ToServant routes (AsClientT n)
     , GenericServant routes (AsClientT n)
     , HasClient m (ToServantApi routes)
     )
  => (forall x. m x -> n x)
  -- ^ natural transformation
  -> routes (AsClientT n)
genericClientHoist nt =
  fromServant $
    hoistClientMonad m api nt $
      clientIn api m
  where
    m = Proxy :: Proxy m
    api = Proxy :: Proxy (ToServantApi routes)
