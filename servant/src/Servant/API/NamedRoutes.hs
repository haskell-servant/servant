{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK not-home     #-}

module Servant.API.NamedRoutes (
    -- * NamedRoutes combinator
    NamedRoutes
    -- * Term-level helpers
  , namedRoute
  ) where

import Data.Proxy (Proxy(..))
import GHC.Records (HasField)
import GHC.TypeLits (Symbol)

import Servant.API.Generic (AsApi)

namedRoute
  :: forall (field :: Symbol) (api :: * -> *) a.
     HasField field (api AsApi) a
  => Proxy a
namedRoute = Proxy

-- | Combinator for embedding a record of named routes into a Servant API type.
data NamedRoutes (api :: * -> *)
