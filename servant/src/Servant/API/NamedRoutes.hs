{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_HADDOCK not-home #-}

module Servant.API.NamedRoutes
  ( -- * NamedRoutes combinator
    NamedRoutes
  ) where

import Data.Kind (Type)

-- | Combinator for embedding a record of named routes into a Servant API type.
data NamedRoutes (api :: Type -> Type)
