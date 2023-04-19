{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This module uses the [@named@](https://hackage.haskell.org/package/named) package to match names with query parameters.
-- For example, this API:
--
-- @
-- type API = "users" :> (QueryParam "category" Category :>
--                        QueryParam' '[Required, Strict] "sort_by" SortBy :>
--                        QueryFlag "with_schema" :>
--                        QueryParams "filters" Filter :>
--                        Get '[JSON] User)
-- @
--
-- can be written as:
--
-- @
-- type API = "users" :> (OptionalQueryParam "category" Category :>
--                        NamedQueryParam "sort_by" SortBy :>
--                        NamedQueryFlag "with_schema" :>
--                        NamedQueryParams "filters" Filter :>
--                        Get '[JSON] User)
-- @
--
-- The [@servant-queryparam-client@](https://hackage.haskell.org/package/servant-queryparam-client)
-- and [@servant-queryparam-server@](https://hackage.haskell.org/package/servant-queryparam-server) provide
-- functions that match the names with query parameters.
module Servant.Named (
  NamedQueryParam,
  UnNameParam,
  OptionalQueryParam,
  NamedQueryParams,
  NamedQueryFlag,
  NamedQueryParam',
  unarg,
) where

import Data.Functor.Identity
import Data.Kind (Type)
import Data.Maybe
import Data.Proxy
import GHC.TypeLits
import Named
import Servant.API
import Servant.API.Modifiers

-- | Like `QueryParam'`, but instead of extracting a type @a@, it
-- extracts a named type @`NamedF` f a sym@, where the name
-- corresponds to the query parameter string.
data NamedQueryParam' (mods :: [Type]) (sym :: Symbol) (a :: Type)

-- | Extract a wrapped value from 'NamedF'.
unarg :: NamedF f a name -> f a
unarg (ArgF a) = a

-- | Type family for converting a named query parameter to a regular query parameter.
-- This family is useful for defining instances of classes that extract information from the API type,
-- such as classes from @servant-swagger@ or @servant-foreign@.
type family UnNameParam x where
  UnNameParam (NamedQueryParams sym a) = QueryParams sym a
  UnNameParam (NamedQueryParam' mods sym a) = QueryParam' mods sym a
  UnNameParam (NamedQueryFlag sym) = QueryFlag sym

instance
  ( KnownSymbol sym
  , ToHttpApiData v
  , HasLink sub
  , SBoolI (FoldRequired mods)
  ) =>
  HasLink (NamedQueryParam' mods sym v :> sub)
  where
  type
    MkLink (NamedQueryParam' mods sym v :> sub) a =
      If (FoldRequired mods) (sym :! v) (sym :? v) -> MkLink sub a
  toLink toA _ l qparam =
    toLink toA (Proxy :: Proxy (QueryParam' mods sym v :> sub)) l $
      case sbool :: SBool (FoldRequired mods) of
        STrue -> runIdentity (unarg qparam)
        SFalse -> unarg qparam

-- | Lookup the value associated to the sym query string parameter and
-- try to extract it as an optional named argument of type @sym `:?`
-- a@.
type OptionalQueryParam = NamedQueryParam' [Optional, Strict]

-- | Like `QueryParam`, but instead of extracting a type @a@, it
-- extracts a named type @named `:!` a@, where @named@ corresponds to
-- the query parameter string.
type NamedQueryParam = NamedQueryParam' [Required, Strict]

-- | Like `QueryParams`, but extracts a named type @named `:?` [a]@
-- instead, where @named@ corresponds to the query parameter string.
-- The default value is the empty list @[]@
data NamedQueryParams (sym :: Symbol) (a :: Type)

instance
  (KnownSymbol sym, ToHttpApiData v, HasLink sub) =>
  HasLink (NamedQueryParams sym v :> sub)
  where
  type MkLink (NamedQueryParams sym v :> sub) a = sym :? [v] -> MkLink sub a
  toLink toA _ l (ArgF params) =
    toLink toA (Proxy :: Proxy (QueryParams sym v :> sub)) l $
      fromMaybe [] params

-- | Like 'QueryFlag', but extracts a named type @named `:?` Bool@ instead,
-- where @named@ corresponds to the query parameter string.
-- The default value is 'False'.
data NamedQueryFlag (sym :: Symbol)

instance
  ( KnownSymbol sym
  , HasLink sub
  ) =>
  HasLink (NamedQueryFlag sym :> sub)
  where
  type
    MkLink (NamedQueryFlag sym :> sub) a =
      (sym :? Bool) -> MkLink sub a
  toLink toA _ l (ArgF qparam) =
    toLink toA (Proxy :: Proxy (QueryFlag sym :> sub)) l $
      fromMaybe False qparam
