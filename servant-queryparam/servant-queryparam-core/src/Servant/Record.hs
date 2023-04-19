{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module provides functions and instances for working with query parameter records.
module Servant.Record (RecordParam, UnRecordParam) where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.Symbols

-- | 'RecordParam' uses fields in a record to represent query parameters.
--
-- - Modified record field names become query parameter names.
-- - Types of record fields become query parameter return types.
--
-- For example, this API:
--
-- @
-- type API = "users" :> (QueryParam "category" Category :>
--                        QueryParam' '[Required, Strict] "sort_by" SortBy :>
--                        QueryFlag "with_schema" :>
--                        QueryParams "filters" Filter :>
--                        Get '[JSON] User
-- @
--
-- can be written with records:
--
-- @
-- data DropPrefixExp :: sym -> 'Exp' sym
--
-- type instance 'Eval' (DropPrefixExp sym) = 'DropPrefix' sym
--
-- data UserParams = UserParams
--   { _userParams_category :: Maybe Category
--   , _userParams_sort_by :: SortBy
--   , _userParams_with_schema :: Bool
--   , _userParams_filters :: [Filter]
--   }
--
-- type API = "users" :> 'RecordParam' DropPrefixExp UserParams :> Get '[JSON] User
-- @
--
-- Here, @DropPrefixExp@ wraps a @sym@ into @Exp@.
--
-- The instance of 'Eval' for @DropPrefixExp sym@ drops the prefix of that @sym@ via 'DropPrefix'.
--
-- 'DropPrefix' will be applied to the fields of @UserParams@.
--
-- Then, the @_userParams_category@ record field will correspond to the query parameter @"category"@.
data RecordParam (mod :: Symbol -> Exp Symbol) (a :: Type)

type family ServantAppend x y where
  ServantAppend (a :> b) c = a :> ServantAppend b c
  ServantAppend a c = a :> c

-- | Type family for rewriting a 'RecordParam' API to a regular @servant@ API.
-- This family is useful for defining instances of classes that extract information from the API type,
-- such as classes from @servant-swagger@ or @servant-foreign@.
--
-- Typical use:
--
-- @
-- instance SomeClass (UnRecordParam (RecordParam mod a :> api))) =>
--          SomeClass (RecordParam mod a :> api) where
--    someMethod _ =
--      someMethod (Proxy :: Proxy (UnRecordParam (RecordParam mod a :> api))
-- @
type family UnRecordParam (mod :: Symbol -> Exp Symbol) (x :: Type) :: Type where
  UnRecordParam mod (a :> b) = ServantAppend (UnRecordParam mod a) b
  UnRecordParam mod (RecordParam mod a) = UnRecordParam mod (Rep a ())
  UnRecordParam mod (D1 m c d) = UnRecordParam mod (c d)
  UnRecordParam mod ((a :*: b) d) =
    ServantAppend
      (UnRecordParam mod (a d))
      (UnRecordParam mod (b d))
  UnRecordParam mod (C1 m a d) = UnRecordParam mod (a d)
  UnRecordParam mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool) d) =
    QueryFlag (Eval (mod sym))
  UnRecordParam mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]) d) =
    QueryParams (Eval (mod sym)) a
  UnRecordParam mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)) d) =
    QueryParam' [Optional, Strict] (Eval (mod sym)) a
  UnRecordParam mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a) d) =
    QueryParam' [Required, Strict] (Eval (mod sym)) a

instance (Generic a, GHasLink mod (Rep a) sub) => HasLink (RecordParam mod a :> sub) where
  type MkLink (RecordParam mod a :> sub) b = a -> MkLink sub b
  toLink toA _ l record = gToLink (Proxy :: Proxy mod) toA (Proxy :: Proxy sub) l (from record :: Rep a ())

data GParam (mod :: Symbol -> Exp Symbol) a

instance GHasLink mod a sub => HasLink (GParam mod (a ()) :> sub) where
  type MkLink (GParam mod (a ()) :> sub) b = a () -> MkLink sub b
  toLink toA _ = gToLink (Proxy :: Proxy mod) toA (Proxy :: Proxy sub)
  {-# INLINE toLink #-}

class HasLink sub => GHasLink (mod :: Symbol -> Exp Symbol) (a :: Type -> Type) sub where
  gToLink :: Proxy mod -> (Link -> b) -> Proxy sub -> Link -> a () -> MkLink sub b

instance GHasLink mod c sub => GHasLink mod (D1 m c) sub where
  gToLink _ toA _ l (M1 x) = gToLink (Proxy :: Proxy mod) toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance
  ( HasLink sub
  , GHasLink mod a (GParam mod (b ()) :> sub)
  ) =>
  GHasLink mod (a :*: b) sub
  where
  gToLink _ toA _ l (a :*: b) =
    gToLink (Proxy :: Proxy mod) toA (Proxy :: Proxy (GParam mod (b ()) :> sub)) l a b
  {-# INLINE gToLink #-}

instance
  ( GHasLink mod a sub
  , HasLink sub
  ) =>
  GHasLink mod (C1 m a) sub
  where
  gToLink _ toA _ l (M1 x) = gToLink (Proxy :: Proxy mod) toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  , HasLink sub
  ) =>
  GHasLink mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryFlag (Eval (mod sym)) :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParams (Eval (mod sym)) a :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a))) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (mod sym)) a :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink mod (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a)) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (mod sym)) a :> sub)) l x
  {-# INLINE gToLink #-}