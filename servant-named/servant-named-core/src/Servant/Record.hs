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

module Servant.Record (RecordParam, UnRecordParam, GHasLink, genericToLink) where

import Data.Kind (Type)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant.API
import Servant.Symbols (Exp, Modify)

-- import Servant.Symbols (Exp, Modify)

-- | 'RecordParam' uses the fields in the record to represent the
-- parameters.  The name of the field is used as parameter name, and
-- the type is the return type.  For example, this api:
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
-- data UserParams = UserParams
--   { category :: Maybe Category
--   , sort_by :: Sortby
--   , with_schema :: Bool
--   , filters :: [Filter]
--   }
--
-- type API = "users" :> RecordParam UserParams :> Get '[JSON] User
-- @
data RecordParam (mkExp :: Symbol -> Exp Symbol) (a :: Type)

type family ServantAppend x y where
  ServantAppend (a :> b) c = a :> ServantAppend b c
  ServantAppend a c = a :> c

-- | Type family to rewrite a RecordParam Api to a regular servant API.
-- Useful to define instances for classes that extract information from
-- the API type, such as Servant.Swagger, or servant-foreign.
--
-- Typical use:
--
-- > instance SomeClass (UnRecordParam (RecordParam mkExp a :> api))) =>
-- >          SomeClass (RecordParam mkExp a :> api) where
-- >    someMethod _ =
-- >      someMethod (Proxy :: Proxy (UnRecordParam (RecordParam mkExp a :> api))
type family UnRecordParam (mkExp :: Symbol -> Exp Symbol) (x :: Type) :: Type where
  UnRecordParam mkExp (a :> b) = ServantAppend (UnRecordParam mkExp a) b
  UnRecordParam mkExp (RecordParam mkExp a) = UnRecordParam mkExp (Rep a ())
  UnRecordParam mkExp (D1 m c d) = UnRecordParam mkExp (c d)
  UnRecordParam mkExp ((a :*: b) d) =
    ServantAppend
      (UnRecordParam mkExp (a d))
      (UnRecordParam mkExp (b d))
  UnRecordParam mkExp (C1 m a d) = UnRecordParam mkExp (a d)
  UnRecordParam mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool) d) =
    QueryFlag (Modify (mkExp sym))
  UnRecordParam mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]) d) =
    QueryParams (Modify (mkExp sym)) a
  UnRecordParam mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)) d) =
    QueryParam' [Optional, Strict] (Modify (mkExp sym)) a
  UnRecordParam mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a) d) =
    QueryParam' [Required, Strict] (Modify (mkExp sym)) a

instance (Generic a, GHasLink mkExp (Rep a) sub) => HasLink (RecordParam mkExp a :> sub) where
  type MkLink (RecordParam mkExp a :> sub) b = a -> MkLink sub b
  toLink toA _ l record = gToLink (Proxy :: Proxy mkExp) toA (Proxy :: Proxy sub) l (from record :: Rep a ())

genericToLink :: forall mkExp a sub b. (Generic a, GHasLink mkExp (Rep a) sub) => (Link -> b) -> Proxy (RecordParam mkExp a :> sub) -> Link -> a -> MkLink sub b
genericToLink toA _ l record = gToLink (Proxy :: Proxy mkExp) toA (Proxy :: Proxy sub) l (from record :: Rep a ())

data GParam (mkExp :: Symbol -> Exp Symbol) a

instance GHasLink mkExp a sub => HasLink (GParam mkExp (a ()) :> sub) where
  type MkLink (GParam mkExp (a ()) :> sub) b = a () -> MkLink sub b
  toLink toA _ = gToLink (Proxy :: Proxy mkExp) toA (Proxy :: Proxy sub)
  {-# INLINE toLink #-}

class HasLink sub => GHasLink (mkExp :: Symbol -> Exp Symbol) (a :: Type -> Type) sub where
  gToLink :: Proxy mkExp -> (Link -> b) -> Proxy sub -> Link -> a () -> MkLink sub b

instance GHasLink mkExp c sub => GHasLink mkExp (D1 m c) sub where
  gToLink _ toA _ l (M1 x) = gToLink (Proxy :: Proxy mkExp) toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance
  ( HasLink sub
  , GHasLink mkExp a (GParam mkExp (b ()) :> sub)
  ) =>
  GHasLink mkExp (a :*: b) sub
  where
  gToLink _ toA _ l (a :*: b) =
    gToLink (Proxy :: Proxy mkExp) toA (Proxy :: Proxy (GParam mkExp (b ()) :> sub)) l a b
  {-# INLINE gToLink #-}

instance
  ( GHasLink mkExp a sub
  , HasLink sub
  ) =>
  GHasLink mkExp (C1 m a) sub
  where
  gToLink _ toA _ l (M1 x) = gToLink (Proxy :: Proxy mkExp) toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  , HasLink sub
  ) =>
  GHasLink mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryFlag (Modify (mkExp sym)) :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParams (Modify (mkExp sym)) a :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a))) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Modify (mkExp sym)) a :> sub)) l x
  {-# INLINE gToLink #-}

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub
  ) =>
  GHasLink mkExp (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a)) sub
  where
  gToLink _ toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Required, Strict] (Modify (mkExp sym)) a :> sub)) l x
  {-# INLINE gToLink #-}