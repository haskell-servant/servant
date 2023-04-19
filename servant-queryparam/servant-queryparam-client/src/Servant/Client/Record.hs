{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports orphan instances to make
-- [@servant-queryparam-core@](https://hackage.haskell.org/package/servant-queryparam-core) work with clients.
module Servant.Client.Record (GHasClient, RunClient, genericClientWithRoute, genericHoistClientMonad) where

import Data.Kind (Type)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.Client.Core.HasClient
import Servant.Client.Core.Request
import Servant.Client.Core.RunClient
import Servant.Record
import Servant.Symbols

instance
  ( RunClient m
  , Generic a
  , GHasClient mod m (Rep a) api
  ) =>
  HasClient m (RecordParam mod a :> api)
  where
  type Client m (RecordParam mod a :> api) = a -> Client m api
  clientWithRoute pm Proxy req record =
    gClientWithRoute (Proxy :: Proxy mod) pm (Proxy :: Proxy api) req (from record :: Rep a ())
  {-# INLINE clientWithRoute #-}
  hoistClientMonad pm Proxy f cl as =
    gHoistClientMonad
      (Proxy :: Proxy mod)
      pm
      (Proxy :: Proxy api)
      f
      (cl . to)
      (from as :: Rep a ())
  {-# INLINE hoistClientMonad #-}

data GParam (mod :: Symbol -> Exp Symbol) a

class GHasClient (mod :: Symbol -> Exp Symbol) m (a :: Type -> Type) api where
  gClientWithRoute ::
    RunClient m =>
    Proxy mod ->
    Proxy m ->
    Proxy api ->
    Request ->
    a () ->
    Client m api
  gHoistClientMonad ::
    RunClient m =>
    Proxy mod ->
    Proxy m ->
    Proxy api ->
    (forall x. mon x -> mon' x) ->
    (a () -> Client mon api) ->
    (a () -> Client mon' api)

genericClientWithRoute ::
  forall mod m a api.
  ( GHasClient mod m a api
  , RunClient m
  ) =>
  Proxy mod ->
  Proxy m ->
  Proxy api ->
  Request ->
  a () ->
  Client m api
genericClientWithRoute = gClientWithRoute

genericHoistClientMonad ::
  forall mod m a api mon mon'.
  ( GHasClient mod m a api
  , RunClient m
  ) =>
  RunClient m =>
  Proxy mod ->
  Proxy m ->
  Proxy api ->
  (forall x. mon x -> mon' x) ->
  (a () -> Client mon api) ->
  (a () -> Client mon' api)
genericHoistClientMonad = gHoistClientMonad

instance
  ( RunClient m
  , GHasClient mod m a api
  ) =>
  HasClient m (GParam mod (a ()) :> api)
  where
  type Client m (GParam mod (a ()) :> api) = a () -> Client m api
  clientWithRoute pm _ = gClientWithRoute (Proxy :: Proxy mod) pm (Proxy :: Proxy api)
  {-# INLINE clientWithRoute #-}
  hoistClientMonad pm _ = gHoistClientMonad (Proxy :: Proxy mod) pm (Proxy :: Proxy api)
  {-# INLINE hoistClientMonad #-}

instance
  GHasClient mod m c api =>
  GHasClient mod m (D1 m3 c) api
  where
  gClientWithRoute _ pm _ req (M1 x) =
    gClientWithRoute (Proxy :: Proxy mod) pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm Proxy f cl x =
    gHoistClientMonad (Proxy :: Proxy mod) pm (Proxy :: Proxy api) f (cl . M1) (unM1 x)
  {-# INLINE gHoistClientMonad #-}

instance
  GHasClient mod m a (GParam mod (b ()) :> api) =>
  GHasClient mod m (a :*: b) api
  where
  gClientWithRoute _ pm _ req (x :*: y) =
    gClientWithRoute (Proxy :: Proxy mod) pm (Proxy :: Proxy (GParam mod (b ()) :> api)) req x y
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm Proxy f cl (x :*: y) =
    gHoistClientMonad
      (Proxy :: Proxy mod)
      pm
      (Proxy :: Proxy (GParam mod (b ()) :> api))
      f
      (\x' y' -> cl (x' :*: y'))
      x
      y
  {-# INLINE gHoistClientMonad #-}

instance GHasClient mod m a api => GHasClient mod m (C1 mon a) api where
  gClientWithRoute _ pm _ req (M1 x) =
    gClientWithRoute (Proxy :: Proxy mod) pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 x) =
    gHoistClientMonad (Proxy :: Proxy mod) pm (Proxy :: Proxy api) f (cl . M1) x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  ) =>
  GHasClient mod m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryFlag (Eval (mod sym)) :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryFlag (Eval (mod sym)) :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  ) =>
  GHasClient mod m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryParams (Eval (mod sym)) a :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParams (Eval (mod sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  ) =>
  GHasClient
    mod
    m
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)))
    api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute
      pm
      (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (mod sym)) a :> api))
      req
      x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Eval (mod sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPABLE #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  ) =>
  GHasClient
    mod
    m
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a))
    api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute
      pm
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (mod sym)) a :> api))
      req
      x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (mod sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}
