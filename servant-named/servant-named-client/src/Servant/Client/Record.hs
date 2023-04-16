{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | -- | This module just exports orphan instances to make named-servant
-- work with clients.  See that package for documentation.
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
import Servant.Symbols (Exp, Modify)

instance
  ( RunClient m
  , Generic a
  , GHasClient mkExp m (Rep a) api
  ) =>
  HasClient m (RecordParam mkExp a :> api)
  where
  type Client m (RecordParam mkExp a :> api) = a -> Client m api
  clientWithRoute pm Proxy req record =
    gClientWithRoute (Proxy :: Proxy mkExp) pm (Proxy :: Proxy api) req (from record :: Rep a ())
  {-# INLINE clientWithRoute #-}
  hoistClientMonad pm Proxy f cl as =
    gHoistClientMonad
      (Proxy :: Proxy mkExp)
      pm
      (Proxy :: Proxy api)
      f
      (cl . to)
      (from as :: Rep a ())
  {-# INLINE hoistClientMonad #-}

data GParam (mkExp :: Symbol -> Exp Symbol) a

class GHasClient (mkExp :: Symbol -> Exp Symbol) m (a :: Type -> Type) api where
  gClientWithRoute ::
    RunClient m =>
    Proxy mkExp ->
    Proxy m ->
    Proxy api ->
    Request ->
    a () ->
    Client m api
  gHoistClientMonad ::
    RunClient m =>
    Proxy mkExp ->
    Proxy m ->
    Proxy api ->
    (forall x. mon x -> mon' x) ->
    (a () -> Client mon api) ->
    (a () -> Client mon' api)

genericClientWithRoute ::
  forall mkExp m a api.
  ( GHasClient mkExp m a api
  , RunClient m
  ) =>
  Proxy mkExp ->
  Proxy m ->
  Proxy api ->
  Request ->
  a () ->
  Client m api
genericClientWithRoute = gClientWithRoute

genericHoistClientMonad ::
  forall mkExp m a api mon mon'.
  ( GHasClient mkExp m a api
  , RunClient m
  ) =>
  RunClient m =>
  Proxy mkExp ->
  Proxy m ->
  Proxy api ->
  (forall x. mon x -> mon' x) ->
  (a () -> Client mon api) ->
  (a () -> Client mon' api)
genericHoistClientMonad = gHoistClientMonad

instance
  ( RunClient m
  , GHasClient mkExp m a api
  ) =>
  HasClient m (GParam mkExp (a ()) :> api)
  where
  type Client m (GParam mkExp (a ()) :> api) = a () -> Client m api
  clientWithRoute pm _ = gClientWithRoute (Proxy :: Proxy mkExp) pm (Proxy :: Proxy api)
  {-# INLINE clientWithRoute #-}
  hoistClientMonad pm _ = gHoistClientMonad (Proxy :: Proxy mkExp) pm (Proxy :: Proxy api)
  {-# INLINE hoistClientMonad #-}

instance
  GHasClient mkExp m c api =>
  GHasClient mkExp m (D1 m3 c) api
  where
  gClientWithRoute _ pm _ req (M1 x) =
    gClientWithRoute (Proxy :: Proxy mkExp) pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm Proxy f cl x =
    gHoistClientMonad (Proxy :: Proxy mkExp) pm (Proxy :: Proxy api) f (cl . M1) (unM1 x)
  {-# INLINE gHoistClientMonad #-}

instance
  GHasClient mkExp m a (GParam mkExp (b ()) :> api) =>
  GHasClient mkExp m (a :*: b) api
  where
  gClientWithRoute _ pm _ req (x :*: y) =
    gClientWithRoute (Proxy :: Proxy mkExp) pm (Proxy :: Proxy (GParam mkExp (b ()) :> api)) req x y
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm Proxy f cl (x :*: y) =
    gHoistClientMonad
      (Proxy :: Proxy mkExp)
      pm
      (Proxy :: Proxy (GParam mkExp (b ()) :> api))
      f
      (\x' y' -> cl (x' :*: y'))
      x
      y
  {-# INLINE gHoistClientMonad #-}

instance GHasClient mkExp m a api => GHasClient mkExp m (C1 mon a) api where
  gClientWithRoute _ pm _ req (M1 x) =
    gClientWithRoute (Proxy :: Proxy mkExp) pm (Proxy :: Proxy api) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 x) =
    gHoistClientMonad (Proxy :: Proxy mkExp) pm (Proxy :: Proxy api) f (cl . M1) x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  ) =>
  GHasClient mkExp m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryFlag (Modify (mkExp sym)) :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryFlag (Modify (mkExp sym)) :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  ) =>
  GHasClient mkExp m (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute pm (Proxy :: Proxy (QueryParams (Modify (mkExp sym)) a :> api)) req x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParams (Modify (mkExp sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPING #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  ) =>
  GHasClient
    mkExp
    m
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)))
    api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute
      pm
      (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Modify (mkExp sym)) a :> api))
      req
      x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParam' '[Optional, Strict] (Modify (mkExp sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}

instance
  {-# OVERLAPPABLE #-}
  ( ToHttpApiData a
  , HasClient m api
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  ) =>
  GHasClient
    mkExp
    m
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a))
    api
  where
  gClientWithRoute _ pm _ req (M1 (K1 x)) =
    clientWithRoute
      pm
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Modify (mkExp sym)) a :> api))
      req
      x
  {-# INLINE gClientWithRoute #-}
  gHoistClientMonad _ pm _ f cl (M1 (K1 x)) =
    hoistClientMonad
      pm
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Modify (mkExp sym)) a :> api))
      f
      (cl . M1 . K1)
      x
  {-# INLINE gHoistClientMonad #-}
