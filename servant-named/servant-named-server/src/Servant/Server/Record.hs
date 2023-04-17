{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module just exports orphan instances to make @servant-named@ work with servers
module Servant.Server.Record () where

import Data.Kind (Type)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.Record (RecordParam)
import Servant.Server
import Servant.Server.Internal
import Servant.Symbols (Exp, Modify)

instance (Generic a, GHasServer mkExp (Rep a) context api) => HasServer (RecordParam mkExp a :> api) context where
  type ServerT (RecordParam mkExp a :> api) m = a -> ServerT api m
  route _ context env =
    gRoute (Proxy :: Proxy mkExp) (Proxy :: Proxy api) context $
      (\f (x :: Rep a ()) -> f (to x)) <$> env
  {-# INLINE route #-}
  hoistServerWithContext _ pc nt s x =
    gHoistServerWithContext (Proxy :: Proxy mkExp) (Proxy :: Proxy api) pc nt (s . to) (from x :: Rep a ())
  {-# INLINE hoistServerWithContext #-}

class GHasServer (mkExp :: Symbol -> Exp Symbol) (a :: Type -> Type) context api where
  gRoute ::
    Proxy mkExp ->
    Proxy api ->
    Context context ->
    Delayed env (a () -> Server api) ->
    Router env
  gHoistServerWithContext ::
    Proxy mkExp ->
    Proxy api ->
    Proxy context ->
    (forall x. m x -> n x) ->
    (a () -> ServerT api m) ->
    (a () -> ServerT api n)

data GParam (mkExp :: Symbol -> Exp Symbol) a

instance
  GHasServer mkExp a context api =>
  HasServer (GParam mkExp (a ()) :> api) context
  where
  type ServerT (GParam mkExp (a ()) :> api) m = a () -> ServerT api m
  route _ = gRoute (Proxy :: Proxy mkExp) (Proxy :: Proxy api)
  {-# INLINE route #-}
  hoistServerWithContext _ = gHoistServerWithContext (Proxy :: Proxy mkExp) (Proxy :: Proxy api)
  {-# INLINE hoistServerWithContext #-}

instance
  GHasServer mkExp c context api =>
  GHasServer mkExp (D1 m3 c) context api
  where
  gRoute _ _ context env =
    gRoute (Proxy :: Proxy mkExp) (Proxy :: Proxy api) context $
      (\f x -> f (M1 x)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 x) =
    gHoistServerWithContext (Proxy :: Proxy mkExp) (Proxy :: Proxy api) pc nt (s . M1) x
  {-# INLINE gHoistServerWithContext #-}

instance
  GHasServer mkExp a context (GParam mkExp (b ()) :> api) =>
  GHasServer mkExp (a :*: b) context api
  where
  gRoute _ _ context env =
    gRoute
      (Proxy :: Proxy mkExp)
      (Proxy :: Proxy (GParam mkExp (b ()) :> api))
      context
      $ (\f x y -> f (x :*: y)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (x :*: y) =
    gHoistServerWithContext
      (Proxy :: Proxy mkExp)
      (Proxy :: Proxy (GParam mkExp (b ()) :> api))
      pc
      nt
      (\x' y' -> s (x' :*: y'))
      x
      y
  {-# INLINE gHoistServerWithContext #-}

instance
  GHasServer mkExp a context api =>
  GHasServer mkExp (C1 n a) context api
  where
  gRoute _ _ context env =
    gRoute (Proxy :: Proxy mkExp) (Proxy :: Proxy api) context $
      (\f x -> f (M1 x)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 x) =
    gHoistServerWithContext (Proxy :: Proxy mkExp) (Proxy :: Proxy api) pc nt (s . M1) x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPING #-}
  ( HasServer api context
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  ) =>
  GHasServer
    mkExp
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryFlag (Modify (mkExp sym)) :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryFlag (Modify (mkExp sym)) :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPING #-}
  ( HasServer api context
  , FromHttpApiData a
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    mkExp
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryParams (Modify (mkExp sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParams (Modify (mkExp sym)) a :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPING #-}
  ( HasServer api context
  , FromHttpApiData a
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    mkExp
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryParam (Modify (mkExp sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParam (Modify (mkExp sym)) a :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPABLE #-}
  ( HasServer api context
  , FromHttpApiData a
  , KnownSymbol sym
  , KnownSymbol (Modify (mkExp sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    mkExp
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Modify (mkExp sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Modify (mkExp sym)) a :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}
