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

-- | This module exports orphan instances to make
-- [@servant-queryparam-core@](https://hackage.haskell.org/package/servant-queryparam-core) work with servers.
module Servant.Server.Record () where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.Record
import Servant.Server
import Servant.Server.Internal
import Servant.Symbols

instance (Generic a, GHasServer mod (Rep a) context api) => HasServer (RecordParam mod a :> api) context where
  type ServerT (RecordParam mod a :> api) m = a -> ServerT api m
  route _ context env =
    gRoute (Proxy :: Proxy mod) (Proxy :: Proxy api) context $
      (\f (x :: Rep a ()) -> f (to x)) <$> env
  {-# INLINE route #-}
  hoistServerWithContext _ pc nt s x =
    gHoistServerWithContext (Proxy :: Proxy mod) (Proxy :: Proxy api) pc nt (s . to) (from x :: Rep a ())
  {-# INLINE hoistServerWithContext #-}

class GHasServer (mod :: Symbol -> Exp Symbol) (a :: Type -> Type) context api where
  gRoute ::
    Proxy mod ->
    Proxy api ->
    Context context ->
    Delayed env (a () -> Server api) ->
    Router env
  gHoistServerWithContext ::
    Proxy mod ->
    Proxy api ->
    Proxy context ->
    (forall x. m x -> n x) ->
    (a () -> ServerT api m) ->
    (a () -> ServerT api n)

data GParam (mod :: Symbol -> Exp Symbol) a

instance
  GHasServer mod a context api =>
  HasServer (GParam mod (a ()) :> api) context
  where
  type ServerT (GParam mod (a ()) :> api) m = a () -> ServerT api m
  route _ = gRoute (Proxy :: Proxy mod) (Proxy :: Proxy api)
  {-# INLINE route #-}
  hoistServerWithContext _ = gHoistServerWithContext (Proxy :: Proxy mod) (Proxy :: Proxy api)
  {-# INLINE hoistServerWithContext #-}

instance
  GHasServer mod c context api =>
  GHasServer mod (D1 m3 c) context api
  where
  gRoute _ _ context env =
    gRoute (Proxy :: Proxy mod) (Proxy :: Proxy api) context $
      (\f x -> f (M1 x)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 x) =
    gHoistServerWithContext (Proxy :: Proxy mod) (Proxy :: Proxy api) pc nt (s . M1) x
  {-# INLINE gHoistServerWithContext #-}

instance
  GHasServer mod a context (GParam mod (b ()) :> api) =>
  GHasServer mod (a :*: b) context api
  where
  gRoute _ _ context env =
    gRoute
      (Proxy :: Proxy mod)
      (Proxy :: Proxy (GParam mod (b ()) :> api))
      context
      $ (\f x y -> f (x :*: y)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (x :*: y) =
    gHoistServerWithContext
      (Proxy :: Proxy mod)
      (Proxy :: Proxy (GParam mod (b ()) :> api))
      pc
      nt
      (\x' y' -> s (x' :*: y'))
      x
      y
  {-# INLINE gHoistServerWithContext #-}

instance
  GHasServer mod a context api =>
  GHasServer mod (C1 n a) context api
  where
  gRoute _ _ context env =
    gRoute (Proxy :: Proxy mod) (Proxy :: Proxy api) context $
      (\f x -> f (M1 x)) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 x) =
    gHoistServerWithContext (Proxy :: Proxy mod) (Proxy :: Proxy api) pc nt (s . M1) x
  {-# INLINE gHoistServerWithContext #-}

instance
  {-# OVERLAPPING #-}
  ( HasServer api context
  , KnownSymbol sym
  , KnownSymbol (Eval (mod sym))
  ) =>
  GHasServer
    mod
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryFlag (Eval (mod sym)) :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryFlag (Eval (mod sym)) :> api))
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
  , KnownSymbol (Eval (mod sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    mod
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryParams (Eval (mod sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParams (Eval (mod sym)) a :> api))
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
  , KnownSymbol (Eval (mod sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    mod
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryParam (Eval (mod sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParam (Eval (mod sym)) a :> api))
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
  , KnownSymbol (Eval (mod sym))
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  GHasServer
    mod
    (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a))
    context
    api
  where
  gRoute _ _ context env =
    route
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (mod sym)) a :> api))
      context
      $ (\f x -> f (M1 (K1 x))) <$> env
  {-# INLINE gRoute #-}
  gHoistServerWithContext _ _ pc nt s (M1 (K1 x)) =
    hoistServerWithContext
      (Proxy :: Proxy (QueryParam' '[Required, Strict] (Eval (mod sym)) a :> api))
      pc
      nt
      (s . M1 . K1)
      x
  {-# INLINE gHoistServerWithContext #-}
