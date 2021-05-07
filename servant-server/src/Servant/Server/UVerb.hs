{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Servant.Server.UVerb
  ( respond,
    IsServerResource,
  )
where

import qualified Data.ByteString as B
import Data.Proxy (Proxy (Proxy))
import Data.SOP (I (I))
import Data.SOP.Constraint (All, And)
import Data.String.Conversions (LBS, cs)
import Network.HTTP.Types (Status, HeaderName, hContentType)
import Network.Wai (responseLBS)
import Servant.API (ReflectMethod, reflectMethod)
import Servant.API.ContentTypes (AllCTRender (handleAcceptH), AllMime)
import Servant.API.ResponseHeaders (GetHeaders (..), Headers (..))
import Servant.API.UVerb (HasStatus, IsMember, Statuses, UVerb, Union, Unique, WithStatus (..), foldMapUnion, inject, statusOf)
import Servant.Server.Internal (Context, Delayed, Handler, HasServer (..), RouteResult (FailFatal, Route), Router, Server, ServerT, acceptCheck, addAcceptCheck, addMethodCheck, allowedMethodHead, err406, getAcceptHeader, leafRouter, methodCheck, runAction)


-- | 'return' for 'UVerb' handlers.  Takes a value of any of the members of the open union,
-- and will construct a union value in an 'Applicative' (eg. 'Server').
respond ::
  forall (x :: *) (xs :: [*]) (f :: * -> *).
  (Applicative f, HasStatus x, IsMember x xs) =>
  x ->
  f (Union xs)
respond = pure . inject . I

class HasResponseHeaders a where
  getResponseHeaders :: a -> [(HeaderName, B.ByteString)]

instance {-# OVERLAPPABLE #-} HasResponseHeaders a where
  getResponseHeaders _ = []

instance {-# OVERLAPPING #-} (HasResponseHeaders a, GetHeaders (Headers h a))
  => HasResponseHeaders (Headers h a) where
  getResponseHeaders x = getHeaders x <> getResponseHeaders (getResponse x)

instance {-# OVERLAPPING #-} HasResponseHeaders a
  => HasResponseHeaders (WithStatus n a) where
  getResponseHeaders (WithStatus x) = getResponseHeaders x

-- | Helper constraint used in @instance 'HasServer' 'UVerb'@.
type IsServerResource contentTypes =
  AllCTRender contentTypes `And` HasStatus `And` HasResponseHeaders

instance
  ( ReflectMethod method,
    AllMime contentTypes,
    All (IsServerResource contentTypes) as,
    Unique (Statuses as) -- for consistency with servant-swagger (server would work fine
        -- without; client is a bit of a corner case, because it dispatches
        -- the parser based on the status code.  with this uniqueness
        -- constraint it won't have to run more than one parser in weird
        -- corner cases.
  ) =>
  HasServer (UVerb method contentTypes as) context
  where
  type ServerT (UVerb method contentTypes as) m = m (Union as)

  hoistServerWithContext _ _ nt s = nt s

  route ::
    forall env.
    Proxy (UVerb method contentTypes as) ->
    Context context ->
    Delayed env (Server (UVerb method contentTypes as)) ->
    Router env
  route _proxy _ctx action = leafRouter route'
    where
      method = reflectMethod (Proxy @method)
      route' env request cont = do
        let action' :: Delayed env (Handler (Union as))
            action' =
              action
                `addMethodCheck` methodCheck method request
                `addAcceptCheck` acceptCheck (Proxy @contentTypes) (getAcceptHeader request)
            mkProxy :: a -> Proxy a
            mkProxy _ = Proxy

        runAction action' env request cont $ \(output :: Union as) -> do
          let encodeResource :: (AllCTRender contentTypes a, HasStatus a, HasResponseHeaders a)
                             => a -> (Status, Maybe (LBS, LBS), [(HeaderName, B.ByteString)])
              encodeResource res =
                ( statusOf $ mkProxy res,
                  handleAcceptH (Proxy @contentTypes) (getAcceptHeader request) res,
                  getResponseHeaders res
                )
              pickResource :: Union as -> (Status, Maybe (LBS, LBS), [(HeaderName, B.ByteString)])
              pickResource = foldMapUnion (Proxy @(IsServerResource contentTypes)) encodeResource

          case pickResource output of
            (_, Nothing, _) -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            (status, Just (contentT, body), headers) ->
              let bdy = if allowedMethodHead method request then "" else body
               in Route $ responseLBS status ((hContentType, cs contentT) : headers) bdy
