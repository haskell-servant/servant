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

import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (fromMaybe)
import Data.SOP.BasicFunctors (K(K))
import Data.SOP.Constraint (All, And)
import Data.SOP.NS (cmap_NS, collapse_NS)
import Data.String.Conversions (LBS, cs)
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Types (Status, hAccept, hContentType)
import Network.Wai (requestHeaders, responseLBS)
import Servant.API (ReflectMethod, reflectMethod)
import Servant.API.ContentTypes (AcceptHeader(AcceptHeader), AllCTRender(handleAcceptH), AllMime)
import Servant.API.UVerb (HasStatus, IsMember, Statuses, Union, Unique, UVerb, inject, statusOf)
import Servant.Server.Internal (Context, Delayed, Handler, HasServer(..), RouteResult(FailFatal, Route), Router, Server, ServerT, acceptCheck, addAcceptCheck, addMethodCheck, allowedMethodHead, ct_wildcard, err406, leafRouter, methodCheck, runAction)


-- | 'return' for 'UVerb' handlers.  Takes a value of any of the members of the open union,
-- and will construct a union value in an 'Applicative' (eg. 'Server').
respond ::
  forall (x :: *) (xs :: [*]) (f :: * -> *).
  (Applicative f, HasStatus x, IsMember x xs) =>
  x ->
  f (Union xs)
respond = pure . inject . Identity

-- | Helper constraint used in @instance 'HasServer' 'UVerb'@.
type IsServerResource contentTypes = AllCTRender contentTypes `And` HasStatus

instance
  ( ReflectMethod method,
    AllMime contentTypes,
    All (IsServerResource contentTypes) as,
    Unique (Statuses as) -- for consistency with servant-swagger (server would work fine
        -- wihtout; client is a bit of a corner case, because it dispatches
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
        let accH :: AcceptHeader -- for picking the content type.
            accH = AcceptHeader $ fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
            action' :: Delayed env (Handler (Union as))
            action' =
              action
                `addMethodCheck` methodCheck method request
                `addAcceptCheck` acceptCheck (Proxy @contentTypes) accH
            mkProxy :: a -> Proxy a
            mkProxy _ = Proxy

        runAction action' env request cont $ \(output :: Union as) -> do
          let encodeResource :: (AllCTRender contentTypes a, HasStatus a) => Identity a -> K (Status, Maybe (LBS, LBS)) a
              encodeResource (Identity res) =
                K
                  ( statusOf $ mkProxy res,
                    handleAcceptH (Proxy @contentTypes) accH res
                  )
              pickResource :: Union as -> (Status, Maybe (LBS, LBS))
              pickResource = collapse_NS . cmap_NS (Proxy @(IsServerResource contentTypes)) encodeResource

          case pickResource output of
            (_, Nothing) -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            (status, Just (contentT, body)) ->
              let bdy = if allowedMethodHead method request then "" else body
               in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy
