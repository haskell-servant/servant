{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.Utils.CustomCombinators (
  CombinatorImplementation,
  runCI,
  makeCaptureCombinator,
  makeRequestCheckCombinator,
  makeAuthCombinator,
  makeReqBodyCombinator,
  makeCombinator,

  -- * re-exports

  RouteResult(..),
) where

import           Data.ByteString
import           Data.Proxy
import           Data.Text
import           Network.Wai

import           Servant.API
import           Servant.Server
import           Servant.Server.Internal

data CombinatorImplementation combinator arg api context where
  CI :: (forall env .
    Proxy (combinator :> api)
    -> Context context
    -> Delayed env (WithArg arg (Server api))
    -> Router' env RoutingApplication)
    -> CombinatorImplementation combinator arg api context

type family WithArg arg rest where
  WithArg () rest = rest
  WithArg arg rest = arg -> rest

runCI :: CombinatorImplementation combinator arg api context
  -> Proxy (combinator :> api)
  -> Context context
  -> Delayed env (WithArg arg (Server api))
  -> Router' env RoutingApplication
runCI (CI i) = i

makeCaptureCombinator ::
  forall api combinator arg context .
  (HasServer api context,
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler)) =>
  (Text -> RouteResult arg)
  -> CombinatorImplementation combinator arg api context
makeCaptureCombinator getArg = CI $ \ Proxy context delayed ->
  CaptureRouter $
  route (Proxy :: Proxy api) context $ addCapture delayed $ \ captured ->
  (liftRouteResult (getArg captured))

makeRequestCheckCombinator ::
  forall api combinator context .
  (HasServer api context,
   WithArg () (ServerT api Handler) ~ ServerT api Handler) =>
  (Request -> RouteResult ())
  -> CombinatorImplementation combinator () api context
makeRequestCheckCombinator check = CI $ \ Proxy context delayed ->
  route (Proxy :: Proxy api) context $ addMethodCheck delayed $
  withRequest $ \ request -> liftRouteResult $ check request

makeAuthCombinator ::
  forall api combinator arg context .
  (HasServer api context,
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler)) =>
  (Request -> RouteResult arg)
  -> CombinatorImplementation combinator arg api context
makeAuthCombinator authCheck = CI $ \ Proxy context delayed ->
  route (Proxy :: Proxy api) context $ addAuthCheck delayed $
  withRequest $ \ request -> liftRouteResult $ authCheck request

makeReqBodyCombinator ::
  forall api combinator arg context .
  (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler),
   HasServer api context) =>
  (IO ByteString -> arg)
  -> CombinatorImplementation combinator arg api context
makeReqBodyCombinator getArg = CI $ \ Proxy context delayed ->
  route (Proxy :: Proxy api) context $ addBodyCheck delayed
  (return ())
  (\ () -> withRequest $ \ request -> liftRouteResult $ Route $ getArg $ requestBody request)

makeCombinator ::
  forall api combinator arg context .
  (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler),
   HasServer api context) =>
  (Request -> RouteResult arg)
  -> CombinatorImplementation combinator arg api context
makeCombinator getArg = CI $ \ Proxy context delayed ->
  route (Proxy :: Proxy api) context $ addBodyCheck delayed -- fixme: shouldn't be body
  (return ())
  (\ () -> withRequest $ \ request -> liftRouteResult $ getArg request)
