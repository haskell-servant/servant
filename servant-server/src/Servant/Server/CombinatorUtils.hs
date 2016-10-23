{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.CombinatorUtils (
  RouteResult(..),
  argumentCombinator,
  captureCombinator,
) where

import           Data.Proxy
import           Data.Text
import           Network.Wai

import           Servant.API
import           Servant.Server
import           Servant.Server.Internal

argumentCombinator ::
  forall api combinator arg context env .
  (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
   HasServer api context) =>
  (Request -> RouteResult arg)
  -> Proxy (combinator :> api)
  -> Context context
  -> Delayed env (Server (combinator :> api))
  -> Router' env RoutingApplication
argumentCombinator getArg Proxy context delayed =
  route (Proxy :: Proxy api) context $ addBodyCheck delayed
    (DelayedIO (return ())) $ \ () ->
      withRequest $ \ request -> liftRouteResult (getArg request)

captureCombinator ::
  forall api combinator arg context env .
  (HasServer api context) =>
  (Text -> RouteResult arg)
  -> Proxy (combinator :> api)
  -> Context context
  -> Delayed env (arg -> Server api)
  -> Router' env RoutingApplication
captureCombinator getArg Proxy context delayed =
  CaptureRouter $
  route (Proxy :: Proxy api) context $ addCapture delayed $ \ captured ->
  (liftRouteResult (getArg captured))
