{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.CombinatorUtils (
  CombinatorImplementation,
  runCI,
  captureCombinator,
  argumentCombinator,
  -- * re-exports
  RouteResult(..),
) where

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
    -> Delayed env (arg -> Server api)
    -> Router' env RoutingApplication)
    -> CombinatorImplementation combinator arg api context

runCI :: CombinatorImplementation combinator arg api context
  -> Proxy (combinator :> api)
  -> Context context
  -> Delayed env (arg -> Server api)
  -> Router' env RoutingApplication
runCI (CI i) = i

captureCombinator ::
  forall api combinator arg context .
  (HasServer api context) =>
  (Text -> RouteResult arg)
  -> CombinatorImplementation combinator arg api context
captureCombinator getArg = CI $ \ Proxy context delayed ->
  CaptureRouter $
  route (Proxy :: Proxy api) context $ addCapture delayed $ \ captured ->
  (liftRouteResult (getArg captured))

argumentCombinator ::
  forall api combinator arg context .
  (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
   HasServer api context) =>
  (Request -> RouteResult arg)
  -> CombinatorImplementation combinator arg api context
argumentCombinator getArg = CI $ \ Proxy context delayed ->
  route (Proxy :: Proxy api) context $
    addBodyCheck delayed contentTypeCheck bodyCheck
  where
    contentTypeCheck = return ()
    bodyCheck () = withRequest $ \ request ->
      liftRouteResult (getArg request)
