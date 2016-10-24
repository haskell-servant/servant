{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- fixme: document phases
-- fixme: add doctests
-- fixme: document that the req body can only be consumed once
-- fixme: document dependency problem

module Servant.Server.Utils.CustomCombinators (
  ServerCombinator,
  runServerCombinator,
  makeCaptureCombinator,
  makeRequestCheckCombinator,
  makeAuthCombinator,
  makeReqBodyCombinator,
  makeCombinator,

  -- * re-exports

  RouteResult(..),
) where

import           Control.Monad.IO.Class
import           Control.Exception (throwIO, ErrorCall(..))
import           Data.ByteString
import           Data.Proxy
import           Data.Text
import           Network.Wai

import           Servant.API
import           Servant.Server
import           Servant.Server.Internal

data ServerCombinator combinator arg api context where
  CI :: (forall env .
    Proxy (combinator :> api)
    -> Context context
    -> Delayed env (WithArg arg (Server api))
    -> Router' env RoutingApplication)
    -> ServerCombinator combinator arg api context

-- fixme: get rid of WithArg?
type family WithArg arg rest where
  WithArg () rest = rest
  WithArg arg rest = arg -> rest

runServerCombinator :: ServerCombinator combinator arg api context
  -> Proxy (combinator :> api)
  -> Context context
  -> Delayed env (WithArg arg (Server api))
  -> Router' env RoutingApplication
runServerCombinator (CI i) = i

makeCaptureCombinator ::
  (HasServer api context,
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler)) =>
  (Context context -> Text -> IO (RouteResult arg))
  -> ServerCombinator combinator arg api context
makeCaptureCombinator = inner -- we use 'inner' to avoid having 'forall' show up in haddock docs
  where
    inner ::
      forall api combinator arg context .
      (HasServer api context,
       WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler)) =>
      (Context context -> Text -> IO (RouteResult arg))
      -> ServerCombinator combinator arg api context
    inner getArg = CI $ \ Proxy context delayed ->
      CaptureRouter $
      route (Proxy :: Proxy api) context $ addCapture delayed $ \ captured ->
      (liftRouteResult =<< liftIO (getArg context captured))

makeRequestCheckCombinator ::
  (HasServer api context,
   WithArg () (ServerT api Handler) ~ ServerT api Handler) =>
  (Context context -> Request -> IO (RouteResult ()))
  -> ServerCombinator combinator () api context
makeRequestCheckCombinator = inner
  where
    inner ::
      forall api combinator context .
      (HasServer api context,
       WithArg () (ServerT api Handler) ~ ServerT api Handler) =>
      (Context context -> Request -> IO (RouteResult ()))
      -> ServerCombinator combinator () api context
    inner check = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addMethodCheck delayed $
      withRequest $ \ request ->
        liftRouteResult =<< liftIO (check context (protectBody "makeRequestCheckCombinator" request))

makeAuthCombinator ::
  (HasServer api context,
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler)) =>
  (Context context -> Request -> IO (RouteResult arg))
  -> ServerCombinator combinator arg api context
makeAuthCombinator = inner
  where
    inner ::
      forall api combinator arg context .
      (HasServer api context,
       WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler)) =>
      (Context context -> Request -> IO (RouteResult arg))
      -> ServerCombinator combinator arg api context
    inner authCheck = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addAuthCheck delayed $
      withRequest $ \ request ->
        liftRouteResult =<< liftIO (authCheck context (protectBody "makeAuthCombinator" request))

makeReqBodyCombinator ::
  (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler),
   HasServer api context) =>
  (Context context -> IO ByteString -> arg)
  -> ServerCombinator combinator arg api context
makeReqBodyCombinator = inner
  where
    inner ::
      forall api combinator arg context .
      (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
       WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler),
       HasServer api context) =>
      (Context context -> IO ByteString -> arg)
      -> ServerCombinator combinator arg api context
    inner getArg = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addBodyCheck delayed
      (return ())
      (\ () -> withRequest $ \ request ->
        liftRouteResult $ Route $ getArg context $ requestBody request)

makeCombinator ::
  (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
   WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler),
   HasServer api context) =>
  (Context context -> Request -> IO (RouteResult arg))
  -> ServerCombinator combinator arg api context
makeCombinator = inner
  where
    inner ::
      forall api combinator arg context .
      (ServerT (combinator :> api) Handler ~ (arg -> ServerT api Handler),
       WithArg arg (ServerT api Handler) ~ (arg -> ServerT api Handler),
       HasServer api context) =>
      (Context context -> Request -> IO (RouteResult arg))
      -> ServerCombinator combinator arg api context
    inner getArg = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addBodyCheck delayed
      (return ())
      (\ () -> withRequest $ \ request ->
        liftRouteResult =<< liftIO (getArg context (protectBody "makeCombinator" request)))

protectBody :: String -> Request -> Request
protectBody name request = request{
  requestBody = throwIO $ ErrorCall $
    "ERROR: " ++ name ++ ": combinator must not access the request body"
}
