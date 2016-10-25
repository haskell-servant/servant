{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- fixme: document phases
-- fixme: document that the req body can only be consumed once
-- fixme: document that you can write request body streaming combinators
-- fixme: document dependency problem

module Servant.Server.Utils.CustomCombinators (

  -- * ServerCombinator

  ServerCombinator,
  runServerCombinator,

  -- * Constructing ServerCombinators

  makeCaptureCombinator,
  makeRequestCheckCombinator,
  makeAuthCombinator,
  makeCombinator,

  -- * Re-exports

  RouteResult(..),
) where

import           Control.Monad.IO.Class
import           Control.Exception (throwIO, ErrorCall(..))
import           Data.Proxy
import           Data.Text
import           Network.Wai

import           Servant.API
import           Servant.Server
import           Servant.Server.Internal

data ServerCombinator combinator serverType api context where
data ServerCombinator combinator api context serverType where
  CI :: (forall env .
    Proxy (combinator :> api)
    -> Context context
    -> Delayed env serverType
    -> Router' env RoutingApplication)
    -> ServerCombinator combinator api context serverType

runServerCombinator :: ServerCombinator combinator api context serverType
  -> Proxy (combinator :> api)
  -> Context context
  -> Delayed env serverType
  -> Router' env RoutingApplication
runServerCombinator (CI i) = i

-- |
-- >>> :set -XTypeFamilies
-- >>> :{
--   import Text.Read
--   data MyCaptureCombinator
--   instance HasServer api context => HasServer (MyCaptureCombinator :> api) context where
--     type ServerT (MyCaptureCombinator :> api) m = Int -> ServerT api m
--     route = runServerCombinator $ makeCaptureCombinator getCaptureString
--   getCaptureString :: Context context -> Text -> IO (RouteResult Int)
--   getCaptureString _context pathSnippet = return $ case readMaybe (cs pathSnippet) of
--     Just n -> Route n
--     Nothing -> FailFatal err400
-- :}
makeCaptureCombinator ::
  (HasServer api context) =>
  (Context context -> Text -> IO (RouteResult arg))
  -> ServerCombinator combinator api context (arg -> ServerT api Handler)
makeCaptureCombinator = inner -- we use 'inner' to avoid having 'forall' show up in haddock docs
  where
    inner ::
      forall api combinator arg context .
      (HasServer api context) =>
      (Context context -> Text -> IO (RouteResult arg))
      -> ServerCombinator combinator api context (arg -> ServerT api Handler)
    inner getArg = CI $ \ Proxy context delayed ->
      CaptureRouter $
      route (Proxy :: Proxy api) context $ addCapture delayed $ \ captured ->
      (liftRouteResult =<< liftIO (getArg context captured))

-- |
-- >>> :{
--   data BlockNonSSL
--   instance HasServer api context => HasServer (BlockNonSSL :> api) context where
--     type ServerT (BlockNonSSL :> api) m = ServerT api m
--     route = runServerCombinator $ makeRequestCheckCombinator checkRequest
--   checkRequest :: Context context -> Request -> IO (RouteResult ())
--   checkRequest _context request = return $ if isSecure request
--     then Route ()
--     else FailFatal err400
-- :}
makeRequestCheckCombinator ::
  (HasServer api context) =>
  (Context context -> Request -> IO (RouteResult ()))
  -> ServerCombinator combinator api context (ServerT api Handler)
makeRequestCheckCombinator = inner
  where
    inner ::
      forall api combinator context .
      (HasServer api context) =>
      (Context context -> Request -> IO (RouteResult ()))
      -> ServerCombinator combinator api context (ServerT api Handler)
    inner check = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addMethodCheck delayed $
      withRequest $ \ request ->
        liftRouteResult =<< liftIO (check context (protectBody "makeRequestCheckCombinator" request))

makeAuthCombinator ::
  (HasServer api context) =>
  (Context context -> Request -> IO (RouteResult arg))
  -> ServerCombinator combinator api context (arg -> ServerT api Handler)
makeAuthCombinator = inner
  where
    inner ::
      forall api combinator arg context .
      (HasServer api context) =>
      (Context context -> Request -> IO (RouteResult arg))
      -> ServerCombinator combinator api context (arg -> ServerT api Handler)
    inner authCheck = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addAuthCheck delayed $
      withRequest $ \ request ->
        liftRouteResult =<< liftIO (authCheck context (protectBody "makeAuthCombinator" request))

makeCombinator ::
  (HasServer api context) =>
  (Context context -> Request -> IO (RouteResult arg))
  -> ServerCombinator combinator api context (arg -> ServerT api Handler)
makeCombinator = inner
  where
    inner ::
      forall api combinator arg context .
      (HasServer api context) =>
      (Context context -> Request -> IO (RouteResult arg))
      -> ServerCombinator combinator api context (arg -> ServerT api Handler)
    inner getArg = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addBodyCheck delayed
      (return ())
      (\ () -> withRequest $ \ request ->
        liftRouteResult =<< liftIO (getArg context request))

protectBody :: String -> Request -> Request
protectBody name request = request{
  requestBody = throwIO $ ErrorCall $
    "ERROR: " ++ name ++ ": combinator must not access the request body"
}
