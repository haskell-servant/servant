{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- fixme: document RouteResult better
-- fixme: document phases
-- fixme: document that the req body can only be consumed once
-- fixme: document dependency problem

-- | This module provides convenience functions that make it easy to write
-- 'HasServer' instances for your own custom servant combinators.
--
-- It is also intended to be a more stable interface for writing
-- combinators than 'Servant.Server.Internal' and its submodules.
--
-- For examples on how to write combinators see 'makeCaptureCombinator' and friends.

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

-- | 'ServerCombinator' is a type to encapsulate the implementations
-- of the 'route' method of the 'HasServer' class of your custom combinators.
-- You can create a 'ServerCombinator' using one of the 'make...' functions below.
--
-- Type parameters:
--
-- - @combinator@ -- Your custom combinator type, usually an uninhabited dummy type.
-- - @context@ -- The context your combinator (and all other combinators) have access to.
--   In most cases this can be ignored. For further information, see
--   'Servant.Server.Internal.Context'.
-- - @api@ -- The subapi to be used in @serverType@.
-- - @serverType@ -- The type of the server that implements an api containing your combinator.
--   This should contain a call to 'ServerT' applied to @api@ -- the other type parameter -- and
--   'Handler'. If your combinator for example supplies an 'Int' to endpoint handlers,
--   @serverType@ would be @'Int' -> 'ServerT' api 'Handler'@.
data ServerCombinator combinator api context serverType where
  CI :: (forall env .
    Proxy (combinator :> api)
    -> Context context
    -> Delayed env serverType
    -> Router' env RoutingApplication)
    -> ServerCombinator combinator api context serverType

-- | 'runServerCombinator' is used to actually implement the method 'route' from the type class
-- 'HasServer'. You can ignore most of the type of this function. All you need to do is to supply
-- a 'ServerCombinator'.
runServerCombinator :: ServerCombinator combinator api context serverType
  -> Proxy (combinator :> api)
  -> Context context
  -> Delayed env serverType
  -> Router' env RoutingApplication
runServerCombinator (CI i) = i

-- | 'makeCaptureCombinator' allows you to write a combinator that inspects a path snippet
-- and provides an additional argument to endpoint handlers. You can choose the type of
-- that argument.
--
-- Here's an example of a combinator 'MyCaptureCombinator' that tries to parse a path snippet as
-- an 'Int' and provides that 'Int' as an argument to the endpoint handler. Note that in case the
-- path snippet cannot be parsed as an 'Int' the combinator errors out (using 'Fail'), which means
-- the endpoint handler will not be called.
--
-- >>> :set -XTypeFamilies
-- >>> :set -XTypeOperators
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -Wno-missing-methods
-- >>> import Text.Read
-- >>> import Data.String.Conversions
-- >>> :{
--   data MyCaptureCombinator
--   instance HasServer api context => HasServer (MyCaptureCombinator :> api) context where
--     type ServerT (MyCaptureCombinator :> api) m = Int -> ServerT api m
--     route = runServerCombinator $ makeCaptureCombinator getCaptureString
--   getCaptureString :: Context context -> Text -> IO (RouteResult Int)
--   getCaptureString _context pathSnippet = return $ case readMaybe (cs pathSnippet) of
--     Just n -> Route n
--     Nothing -> Fail err404
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

-- | 'makeRequestCheckCombinator' allows you to a combinator that checks a property of the
-- 'Request', while not providing any additional argument to your endpoint handlers.
--
-- Combinators created with 'makeRequestCheckCombinator' are *not* allowed to access the
-- request body (see 'makeCombinator').
--
-- This example shows a combinator 'BlockNonSSL' that disallows requests through @http@ and
-- only allows @https@. Note that -- in case of @http@ -- it uses 'FailFatal' to prevent
-- servant from trying out any remaining endpoints.
--
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

-- | 'makeAuthCombinator' allows you to write combinators for authorization.
--
-- Combinators created with this function are *not* allowed to access the request body
-- (see 'makeCombinator').
makeAuthCombinator ::
  (HasServer api context) =>
  (Context context -> Request -> IO (RouteResult authInformation))
  -> ServerCombinator combinator api context (authInformation -> ServerT api Handler)
makeAuthCombinator = inner
  where
    inner ::
      forall api combinator authInformation context .
      (HasServer api context) =>
      (Context context -> Request -> IO (RouteResult authInformation))
      -> ServerCombinator combinator api context (authInformation -> ServerT api Handler)
    inner authCheck = CI $ \ Proxy context delayed ->
      route (Proxy :: Proxy api) context $ addAuthCheck delayed $
      withRequest $ \ request ->
        liftRouteResult =<< liftIO (authCheck context (protectBody "makeAuthCombinator" request))

-- | 'makeCombinator' allows you to write combinators that have access to the whole request
-- (including the request body) while providing an additional argument to the endpoint handler.
-- This includes writing combinators that allow you to stream the request body. Here's a simple
-- example for that using a very simple stream implementation @Source@:
--
-- >>> import Data.ByteString
-- >>> :{
--   data Source = Source (IO ByteString)
--   data Stream
--   instance HasServer api context => HasServer (Stream :> api) context where
--     type ServerT (Stream :> api) m = Source -> ServerT api m
--     route = runServerCombinator $ makeCombinator requestToSource
--   requestToSource :: Context context -> Request -> IO (RouteResult Source)
--   requestToSource _context request =
--     return $ Route $ Source $ requestBody request
-- :}
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
