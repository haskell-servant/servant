{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.CombinatorUtils (
  RouteResult(..),
  argumentCombinator,
) where

import           Data.Proxy
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
  route (Proxy :: Proxy api) context $ addBodyCheck delayed $
  DelayedIO $ \ request -> return $ getArg request
