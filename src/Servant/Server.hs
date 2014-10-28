{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module lets you implement 'Server's for defined APIs.  You will
-- probably need 'serve' (and look at the 'HasServer' type family), but
-- 'toApplication' and 'route' are rather internals.

module Servant.Server where

import Data.Monoid
import Data.Proxy
import Network.HTTP.Types
import Network.Wai

-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
serve :: HasServer layout => Proxy layout -> Server layout -> Application
serve p server = toApplication (route p server)

toApplication :: RoutingApplication -> Application
toApplication ra request respond = do
  ra request (routingRespond . routeResult)
 where
  routingRespond :: Either RouteMismatch Response -> IO ResponseReceived
  routingRespond (Left NotFound) =
    respond $ responseLBS notFound404 [] "not found"
  routingRespond (Left WrongMethod) =
    respond $ responseLBS methodNotAllowed405 [] "method not allowed"
  routingRespond (Right response) =
    respond response

-- * Route mismatch
data RouteMismatch =
    NotFound    -- ^ the usual "not found" error
  | WrongMethod -- ^ a more informative "you just got the HTTP method wrong" error
  deriving (Eq, Show)

-- | 
-- @
-- 'NotFound'    <> x = x
-- 'WrongMethod' <> _ = 'WrongMethod'
-- @
instance Monoid RouteMismatch where
  mempty = NotFound

  NotFound `mappend` x = x
  WrongMethod `mappend` _ = WrongMethod

-- | A wrapper around @'Either' 'RouteMismatch' a@.
newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Eq, Show)

failWith :: RouteMismatch -> RouteResult a
failWith = RR . Left

succeedWith :: a -> RouteResult a
succeedWith = RR . Right

isMismatch :: RouteResult a -> Bool
isMismatch (RR (Left _)) = True
isMismatch _             = False

-- | If we get a `Right`, it has precedence over everything else.
--
-- This in particular means that if we could get several 'Right's,
-- only the first we encounter would be taken into account.
instance Monoid (RouteResult a) where
  mempty = RR $ Left mempty

  RR (Left x)  `mappend` RR (Left y)  = RR $ Left (x <> y)
  RR (Left _)  `mappend` RR (Right y) = RR $ Right y
  r            `mappend` _            = r

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

class HasServer layout where
  type Server layout :: *
  route :: Proxy layout -> Server layout -> RoutingApplication
