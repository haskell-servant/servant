{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module lets you implement 'Server's for defined APIs.  You will
-- probably need 'serve' (and look at the 'HasServer' type family), but
-- 'toApplication' and 'route' are rather internals.

module Servant.Server where

import Data.Proxy
import Data.Text
import Network.HTTP.Types
import Network.Wai

-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
serve :: HasServer layout => Proxy layout -> Server layout -> Application
serve p server = toApplication (route p server)

toApplication :: RoutingApplication -> Application
toApplication ra request respond = do
  ra (pathInfo request) request routingRespond
 where
  routingRespond :: Maybe Response -> IO ResponseReceived
  routingRespond Nothing =
    respond $ responseLBS notFound404 [] "not found"
  routingRespond (Just response) =
    respond response

type RoutingApplication =
     [Text] -- ^ the unmodified 'pathInfo'
  -> Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (Maybe Response -> IO ResponseReceived) -> IO ResponseReceived

class HasServer layout where
  type Server layout :: *
  route :: Proxy layout -> Server layout -> RoutingApplication
