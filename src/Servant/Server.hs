{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Server where

import Data.Proxy
import Network.HTTP.Types
import Network.Wai

-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
serve :: HasServer layout => Proxy layout -> Server layout -> Application
serve p server = toApplication (route p server)

toApplication :: RoutingApplication -> Application
toApplication ra = \ request respond -> do
  m <- ra request
  case m of
    Nothing -> respond $ responseLBS notFound404 [] "not found"
    Just response -> respond response

type RoutingApplication =
  Request -> IO (Maybe Response)

class HasServer layout where
  type Server layout :: *
  route :: Proxy layout -> Server layout -> RoutingApplication
