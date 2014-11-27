{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Sub where

import Data.Proxy
import Data.String.Conversions
import GHC.TypeLits
import Network.Wai
import Servant.Server

-- | The contained API (second argument) can be found under @("/" ++ path)@
-- (path being the first argument).
--
-- Example:
--
-- > -- GET /hello/world
-- > -- returning a JSON encoded World value
-- > type MyApi = "hello" :> "world" :> Get World
data (path :: k) :> a = Proxy path :> a
infixr 9 :>

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where
  type Server (path :> sublayout) = Server sublayout
  route Proxy subserver request respond = case pathInfo request of
    (first : rest)
      | first == cs (symbolVal proxyPath)
      -> route (Proxy :: Proxy sublayout) subserver request{
           pathInfo = rest
         } respond
    _ -> respond $ failWith NotFound

    where proxyPath = Proxy :: Proxy path
