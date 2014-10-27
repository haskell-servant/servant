{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Union where

import Data.Proxy
import Servant.Client
import Servant.Server

-- | Union of two APIs, first takes precedence in case of overlap.
data a :<|> b = a :<|> b
infixr 8 :<|>

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (a :<|> b) globalPathInfo request respond = do
    route (Proxy :: Proxy a) a globalPathInfo request $ \ mResponse ->
      case mResponse of
        Nothing -> route (Proxy :: Proxy b) b globalPathInfo request respond
        Just response -> respond $ Just response

instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b
  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy a) req :<|>
    clientWithRoute (Proxy :: Proxy b) req
