{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Union where

import Data.Proxy
import Servant.Client
import Servant.Docs
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
        Just resp -> respond $ Just resp

instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b
  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy a) req :<|>
    clientWithRoute (Proxy :: Proxy b) req

instance (HasDocs layout1, HasDocs layout2)
      => HasDocs (layout1 :<|> layout2) where

  docsFor Proxy (ep, action) = docsFor p1 (ep, action) <> docsFor p2 (ep, action)

    where p1 :: Proxy layout1
          p1 = Proxy

          p2 :: Proxy layout2
          p2 = Proxy
