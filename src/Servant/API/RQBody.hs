{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.RQBody where

import Control.Applicative
import Data.Aeson
import Data.Proxy
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Server

-- * Request Body support
data RQBody a

instance (FromJSON a, HasServer sublayout)
      => HasServer (RQBody a :> sublayout) where

  type Server (RQBody a :> sublayout) =
    a -> Server sublayout

  route Proxy subserver request = do
    mrqbody <- decode' <$> lazyRequestBody request
    case mrqbody of
      Nothing -> return Nothing
      Just v  -> route (Proxy :: Proxy sublayout) (subserver v) request

instance (ToJSON a, HasClient sublayout)
      => HasClient (RQBody a :> sublayout) where

  type Client (RQBody a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req body =
    clientWithRoute (Proxy :: Proxy sublayout) $
      setRQBody (encode body) req
