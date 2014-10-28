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
import Servant.Docs
import Servant.Server

-- * Request Body support
data RQBody a

instance (FromJSON a, HasServer sublayout)
      => HasServer (RQBody a :> sublayout) where

  type Server (RQBody a :> sublayout) =
    a -> Server sublayout

  route Proxy subserver request respond = do
    mrqbody <- decode' <$> lazyRequestBody request
    case mrqbody of
      Nothing -> respond Nothing
      Just v  -> route (Proxy :: Proxy sublayout) (subserver v) request respond

instance (ToJSON a, HasClient sublayout)
      => HasClient (RQBody a :> sublayout) where

  type Client (RQBody a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req body =
    clientWithRoute (Proxy :: Proxy sublayout) $
      setRQBody (encode body) req

instance (ToSample a, HasDocs sublayout)
      => HasDocs (RQBody a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout

          action' = action & rqbody .~ toSample p
          p = Proxy :: Proxy a
