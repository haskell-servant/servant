{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.GetParam where

import Data.Proxy
import Data.String.Conversions
import Data.Text
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Server
import Servant.Text

-- * GET params support (i.e query string arguments)
data GetParam sym a

instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (GetParam sym a :> sublayout) where

  type Server (GetParam sym a :> sublayout) =
    Maybe a -> Server sublayout

  route Proxy subserver globalPathInfo request respond = do
    let querytext = parseQueryText $ rawQueryString request
        param =
          case lookup paramName querytext of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> fromText v -- if present, we try to convert to
                                        -- the right type

    route (Proxy :: Proxy sublayout) (subserver param) globalPathInfo request respond

    where paramName = cs $ symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToText a, HasClient sublayout)
      => HasClient (GetParam sym a :> sublayout) where

  type Client (GetParam sym a :> sublayout) =
    Maybe a -> Client sublayout

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy req mparam =
    clientWithRoute (Proxy :: Proxy sublayout) $
      appendToQueryString pname mparamText req

    where pname  = pack pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap toText mparam
