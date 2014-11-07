{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.QueryParam where

import Data.List
import Data.Maybe
import Data.Proxy
import Data.String.Conversions
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Common.Req
import Servant.Common.Text
import Servant.Docs
import Servant.Server

-- * Single query string parameter lookup

-- | You must implement:
--
-- - a @'FromText' a@ instance for serving
-- - a @'ToText' a@ instance for (client-side) querying
-- - a @'ToParam' ('QueryParam' sym a)@ instance for automatic documentation generation
data QueryParam sym a

instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (QueryParam sym a :> sublayout) where

  type Server (QueryParam sym a :> sublayout) =
    Maybe a -> Server sublayout

  route Proxy subserver request respond = do
    let querytext = parseQueryText $ rawQueryString request
        param =
          case lookup paramname querytext of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> fromText v -- if present, we try to convert to
                                        -- the right type

    route (Proxy :: Proxy sublayout) (subserver param) request respond

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToText a, HasClient sublayout)
      => HasClient (QueryParam sym a :> sublayout) where

  type Client (QueryParam sym a :> sublayout) =
    Maybe a -> Client sublayout

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy req mparam =
    clientWithRoute (Proxy :: Proxy sublayout) $
      appendToQueryString pname mparamText req

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap toText mparam

instance (KnownSymbol sym, ToParam (QueryParam sym a), HasDocs sublayout)
      => HasDocs (QueryParam sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          paramP = Proxy :: Proxy (QueryParam sym a)
          action' = over params (|> toParam paramP) action

-- | Retrieve a list from the query string.
data QueryParams sym a

instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (QueryParams sym a :> sublayout) where

  type Server (QueryParams sym a :> sublayout) =
    [a] -> Server sublayout

  route Proxy subserver request respond = do
    let querytext = parseQueryText $ rawQueryString request
        -- if sym is "foo", we look for query string parameters
        -- named "foo" or "foo[]" and call fromText on the
        -- corresponding values
        parameters = filter looksLikeParam querytext
        values = catMaybes $ map (convert . snd) parameters

    route (Proxy :: Proxy sublayout) (subserver values) request respond

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = fromText v

instance (KnownSymbol sym, ToText a, HasClient sublayout)
      => HasClient (QueryParams sym a :> sublayout) where

  type Client (QueryParams sym a :> sublayout) =
    [a] -> Client sublayout

  clientWithRoute Proxy req paramlist =
    clientWithRoute (Proxy :: Proxy sublayout) $
      foldl' (\ value req -> appendToQueryString pname req value) req paramlist'

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          paramlist' = map (Just . toText) paramlist

instance (KnownSymbol sym, ToParam (QueryParams sym a), HasDocs sublayout)
      => HasDocs (QueryParams sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          paramP = Proxy :: Proxy (QueryParams sym a)
          action' = over params (|> toParam paramP) action

-- | Retrieve a value-less boolean from the query string.
data QueryFlag a

instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (QueryFlag sym :> sublayout) where

  type Server (QueryFlag sym :> sublayout) =
    Bool -> Server sublayout

  route Proxy subserver request respond = do
    let querytext = parseQueryText $ rawQueryString request
        param = case lookup paramname querytext of
          Just Nothing  -> True  -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing       -> False -- param not in the query string

    route (Proxy :: Proxy sublayout) (subserver param) request respond

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False

instance (KnownSymbol sym, HasClient sublayout)
      => HasClient (QueryFlag sym :> sublayout) where

  type Client (QueryFlag sym :> sublayout) =
    Bool -> Client sublayout

  clientWithRoute Proxy req flag =
    clientWithRoute (Proxy :: Proxy sublayout) $
      if flag
        then appendToQueryString paramname Nothing req
        else req

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToParam (QueryFlag sym), HasDocs sublayout)
      => HasDocs (QueryFlag sym :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          paramP = Proxy :: Proxy (QueryFlag sym)
          action' = over params (|> toParam paramP) action
