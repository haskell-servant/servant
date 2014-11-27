{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.QueryParam where

import Data.Maybe
import Data.Proxy
import Data.String.Conversions
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Servant.API.Sub
import Servant.Common.Text
import Servant.Server

-- | Lookup the value associated to the @sym@ query string parameter
-- and try to extract it as a value of type @a@.
--
-- Example:
--
-- > -- /books?author=<author name>
-- > type MyApi = "books" :> QueryParam "author" Text :> Get [Book]
data QueryParam sym a

-- | If you use @'QueryParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> EitherT (Int, String) IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
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

-- | Lookup the values associated to the @sym@ query string parameter
-- and try to extract it as a value of type @[a]@. This is typically
-- meant to support query string parameters of the form
-- @param[]=val1&param[]=val2@ and so on. Note that servant doesn't actually
-- require the @[]@s and will fetch the values just fine with
-- @param=val1&param=val2@, too.
--
-- Example:
--
-- > -- /books?authors[]=<author1>&authors[]=<author2>&...
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get [Book]
data QueryParams sym a

-- | If you use @'QueryParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT (Int, String) IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
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

-- | Lookup a potentially value-less query string parameter
-- with boolean semantics. If the param @sym@ is there without any value,
-- or if it's there with value "true" or "1", it's interpreted as 'True'.
-- Otherwise, it's interpreted as 'False'.
--
-- Example:
--
-- > -- /books?published
-- > type MyApi = "books" :> QueryFlag "published" :> Get [Book]
data QueryFlag sym

-- | If you use @'QueryFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> EitherT (Int, String) IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
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
