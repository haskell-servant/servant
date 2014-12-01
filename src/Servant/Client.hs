{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client
  ( client
  , HasClient(..)
  , module Servant.Common.BaseUrl
  ) where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Proxy
import Data.String.Conversions
import Data.Text (unpack)
import GHC.TypeLits
import Network.HTTP.Types
import Servant.API
import Servant.Common.BaseUrl
import Servant.Common.Req
import Servant.Common.Text

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: BaseUrl -> EitherT String IO [Book]
-- > postNewBook :: Book -> BaseUrl -> EitherT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi
client :: HasClient layout => Proxy layout -> Client layout
client p = clientWithRoute p defReq

-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
class HasClient layout where
  type Client layout :: *
  clientWithRoute :: Proxy layout -> Req -> Client layout

-- | A client querying function for @a ':<|>' b@ will actually hand you
--   one function for querying @a@ and another one for querying @b@,
--   stitching them together with ':<|>', which really is just like a pair.
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: BaseUrl -> EitherT String IO [Book]
-- > postNewBook :: Book -> BaseUrl -> EitherT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi
instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b
  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy a) req :<|>
    clientWithRoute (Proxy :: Proxy b) req

-- | If you use a 'Capture' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'Capture'.
-- That function will take care of inserting a textual representation
-- of this value at the right place in the request path.
--
-- You can control how values for this type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBook :: Text -> BaseUrl -> EitherT String IO Book
-- > getBook = client myApi
-- > -- then you can just use "getBook" to query that endpoint
instance (KnownSymbol capture, ToText a, HasClient sublayout)
      => HasClient (Capture capture a :> sublayout) where

  type Client (Capture capture a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req val =
    clientWithRoute (Proxy :: Proxy sublayout) $
      appendToPath p req

    where p = unpack (toText val)

-- | If you have a 'Delete' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance HasClient Delete where
  type Client Delete = BaseUrl -> EitherT String IO ()

  clientWithRoute Proxy req host =
    performRequestJSON methodDelete req 204 host

-- | If you have a 'Get' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance FromJSON result => HasClient (Get result) where
  type Client (Get result) = BaseUrl -> EitherT String IO result
  clientWithRoute Proxy req host =
    performRequestJSON methodGet req 200 host

-- | If you have a 'Post' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance FromJSON a => HasClient (Post a) where
  type Client (Post a) = BaseUrl -> EitherT String IO a

  clientWithRoute Proxy req uri =
    performRequestJSON methodPost req 201 uri

-- | If you have a 'Put' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance FromJSON a => HasClient (Put a) where
  type Client (Put a) = BaseUrl -> EitherT String IO a

  clientWithRoute Proxy req host =
    performRequestJSON methodPut req 200 host

-- | If you use a 'QueryParam' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'QueryParam',
-- enclosed in Maybe.
--
-- If you give Nothing, nothing will be added to the query string.
--
-- If you give a non-'Nothing' value, this function will take care
-- of inserting a textual representation of this value in the query string.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: Maybe Text -> BaseUrl -> EitherT String IO [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
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

-- | If you use a 'QueryParams' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument, a list of values of the type specified 
-- by your 'QueryParams'.
--
-- If you give an empty list, nothing will be added to the query string.
--
-- Otherwise, this function will take care
-- of inserting a textual representation of your values in the query string,
-- under the same query string parameter name.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: [Text] -> BaseUrl -> EitherT String IO [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
instance (KnownSymbol sym, ToText a, HasClient sublayout)
      => HasClient (QueryParams sym a :> sublayout) where

  type Client (QueryParams sym a :> sublayout) =
    [a] -> Client sublayout

  clientWithRoute Proxy req paramlist =
    clientWithRoute (Proxy :: Proxy sublayout) $
      foldl' (\ value req' -> appendToQueryString pname req' value) req paramlist'

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          paramlist' = map (Just . toText) paramlist

-- | If you use a 'QueryFlag' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional 'Bool' argument.
--
-- If you give 'False', nothing will be added to the query string.
--
-- Otherwise, this function will insert a value-less query string
-- parameter under the name associated to your 'QueryFlag'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooks :: Bool -> BaseUrl -> EitherT String IO [Book]
-- > getBooks = client myApi
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books
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

-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the status code and the response body as a 'ByteString'.
instance HasClient Raw where
  type Client Raw = Method -> BaseUrl -> EitherT String IO (Int, ByteString)

  clientWithRoute :: Proxy Raw -> Req -> Client Raw
  clientWithRoute Proxy req httpMethod host =
    performRequest httpMethod req (const True) host

-- | If you use a 'ReqBody' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'ReqBody'.
-- That function will take care of encoding this argument as JSON and
-- of using it as the request body.
--
-- All you need is for your type to have a 'ToJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody Book :> Post Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > addBook :: Book -> BaseUrl -> EitherT String IO Book
-- > addBook = client myApi
-- > -- then you can just use "addBook" to query that endpoint
instance (ToJSON a, HasClient sublayout)
      => HasClient (ReqBody a :> sublayout) where

  type Client (ReqBody a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req body =
    clientWithRoute (Proxy :: Proxy sublayout) $
      setRQBody (encode body) req

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient sublayout) => HasClient (path :> sublayout) where
  type Client (path :> sublayout) = Client sublayout

  clientWithRoute Proxy req =
     clientWithRoute (Proxy :: Proxy sublayout) $
       appendToPath p req

    where p = symbolVal (Proxy :: Proxy path)

