{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

#include "overlapping-compat.h"
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client.Core
  ( AuthClientData
  , AuthenticateReq(..)
  , clientIn
  , HasClient(..)
  , mkAuthenticateReq
  , ServantError(..)
  , EmptyClient(..)
  , RunClient(..)
  , Request(..)
  , Response(..)
  , RequestBody(..)
  , module Servant.Client.Core.Internal.BaseUrl
  , ClientLike(..)
  , genericMkClientL
  , genericMkClientP
  -- * Writing instances
  , addHeader
  , appendToQueryString
  , appendToPath
  , setRequestBodyLBS
  , setRequestBody
  ) where

import           Control.Monad.Error.Class              (throwError)
import           Data.List                              (foldl')
import           Data.Proxy                             (Proxy (Proxy))
import           Data.String                            (fromString)
import           Data.Text                              (pack)
import           GHC.Exts                               (fromList, toList)
import           GHC.TypeLits                           (KnownSymbol, symbolVal)
import qualified Network.HTTP.Types                     as H
import           Prelude                                ()
import           Prelude.Compat
import           Servant.API                            ((:<|>) ((:<|>)), (:>),
                                                         AuthProtect, BasicAuth,
                                                         BasicAuthData,
                                                         BuildHeadersTo (..),
                                                         Capture, CaptureAll,
                                                         Description, EmptyAPI,
                                                         Header, Headers (..),
                                                         HttpVersion, IsSecure,
                                                         MimeRender (mimeRender),
                                                         MimeUnrender (mimeUnrender),
                                                         NoContent (NoContent),
                                                         QueryFlag, QueryParam,
                                                         QueryParams, Raw,
                                                         ReflectMethod (..),
                                                         RemoteHost, ReqBody,
                                                         Summary, ToHttpApiData,
                                                         Vault, Verb,
                                                         WithNamedContext,
                                                         contentType,
                                                         getHeadersHList,
                                                         getResponse,
                                                         toQueryParam,
                                                         toUrlPiece)
import           Servant.API.ContentTypes               (contentTypes)

import           Servant.Client.Core.Internal.Auth
import           Servant.Client.Core.Internal.BaseUrl   (BaseUrl (..),
                                                         InvalidBaseUrlException,
                                                         Scheme (..),
                                                         parseBaseUrl,
                                                         showBaseUrl)
import           Servant.Client.Core.Internal.BasicAuth
import           Servant.Client.Core.Internal.Class
import           Servant.Client.Core.Internal.Request
import           Servant.Client.Core.Internal.Generic

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client within
-- a given monadic context `m`
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > clientM :: Proxy ClientM
-- > clientM = Proxy
-- >
-- > getAllBooks :: ClientM [Book]
-- > postNewBook :: Book -> ClientM Book
-- > (getAllBooks :<|> postNewBook) = myApi `clientIn` clientM
clientIn :: HasClient m api => Proxy api -> Proxy m -> Client m api
clientIn p pm = clientWithRoute pm p defaultRequest


-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
class RunClient m => HasClient m api where
  type Client (m :: * -> *) (api :: *) :: *
  clientWithRoute :: Proxy m -> Proxy api -> Request -> Client m api


-- | A client querying function for @a ':<|>' b@ will actually hand you
--   one function for querying @a@ and another one for querying @b@,
--   stitching them together with ':<|>', which really is just like a pair.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: ClientM [Book]
-- > postNewBook :: Book -> ClientM Book
-- > (getAllBooks :<|> postNewBook) = client myApi
instance (HasClient m a, HasClient m b) => HasClient m (a :<|> b) where
  type Client m (a :<|> b) = Client m a :<|> Client m b
  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy a) req :<|>
    clientWithRoute pm (Proxy :: Proxy b) req

-- | Singleton type representing a client for an empty API.
data EmptyClient = EmptyClient deriving (Eq, Show, Bounded, Enum)

-- | The client for 'EmptyAPI' is simply 'EmptyClient'.
--
-- > type MyAPI = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "nothing" :> EmptyAPI
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: ClientM [Book]
-- > (getAllBooks :<|> EmptyClient) = client myApi
instance RunClient m => HasClient m EmptyAPI where
  type Client m EmptyAPI = EmptyClient
  clientWithRoute _pm Proxy _ = EmptyClient

-- | If you use a 'Capture' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'Capture'.
-- That function will take care of inserting a textual representation
-- of this value at the right place in the request path.
--
-- You can control how values for this type are turned into
-- text by specifying a 'ToHttpApiData' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBook :: Text -> ClientM Book
-- > getBook = client myApi
-- > -- then you can just use "getBook" to query that endpoint
instance (KnownSymbol capture, ToHttpApiData a, HasClient m api)
      => HasClient m (Capture capture a :> api) where

  type Client m (Capture capture a :> api) =
    a -> Client m api

  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api)
                    (appendToPath p req)

    where p = (toUrlPiece val)

-- | If you use a 'CaptureAll' in one of your endpoints in your API,
-- the corresponding querying function will automatically take an
-- additional argument of a list of the type specified by your
-- 'CaptureAll'. That function will take care of inserting a textual
-- representation of this value at the right place in the request
-- path.
--
-- You can control how these values are turned into text by specifying
-- a 'ToHttpApiData' instance of your type.
--
-- Example:
--
-- > type MyAPI = "src" :> CaptureAll Text -> Get '[JSON] SourceFile
-- >
-- > myApi :: Proxy
-- > myApi = Proxy
--
-- > getSourceFile :: [Text] -> ClientM SourceFile
-- > getSourceFile = client myApi
-- > -- then you can use "getSourceFile" to query that endpoint
instance (KnownSymbol capture, ToHttpApiData a, HasClient m sublayout)
      => HasClient m (CaptureAll capture a :> sublayout) where

  type Client m (CaptureAll capture a :> sublayout) =
    [a] -> Client m sublayout

  clientWithRoute pm Proxy req vals =
    clientWithRoute pm (Proxy :: Proxy sublayout)
                    (foldl' (flip appendToPath) req ps)

    where ps = map (toUrlPiece) vals

instance OVERLAPPABLE_
  -- Note [Non-Empty Content Types]
  ( RunClient m, MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts)
  ) => HasClient m (Verb method status cts' a) where
  type Client m (Verb method status cts' a) = m a
  clientWithRoute _pm Proxy req = do
    response <- runRequest req
      { requestAccept = fromList $ toList accept
      , requestMethod = method
      }
    case mimeUnrender (Proxy :: Proxy ct) $ responseBody response of
      Left err -> throwError $ DecodeFailure (pack err) response
      Right val -> return val
      where method = reflectMethod (Proxy :: Proxy method)
            accept = contentTypes (Proxy :: Proxy ct)

instance OVERLAPPING_
  ( RunClient m, ReflectMethod method
  ) => HasClient m (Verb method status cts NoContent) where
  type Client m (Verb method status cts NoContent)
    = m NoContent
  clientWithRoute _pm Proxy req = do
    _response <- runRequest req { requestMethod = method }
    return NoContent
      where method = reflectMethod (Proxy :: Proxy method)

instance OVERLAPPING_
  -- Note [Non-Empty Content Types]
  ( RunClient m, MimeUnrender ct a, BuildHeadersTo ls
  , ReflectMethod method, cts' ~ (ct ': cts)
  ) => HasClient m (Verb method status cts' (Headers ls a)) where
  type Client m (Verb method status cts' (Headers ls a))
    = m (Headers ls a)
  clientWithRoute _pm Proxy req = do
    response <- runRequest req
       { requestMethod = method
       , requestAccept = fromList $ toList accept
       }
    case mimeUnrender (Proxy :: Proxy ct) $ responseBody response of
      Left err -> throwError $ DecodeFailure (pack err) response
      Right val -> return $ Headers
        { getResponse = val
        , getHeadersHList = buildHeadersTo . toList $ responseHeaders response
        }
      where method = reflectMethod (Proxy :: Proxy method)
            accept = contentTypes (Proxy :: Proxy ct)

instance OVERLAPPING_
  ( RunClient m, BuildHeadersTo ls, ReflectMethod method
  ) => HasClient m (Verb method status cts (Headers ls NoContent)) where
  type Client m (Verb method status cts (Headers ls NoContent))
    = m (Headers ls NoContent)
  clientWithRoute _pm Proxy req = do
    let method = reflectMethod (Proxy :: Proxy method)
    response <- runRequest req { requestMethod = method }
    return $ Headers { getResponse = NoContent
                     , getHeadersHList = buildHeadersTo . toList $ responseHeaders response
                     }


-- | If you use a 'Header' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'Header',
-- wrapped in Maybe.
--
-- That function will take care of encoding this argument as Text
-- in the request headers.
--
-- All you need is for your type to have a 'ToHttpApiData' instance.
--
-- Example:
--
-- > newtype Referer = Referer { referrer :: Text }
-- >   deriving (Eq, Show, Generic, ToHttpApiData)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > viewReferer :: Maybe Referer -> ClientM Book
-- > viewReferer = client myApi
-- > -- then you can just use "viewRefer" to query that endpoint
-- > -- specifying Nothing or e.g Just "http://haskell.org/" as arguments
instance (KnownSymbol sym, ToHttpApiData a, HasClient m api)
      => HasClient m (Header sym a :> api) where

  type Client m (Header sym a :> api) =
    Maybe a -> Client m api

  clientWithRoute pm Proxy req mval =
    clientWithRoute pm (Proxy :: Proxy api)
                    (maybe req
                           (\value -> addHeader hname value req)
                           mval
                    )

    where hname = fromString $ symbolVal (Proxy :: Proxy sym)

-- | Using a 'HttpVersion' combinator in your API doesn't affect the client
-- functions.
instance HasClient m api
  => HasClient m (HttpVersion :> api) where

  type Client m (HttpVersion :> api) =
    Client m api

  clientWithRoute pm Proxy =
    clientWithRoute pm (Proxy :: Proxy api)

-- | Ignore @'Summary'@ in client functions.
instance HasClient m api => HasClient m (Summary desc :> api) where
  type Client m (Summary desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

-- | Ignore @'Description'@ in client functions.
instance HasClient m api => HasClient m (Description desc :> api) where
  type Client m (Description desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

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
-- text by specifying a 'ToHttpApiData' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: Maybe Text -> ClientM [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
instance (KnownSymbol sym, ToHttpApiData a, HasClient m api)
      => HasClient m (QueryParam sym a :> api) where

  type Client m (QueryParam sym a :> api) =
    Maybe a -> Client m api

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute pm Proxy req mparam =
    clientWithRoute pm (Proxy :: Proxy api)
                    (maybe req
                           (flip (appendToQueryString pname) req . Just)
                           mparamText
                    )

    where pname  = pack $ symbolVal (Proxy :: Proxy sym)
          mparamText = fmap toQueryParam mparam

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
-- text by specifying a 'ToHttpApiData' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: [Text] -> ClientM [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
instance (KnownSymbol sym, ToHttpApiData a, HasClient m api)
      => HasClient m (QueryParams sym a :> api) where

  type Client m (QueryParams sym a :> api) =
    [a] -> Client m api

  clientWithRoute pm Proxy req paramlist =
    clientWithRoute pm (Proxy :: Proxy api)
                    (foldl' (\ req' -> maybe req' (flip (appendToQueryString pname) req' . Just))
                            req
                            paramlist'
                    )

    where pname = pack $ symbolVal (Proxy :: Proxy sym)
          paramlist' = map (Just . toQueryParam) paramlist

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
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooks :: Bool -> ClientM [Book]
-- > getBooks = client myApi
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books
instance (KnownSymbol sym, HasClient m api)
      => HasClient m (QueryFlag sym :> api) where

  type Client m (QueryFlag sym :> api) =
    Bool -> Client m api

  clientWithRoute pm Proxy req flag =
    clientWithRoute pm (Proxy :: Proxy api)
                    (if flag
                       then appendToQueryString paramname Nothing req
                       else req
                    )

    where paramname = pack $ symbolVal (Proxy :: Proxy sym)


-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the full `Response`.
instance RunClient m => HasClient m Raw where
  type Client m Raw
    = H.Method ->  m Response

  clientWithRoute :: Proxy m -> Proxy Raw -> Request -> Client m Raw
  clientWithRoute _pm Proxy req httpMethod = do
    runRequest req { requestMethod = httpMethod }

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
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > addBook :: Book -> ClientM Book
-- > addBook = client myApi
-- > -- then you can just use "addBook" to query that endpoint
instance (MimeRender ct a, HasClient m api)
      => HasClient m (ReqBody (ct ': cts) a :> api) where

  type Client m (ReqBody (ct ': cts) a :> api) =
    a -> Client m api

  clientWithRoute pm Proxy req body =
    clientWithRoute pm (Proxy :: Proxy api)
                    (let ctProxy = Proxy :: Proxy ct
                     in setRequestBodyLBS (mimeRender ctProxy body)
                                          -- We use first contentType from the Accept list
                                          (contentType ctProxy)
                                          req
                    )

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient m api) => HasClient m (path :> api) where
  type Client m (path :> api) = Client m api

  clientWithRoute pm Proxy req =
     clientWithRoute pm (Proxy :: Proxy api)
                     (appendToPath p req)

    where p = pack $ symbolVal (Proxy :: Proxy path)

instance HasClient m api => HasClient m (Vault :> api) where
  type Client m (Vault :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) req

instance HasClient m api => HasClient m (RemoteHost :> api) where
  type Client m (RemoteHost :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) req

instance HasClient m api => HasClient m (IsSecure :> api) where
  type Client m (IsSecure :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) req

instance HasClient m subapi =>
  HasClient m (WithNamedContext name context subapi) where

  type Client m (WithNamedContext name context subapi) = Client m subapi
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy subapi)

instance ( HasClient m api
         ) => HasClient m (AuthProtect tag :> api) where
  type Client m (AuthProtect tag :> api)
    = AuthenticateReq (AuthProtect tag) -> Client m api

  clientWithRoute pm Proxy req (AuthenticateReq (val,func)) =
    clientWithRoute pm (Proxy :: Proxy api) (func val req)

-- * Basic Authentication

instance HasClient m api => HasClient m (BasicAuth realm usr :> api) where
  type Client m (BasicAuth realm usr :> api) = BasicAuthData -> Client m api

  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api) (basicAuthReq val req)


{- Note [Non-Empty Content Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Rather than have

   instance (..., cts' ~ (ct ': cts)) => ... cts' ...

It may seem to make more sense to have:

   instance (...) => ... (ct ': cts) ...

But this means that if another instance exists that does *not* require
non-empty lists, but is otherwise more specific, no instance will be overall
more specific. This in turn generally means adding yet another instance (one
for empty and one for non-empty lists).
-}
