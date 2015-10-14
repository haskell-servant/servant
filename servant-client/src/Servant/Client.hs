{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client
  ( client
  , HasClient(..)
  , ServantError(..)
  , module Servant.Common.BaseUrl
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
#endif
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.ByteString.Lazy       (ByteString)
import           Data.List
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text                  (unpack)
import           GHC.TypeLits
import           Network.HTTP.Client        (Response, Manager)
import           Network.HTTP.Media
import qualified Network.HTTP.Types         as H
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant.API
import           Servant.Common.BaseUrl
import           Servant.Common.Req

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: ExceptT String IO [Book]
-- > postNewBook :: Book -> ExceptT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi host manager
-- >   where host = BaseUrl Http "localhost" 8080
client :: HasClient layout => Proxy layout -> BaseUrl -> Manager -> Client layout
client p baseurl = clientWithRoute p defReq baseurl

-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
class HasClient layout where
  type Client layout :: *
  clientWithRoute :: Proxy layout -> Req -> BaseUrl -> Manager -> Client layout


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
-- > getAllBooks :: ExceptT String IO [Book]
-- > postNewBook :: Book -> ExceptT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi host manager
-- >   where host = BaseUrl Http "localhost" 8080
instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b
  clientWithRoute Proxy req baseurl manager =
    clientWithRoute (Proxy :: Proxy a) req baseurl manager :<|>
    clientWithRoute (Proxy :: Proxy b) req baseurl manager

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
-- > getBook :: Text -> ExceptT String IO Book
-- > getBook = client myApi host manager
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBook" to query that endpoint
instance (KnownSymbol capture, ToHttpApiData a, HasClient sublayout)
      => HasClient (Capture capture a :> sublayout) where

  type Client (Capture capture a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req baseurl manager val =
    clientWithRoute (Proxy :: Proxy sublayout)
                    (appendToPath p req)
                    baseurl
                    manager

    where p = unpack (toUrlPiece val)

-- | If you have a 'Delete' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
  (MimeUnrender ct a, cts' ~ (ct ': cts)) => HasClient (Delete cts' a) where
  type Client (Delete cts' a) = ExceptT ServantError IO a
  clientWithRoute Proxy req baseurl manager =
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodDelete req baseurl manager

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Delete cts ()) where
  type Client (Delete cts ()) = ExceptT ServantError IO ()
  clientWithRoute Proxy req baseurl manager =
    void $ performRequestNoBody H.methodDelete req baseurl manager

-- | If you have a 'Delete xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  ( MimeUnrender ct a, BuildHeadersTo ls, cts' ~ (ct ': cts)
  ) => HasClient (Delete cts' (Headers ls a)) where
  type Client (Delete cts' (Headers ls a)) = ExceptT ServantError IO (Headers ls a)
  clientWithRoute Proxy req baseurl manager = do
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodDelete req baseurl manager
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }

-- | If you have a 'Get' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
  (MimeUnrender ct result) => HasClient (Get (ct ': cts) result) where
  type Client (Get (ct ': cts) result) = ExceptT ServantError IO result
  clientWithRoute Proxy req baseurl manager =
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodGet req baseurl manager

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Get (ct ': cts) ()) where
  type Client (Get (ct ': cts) ()) = ExceptT ServantError IO ()
  clientWithRoute Proxy req baseurl manager =
    performRequestNoBody H.methodGet req baseurl manager

-- | If you have a 'Get xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  ( MimeUnrender ct a, BuildHeadersTo ls
  ) => HasClient (Get (ct ': cts) (Headers ls a)) where
  type Client (Get (ct ': cts) (Headers ls a)) = ExceptT ServantError IO (Headers ls a)
  clientWithRoute Proxy req baseurl manager = do
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodGet req baseurl manager
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
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
-- >   deriving (Eq, Show, Generic, FromText, ToHttpApiData)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > viewReferer :: Maybe Referer -> ExceptT String IO Book
-- > viewReferer = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "viewRefer" to query that endpoint
-- > -- specifying Nothing or e.g Just "http://haskell.org/" as arguments
instance (KnownSymbol sym, ToHttpApiData a, HasClient sublayout)
      => HasClient (Header sym a :> sublayout) where

  type Client (Header sym a :> sublayout) =
    Maybe a -> Client sublayout

  clientWithRoute Proxy req baseurl manager mval =
    clientWithRoute (Proxy :: Proxy sublayout)
                    (maybe req
                           (\value -> Servant.Common.Req.addHeader hname value req)
                           mval
                    )
                    baseurl
                    manager

    where hname = symbolVal (Proxy :: Proxy sym)

-- | If you have a 'Post' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
  (MimeUnrender ct a) => HasClient (Post (ct ': cts) a) where
  type Client (Post (ct ': cts) a) = ExceptT ServantError IO a
  clientWithRoute Proxy req baseurl manager =
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodPost req baseurl manager

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Post (ct ': cts) ()) where
  type Client (Post (ct ': cts) ()) = ExceptT ServantError IO ()
  clientWithRoute Proxy req baseurl manager =
    void $ performRequestNoBody H.methodPost req baseurl manager

-- | If you have a 'Post xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  ( MimeUnrender ct a, BuildHeadersTo ls
  ) => HasClient (Post (ct ': cts) (Headers ls a)) where
  type Client (Post (ct ': cts) (Headers ls a)) = ExceptT ServantError IO (Headers ls a)
  clientWithRoute Proxy req baseurl manager = do
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodPost req baseurl manager
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }

-- | If you have a 'Put' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
  (MimeUnrender ct a) => HasClient (Put (ct ': cts) a) where
  type Client (Put (ct ': cts) a) = ExceptT ServantError IO a
  clientWithRoute Proxy req baseurl manager =
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodPut req baseurl manager

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Put (ct ': cts) ()) where
  type Client (Put (ct ': cts) ()) = ExceptT ServantError IO ()
  clientWithRoute Proxy req baseurl manager =
    void $ performRequestNoBody H.methodPut req baseurl manager

-- | If you have a 'Put xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  ( MimeUnrender ct a, BuildHeadersTo ls
  ) => HasClient (Put (ct ': cts) (Headers ls a)) where
  type Client (Put (ct ': cts) (Headers ls a)) = ExceptT ServantError IO (Headers ls a)
  clientWithRoute Proxy req baseurl manager= do
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodPut req baseurl manager
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }

-- | If you have a 'Patch' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
  (MimeUnrender ct a) => HasClient (Patch (ct ': cts) a) where
  type Client (Patch (ct ': cts) a) = ExceptT ServantError IO a
  clientWithRoute Proxy req baseurl manager =
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodPatch req baseurl manager

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Patch (ct ': cts) ()) where
  type Client (Patch (ct ': cts) ()) = ExceptT ServantError IO ()
  clientWithRoute Proxy req baseurl manager =
    void $ performRequestNoBody H.methodPatch req baseurl manager

-- | If you have a 'Patch xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  ( MimeUnrender ct a, BuildHeadersTo ls
  ) => HasClient (Patch (ct ': cts) (Headers ls a)) where
  type Client (Patch (ct ': cts) (Headers ls a)) = ExceptT ServantError IO (Headers ls a)
  clientWithRoute Proxy req baseurl manager = do
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodPatch req baseurl manager
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }

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
-- > getBooksBy :: Maybe Text -> ExceptT String IO [Book]
-- > getBooksBy = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
instance (KnownSymbol sym, ToHttpApiData a, HasClient sublayout)
      => HasClient (QueryParam sym a :> sublayout) where

  type Client (QueryParam sym a :> sublayout) =
    Maybe a -> Client sublayout

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy req baseurl manager mparam =
    clientWithRoute (Proxy :: Proxy sublayout)
                    (maybe req
                           (flip (appendToQueryString pname) req . Just)
                           mparamText
                    )
                    baseurl
                    manager

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
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
-- > getBooksBy :: [Text] -> ExceptT String IO [Book]
-- > getBooksBy = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
instance (KnownSymbol sym, ToHttpApiData a, HasClient sublayout)
      => HasClient (QueryParams sym a :> sublayout) where

  type Client (QueryParams sym a :> sublayout) =
    [a] -> Client sublayout

  clientWithRoute Proxy req baseurl manager paramlist =
    clientWithRoute (Proxy :: Proxy sublayout)
                    (foldl' (\ req' -> maybe req' (flip (appendToQueryString pname) req' . Just))
                            req
                            paramlist'
                    )
                    baseurl manager

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
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
-- > getBooks :: Bool -> ExceptT String IO [Book]
-- > getBooks = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books
instance (KnownSymbol sym, HasClient sublayout)
      => HasClient (QueryFlag sym :> sublayout) where

  type Client (QueryFlag sym :> sublayout) =
    Bool -> Client sublayout

  clientWithRoute Proxy req baseurl manager flag =
    clientWithRoute (Proxy :: Proxy sublayout)
                    (if flag
                       then appendToQueryString paramname Nothing req
                       else req
                    )
                    baseurl manager

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)


-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the full `Response`.
instance HasClient Raw where
  type Client Raw = H.Method -> ExceptT ServantError IO (Int, ByteString, MediaType, [HTTP.Header], Response ByteString)

  clientWithRoute :: Proxy Raw -> Req -> BaseUrl -> Manager -> Client Raw
  clientWithRoute Proxy req baseurl manager httpMethod = do
    performRequest httpMethod req baseurl manager

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
-- > addBook :: Book -> ExceptT String IO Book
-- > addBook = client myApi host manager
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "addBook" to query that endpoint
instance (MimeRender ct a, HasClient sublayout)
      => HasClient (ReqBody (ct ': cts) a :> sublayout) where

  type Client (ReqBody (ct ': cts) a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req baseurl manager body =
    clientWithRoute (Proxy :: Proxy sublayout)
                    (let ctProxy = Proxy :: Proxy ct
                     in setRQBody (mimeRender ctProxy body)
                                  (contentType ctProxy)
                                  req
                    )
                    baseurl manager

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient sublayout) => HasClient (path :> sublayout) where
  type Client (path :> sublayout) = Client sublayout

  clientWithRoute Proxy req baseurl manager =
     clientWithRoute (Proxy :: Proxy sublayout)
                     (appendToPath p req)
                     baseurl manager

    where p = symbolVal (Proxy :: Proxy path)

instance HasClient api => HasClient (Vault :> api) where
  type Client (Vault :> api) = Client api

  clientWithRoute Proxy req baseurl manager =
    clientWithRoute (Proxy :: Proxy api) req baseurl manager

instance HasClient api => HasClient (RemoteHost :> api) where
  type Client (RemoteHost :> api) = Client api

  clientWithRoute Proxy req baseurl manager =
    clientWithRoute (Proxy :: Proxy api) req baseurl manager

instance HasClient api => HasClient (IsSecure :> api) where
  type Client (IsSecure :> api) = Client api

  clientWithRoute Proxy req baseurl manager =
    clientWithRoute (Proxy :: Proxy api) req baseurl manager
