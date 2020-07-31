{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Client.Core.HasClient (
    clientIn,
    HasClient (..),
    EmptyClient (..),
    collapseUResp,
    extractUResp,
    ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
                 (left, (+++))
import           Control.Monad
                 (unless)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Either
                 (partitionEithers)
import           Data.Foldable
                 (toList)
import           Data.Functor.Identity
                 (Identity (Identity), runIdentity)
import           Data.List
                 (foldl')
import           Data.Sequence
                 (fromList)
import qualified Data.Text                       as T
import           Network.HTTP.Media
                 (MediaType, matches, parseAccept, (//))
import qualified Data.Sequence as Seq
import           Data.SOP.BasicFunctors
                 (K (K), (:.:) (Comp))
import           Data.SOP.Constraint
                 (All, Constraint)
import           Data.SOP.NP
                 (NP (..), cpure_NP)
import           Data.SOP.NS
                 (NS (S), cmap_NS, collapse_NS)
import           Data.String
                 (fromString)
import           Data.Text
                 (Text, pack)
import           Data.Typeable
                 (Proxy (Proxy), Typeable, cast)
import           GHC.TypeLits
                 (KnownSymbol, symbolVal)
import           Network.HTTP.Types
                 (Status)
import qualified Network.HTTP.Types                       as H
import           Servant.API
                 ((:<|>) ((:<|>)), (:>), AuthProtect, BasicAuth, BasicAuthData,
                 BuildHeadersTo (..), Capture', CaptureAll, Description,
                 EmptyAPI, FramingRender (..), FramingUnrender (..),
                 FromSourceIO (..), Header', Headers (..), HttpVersion,
                 IsSecure, MimeRender (mimeRender),
                 MimeUnrender (mimeUnrender), NoContent (NoContent), QueryFlag,
                 QueryParam', QueryParams, Raw, ReflectMethod (..), RemoteHost,
                 ReqBody', SBoolI, Stream, StreamBody', Summary, ToHttpApiData,
                 ToSourceIO (..), Vault, Verb, NoContentVerb, WithNamedContext,
                 contentType, getHeadersHList, getResponse, toQueryParam,
                 toUrlPiece)
import           Servant.API.ContentTypes
                 (contentTypes, AllMime (allMime), AllMimeUnrender (allMimeUnrender))
import           Servant.API.Modifiers
                 (FoldRequired, RequiredArgument, foldRequiredArgument)
import           Servant.API.UVerb
                 (HasStatus, Statuses, UVerb, Union, Unique, inject, statusOf)
import           Servant.API.UVerb.OpenUnion
                 (IsMember)

import           Servant.Client.Core.Auth
import           Servant.Client.Core.BasicAuth
import           Servant.Client.Core.ClientError
import           Servant.Client.Core.Request
import           Servant.Client.Core.Response
import           Servant.Client.Core.RunClient

-- * Accessing APIs as a Client

-- | 'clientIn' allows you to produce operations to query an API from a client
-- within a 'RunClient' monad.
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


-- | This class lets us define how each API combinator influences the creation
-- of an HTTP request.
--
-- Unless you are writing a new backend for @servant-client-core@ or new
-- combinators that you want to support client-generation, you can ignore this
-- class.
class RunClient m => HasClient m api where
  type Client (m :: * -> *) (api :: *) :: *
  clientWithRoute :: Proxy m -> Proxy api -> Request -> Client m api
  hoistClientMonad
    :: Proxy m
    -> Proxy api
    -> (forall x. mon x -> mon' x)
    -> Client mon api
    -> Client mon' api


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

  hoistClientMonad pm _ f (ca :<|> cb) =
    hoistClientMonad pm (Proxy :: Proxy a) f ca :<|>
    hoistClientMonad pm (Proxy :: Proxy b) f cb

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
  hoistClientMonad _ _ _ EmptyClient = EmptyClient

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
      => HasClient m (Capture' mods capture a :> api) where

  type Client m (Capture' mods capture a :> api) =
    a -> Client m api

  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api)
                    (appendToPath p req)

    where p = (toUrlPiece val)

  hoistClientMonad pm _ f cl = \a ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl a)

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

  hoistClientMonad pm _ f cl = \as ->
    hoistClientMonad pm (Proxy :: Proxy sublayout) f (cl as)

instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  ( RunClient m, MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts)
  ) => HasClient m (Verb method status cts' a) where
  type Client m (Verb method status cts' a) = m a
  clientWithRoute _pm Proxy req = do
    response <- runRequest req
      { requestAccept = fromList $ toList accept
      , requestMethod = method
      }
    response `decodedAs` (Proxy :: Proxy ct)
    where
      accept = contentTypes (Proxy :: Proxy ct)
      method = reflectMethod (Proxy :: Proxy method)

  hoistClientMonad _ _ f ma = f ma

instance {-# OVERLAPPING #-}
  ( RunClient m, ReflectMethod method
  ) => HasClient m (Verb method status cts NoContent) where
  type Client m (Verb method status cts NoContent)
    = m NoContent
  clientWithRoute _pm Proxy req = do
    _response <- runRequest req { requestMethod = method }
    return NoContent
      where method = reflectMethod (Proxy :: Proxy method)

  hoistClientMonad _ _ f ma = f ma

instance (RunClient m, ReflectMethod method) =>
         HasClient m (NoContentVerb method) where
  type Client m (NoContentVerb method)
    = m NoContent
  clientWithRoute _pm Proxy req = do
    _response <- runRequest req { requestMethod = method }
    return NoContent
      where method = reflectMethod (Proxy :: Proxy method)

  hoistClientMonad _ _ f ma = f ma

instance {-# OVERLAPPING #-}
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
    val <- response `decodedAs` (Proxy :: Proxy ct)
    return $ Headers { getResponse = val
                     , getHeadersHList = buildHeadersTo . toList $ responseHeaders response
                     }
      where method = reflectMethod (Proxy :: Proxy method)
            accept = contentTypes (Proxy :: Proxy ct)

  hoistClientMonad _ _ f ma = f ma

instance {-# OVERLAPPING #-}
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

  hoistClientMonad _ _ f ma = f ma

-- | Convenience function to apply a function to an unknown union element using a type class.
-- All elements of the union must have instances in the type class, and the function is
-- applied unconditionally.
--
-- See also: 'extractUResp'.
collapseUResp ::
  forall (c :: * -> Constraint) (a :: *) (as :: [*]).
  All c as =>
  Proxy c -> (forall x. c x => x -> a) -> Union as -> a
collapseUResp proxy render = collapse_NS . cmap_NS proxy (K . render . runIdentity)

-- | Convenience function to extract a union element using 'cast', ie. return the value if the
-- selected type happens to be the actual type of the union in this value, or 'Nothing'
-- otherwise.
--
-- See also: 'collapseUResp'.
extractUResp ::
  forall (a :: *) (as :: [*]).
  (All Typeable as, Typeable a, IsMember a as) =>
  Union as -> Maybe a
extractUResp = collapse_NS . cmap_NS (Proxy @Typeable) (K . cast . runIdentity)

data ClientParseError = ClientParseError MediaType String | ClientStatusMismatch | ClientNoMatchingStatus
  deriving (Eq, Show)

instance {-# OVERLAPPING #-}
  ( RunClient m,
    contentTypes ~ (contentType ': contentTypes'), -- TODO: can we to _ instead of contentTypes'?  probably not.
    as ~ (a ': as'),
    AllMime contentTypes,
    ReflectMethod method,
    All (AllMimeUnrender contentTypes) as,
    All HasStatus as,
    Unique (Statuses as)
  ) =>
  HasClient m (UVerb method contentTypes as)
  where
  type Client m (UVerb method contentTypes as) = m (Union as)

  clientWithRoute _ _ request = do
    let accept = Seq.fromList . allMime $ Proxy @contentTypes
        -- TODO(fisx): we want to send an accept header with, say, the first content type
        -- supported by the api, so we don't have to parse all of them, no?  not sure i'm
        -- missing anything here.

        method = reflectMethod $ Proxy @method
    response <- runRequest request {requestMethod = method, requestAccept = accept}
    responseContentType <- checkContentTypeHeader response
    unless (any (matches responseContentType) accept)
      $ throwClientError
      $ UnsupportedContentType responseContentType response

    let status = responseStatusCode response
        body = responseBody response
        res = tryParsers status $ mimeUnrenders (Proxy @contentTypes) body
    case res of
      Left errors -> throwClientError $ DecodeFailure (T.pack (show errors)) response
      Right x -> return x
    where
      -- | Given a list of parsers of 'mkres', returns the first one that succeeds and all the
      -- failures it encountered along the way
      -- TODO; better name, rewrite haddocs.
      tryParsers :: forall xs. All HasStatus xs => Status -> NP ([] :.: Either (MediaType, String)) xs -> Either [ClientParseError] (Union xs)
      tryParsers _ Nil = Left [ClientNoMatchingStatus]
      tryParsers status (Comp x :* xs)
        | status == statusOf (Comp x) =
          case partitionEithers x of
            (err', []) -> (map (uncurry ClientParseError) err' ++) +++ S $ tryParsers status xs
            (_, (res : _)) -> Right . inject . Identity $ res
        | otherwise = -- no reason to parse in the first place. This ain't the one we're looking for
          (ClientStatusMismatch :) +++ S $ tryParsers status xs

      -- | Given a list of types, parses the given response body as each type
      mimeUnrenders ::
        forall cts xs.
        All (AllMimeUnrender cts) xs =>
        Proxy cts ->
        BL.ByteString ->
        NP ([] :.: Either (MediaType, String)) xs
      mimeUnrenders ctp body = cpure_NP
        (Proxy @(AllMimeUnrender cts))
        (Comp . map (\(mediaType, parser) -> left ((,) mediaType) (parser body)) . allMimeUnrender $ ctp)

  hoistClientMonad _ _ nt s = nt s

instance {-# OVERLAPPABLE #-}
  ( RunStreamingClient m, MimeUnrender ct chunk, ReflectMethod method,
    FramingUnrender framing, FromSourceIO chunk a
  ) => HasClient m (Stream method status framing ct a) where

  type Client m (Stream method status framing ct a) = m a

  hoistClientMonad _ _ f ma = f ma

  clientWithRoute _pm Proxy req = withStreamingRequest req' $ \gres -> do
      let mimeUnrender'    = mimeUnrender (Proxy :: Proxy ct) :: BL.ByteString -> Either String chunk
          framingUnrender' = framingUnrender (Proxy :: Proxy framing) mimeUnrender'
      return $ fromSourceIO $ framingUnrender' $ responseBody gres
    where
      req' = req
          { requestAccept = fromList [contentType (Proxy :: Proxy ct)]
          , requestMethod = reflectMethod (Proxy :: Proxy method)
          }

instance {-# OVERLAPPING #-}
  ( RunStreamingClient m, MimeUnrender ct chunk, ReflectMethod method,
    FramingUnrender framing, FromSourceIO chunk a,
    BuildHeadersTo hs
  ) => HasClient m (Stream method status framing ct (Headers hs a)) where

  type Client m (Stream method status framing ct (Headers hs a)) = m (Headers hs a)

  hoistClientMonad _ _ f ma = f ma

  clientWithRoute _pm Proxy req = withStreamingRequest req' $ \gres -> do
      let mimeUnrender'    = mimeUnrender (Proxy :: Proxy ct) :: BL.ByteString -> Either String chunk
          framingUnrender' = framingUnrender (Proxy :: Proxy framing) mimeUnrender'
          val = fromSourceIO $ framingUnrender' $ responseBody gres
      return $ Headers
        { getResponse = val
        , getHeadersHList = buildHeadersTo . toList $ responseHeaders gres
        }

    where
      req' = req
          { requestAccept = fromList [contentType (Proxy :: Proxy ct)]
          , requestMethod = reflectMethod (Proxy :: Proxy method)
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
instance (KnownSymbol sym, ToHttpApiData a, HasClient m api, SBoolI (FoldRequired mods))
      => HasClient m (Header' mods sym a :> api) where

  type Client m (Header' mods sym a :> api) =
    RequiredArgument mods a -> Client m api

  clientWithRoute pm Proxy req mval =
    clientWithRoute pm (Proxy :: Proxy api) $ foldRequiredArgument
      (Proxy :: Proxy mods) add (maybe req add) mval
    where
      hname = fromString $ symbolVal (Proxy :: Proxy sym)

      add :: a -> Request
      add value = addHeader hname value req

  hoistClientMonad pm _ f cl = \arg ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl arg)

-- | Using a 'HttpVersion' combinator in your API doesn't affect the client
-- functions.
instance HasClient m api
  => HasClient m (HttpVersion :> api) where

  type Client m (HttpVersion :> api) =
    Client m api

  clientWithRoute pm Proxy =
    clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

-- | Ignore @'Summary'@ in client functions.
instance HasClient m api => HasClient m (Summary desc :> api) where
  type Client m (Summary desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

-- | Ignore @'Description'@ in client functions.
instance HasClient m api => HasClient m (Description desc :> api) where
  type Client m (Description desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

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
instance (KnownSymbol sym, ToHttpApiData a, HasClient m api, SBoolI (FoldRequired mods))
      => HasClient m (QueryParam' mods sym a :> api) where

  type Client m (QueryParam' mods sym a :> api) =
    RequiredArgument mods a -> Client m api

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute pm Proxy req mparam =
    clientWithRoute pm (Proxy :: Proxy api) $ foldRequiredArgument
      (Proxy :: Proxy mods) add (maybe req add) mparam
    where
      add :: a -> Request
      add param = appendToQueryString pname (Just $ toQueryParam param) req

      pname :: Text
      pname  = pack $ symbolVal (Proxy :: Proxy sym)

  hoistClientMonad pm _ f cl = \arg ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl arg)

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

  hoistClientMonad pm _ f cl = \as ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl as)

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

  hoistClientMonad pm _ f cl = \b ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl b)

-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the full `Response`.
instance RunClient m => HasClient m Raw where
  type Client m Raw
    = H.Method ->  m Response

  clientWithRoute :: Proxy m -> Proxy Raw -> Request -> Client m Raw
  clientWithRoute _pm Proxy req httpMethod = do
    runRequest req { requestMethod = httpMethod }

  hoistClientMonad _ _ f cl = \meth -> f (cl meth)

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
      => HasClient m (ReqBody' mods (ct ': cts) a :> api) where

  type Client m (ReqBody' mods (ct ': cts) a :> api) =
    a -> Client m api

  clientWithRoute pm Proxy req body =
    clientWithRoute pm (Proxy :: Proxy api)
                    (let ctProxy = Proxy :: Proxy ct
                     in setRequestBodyLBS (mimeRender ctProxy body)
                                          -- We use first contentType from the Accept list
                                          (contentType ctProxy)
                                          req
                    )

  hoistClientMonad pm _ f cl = \a ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl a)

instance
    ( HasClient m api, MimeRender ctype chunk, FramingRender framing, ToSourceIO chunk a
    ) => HasClient m (StreamBody' mods framing ctype a :> api)
  where

    type Client m (StreamBody' mods framing ctype a :> api) = a -> Client m api

    hoistClientMonad pm _ f cl = \a ->
      hoistClientMonad pm (Proxy :: Proxy api) f (cl a)

    clientWithRoute pm Proxy req body
        = clientWithRoute pm (Proxy :: Proxy api)
        $ setRequestBody (RequestBodySource sourceIO) (contentType ctypeP) req
      where
        ctypeP   = Proxy :: Proxy ctype
        framingP = Proxy :: Proxy framing

        sourceIO = framingRender
            framingP
            (mimeRender ctypeP :: chunk -> BL.ByteString)
            (toSourceIO body)

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient m api) => HasClient m (path :> api) where
  type Client m (path :> api) = Client m api

  clientWithRoute pm Proxy req =
     clientWithRoute pm (Proxy :: Proxy api)
                     (appendToPath p req)

    where p = pack $ symbolVal (Proxy :: Proxy path)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

instance HasClient m api => HasClient m (Vault :> api) where
  type Client m (Vault :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) req

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

instance HasClient m api => HasClient m (RemoteHost :> api) where
  type Client m (RemoteHost :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) req

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

instance HasClient m api => HasClient m (IsSecure :> api) where
  type Client m (IsSecure :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) req

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl

instance HasClient m subapi =>
  HasClient m (WithNamedContext name context subapi) where

  type Client m (WithNamedContext name context subapi) = Client m subapi
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy subapi)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy subapi) f cl

instance ( HasClient m api
         ) => HasClient m (AuthProtect tag :> api) where
  type Client m (AuthProtect tag :> api)
    = AuthenticatedRequest (AuthProtect tag) -> Client m api

  clientWithRoute pm Proxy req (AuthenticatedRequest (val,func)) =
    clientWithRoute pm (Proxy :: Proxy api) (func val req)

  hoistClientMonad pm _ f cl = \authreq ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl authreq)

-- * Basic Authentication

instance HasClient m api => HasClient m (BasicAuth realm usr :> api) where
  type Client m (BasicAuth realm usr :> api) = BasicAuthData -> Client m api

  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api) (basicAuthReq val req)

  hoistClientMonad pm _ f cl = \bauth ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl bauth)



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

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application"//"octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'

decodedAs :: forall ct a m. (MimeUnrender ct a, RunClient m)
  => Response -> Proxy ct -> m a
decodedAs response ct = do
  responseContentType <- checkContentTypeHeader response
  unless (any (matches responseContentType) accept) $
    throwClientError $ UnsupportedContentType responseContentType response
  case mimeUnrender ct $ responseBody response of
    Left err -> throwClientError $ DecodeFailure (T.pack err) response
    Right val -> return val
  where
    accept = toList $ contentTypes ct
