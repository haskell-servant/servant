{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Servant.Client.Core.HasClient
  ( clientIn
  , HasClient (..)
  , EmptyClient (..)
  , AsClientT
  , (//)
  , (/:)
  , foldMapUnion
  , matchUnion
  , fromSomeClientResponse
  )
where

import Control.Arrow (left, (+++))
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BSL
import Data.Constraint (Dict (..))
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Kind (Type)
import qualified Data.List as List
import Data.SOP.BasicFunctors (I (I), (:.:) (Comp))
import Data.SOP.Constraint (All)
import Data.SOP.NP (NP (..), cpure_NP)
import Data.SOP.NS (NS (..))
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import Data.String (fromString)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import GHC.TypeLits (KnownSymbol, TypeError, symbolVal)
import Network.HTTP.Media (MediaType, matches, parseAccept)
import qualified Network.HTTP.Media as M
import qualified Network.HTTP.Media as Media
import Network.HTTP.Types (Status)
import qualified Network.HTTP.Types as H
import Prelude.Compat
import Servant.API
  ( AuthProtect
  , BasicAuth
  , BasicAuthData
  , BuildHeadersTo (..)
  , Capture'
  , CaptureAll
  , DeepQuery
  , Description
  , EmptyAPI
  , Fragment
  , FramingRender (..)
  , FramingUnrender (..)
  , FromSourceIO (..)
  , Header'
  , Headers (..)
  , Host
  , HttpVersion
  , IsSecure
  , MimeRender (mimeRender)
  , MimeUnrender (mimeUnrender)
  , NamedRoutes
  , NoContent (NoContent)
  , NoContentVerb
  , OperationId
  , QueryFlag
  , QueryParam'
  , QueryParams
  , QueryString
  , Raw
  , RawM
  , ReflectMethod (..)
  , RemoteHost
  , ReqBody'
  , SBoolI
  , Stream
  , StreamBody'
  , Summary
  , ToHttpApiData
  , ToSourceIO (..)
  , Vault
  , Verb
  , WithNamedContext
  , WithResource
  , WithStatus (..)
  , contentType
  , getHeadersHList
  , getResponse
  , toEncodedUrlPiece
  , (:<|>) ((:<|>))
  , (:>)
  )
import Servant.API.ContentTypes
  ( AllMime (allMime)
  , AllMimeUnrender (allMimeUnrender)
  , EventStream
  )
import Servant.API.Generic
  ( GenericMode (..)
  , GenericServant
  , ToServant
  , ToServantApi
  , fromServant
  , toServant
  )
import Servant.API.Modifiers
  ( FoldRequired
  , RequiredArgument
  , foldRequiredArgument
  )
import Servant.API.MultiVerb
import Servant.API.QueryString (ToDeepQuery (..), generateDeepParam)
import Servant.API.ServerSentEvents
  ( EventKind (JsonEvent, RawEvent)
  , ServerSentEvents'
  )
import Servant.API.Status (KnownStatus)
import Servant.API.Stream (NoFraming)
import Servant.API.TypeErrors
import Servant.API.TypeLevel (AtMostOneFragment, FragmentUnique)
import Servant.API.UVerb
  ( HasStatus
  , HasStatuses (Statuses, statuses)
  , UVerb
  , Union
  , Unique
  , foldMapUnion
  , inject
  , matchUnion
  , statusOf
  )
import Prelude ()

import Servant.Client.Core.Auth
import Servant.Client.Core.BasicAuth
import Servant.Client.Core.ClientError
import Servant.Client.Core.MultiVerb.ResponseUnrender
import Servant.Client.Core.Request
import Servant.Client.Core.Response
import qualified Servant.Client.Core.Response as Response
import Servant.Client.Core.RunClient
import Servant.Client.Core.ServerSentEvents

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
  type Client (m :: Type -> Type) (api :: Type) :: Type
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
    clientWithRoute pm (Proxy :: Proxy a) req
      :<|> clientWithRoute pm (Proxy :: Proxy b) req

  hoistClientMonad pm _ f (ca :<|> cb) =
    hoistClientMonad pm (Proxy :: Proxy a) f ca
      :<|> hoistClientMonad pm (Proxy :: Proxy b) f cb

-- | Singleton type representing a client for an empty API.
data EmptyClient = EmptyClient deriving (Bounded, Enum, Eq, Show)

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
instance
  (HasClient m api, ToHttpApiData a)
  => HasClient m (Capture' mods capture a :> api)
  where
  type
    Client m (Capture' mods capture a :> api) =
      a -> Client m api

  clientWithRoute pm Proxy req val =
    clientWithRoute
      pm
      (Proxy :: Proxy api)
      (appendToPath p req)
    where
      p = toEncodedUrlPiece val

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

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
instance
  (HasClient m sublayout, ToHttpApiData a)
  => HasClient m (CaptureAll capture a :> sublayout)
  where
  type
    Client m (CaptureAll capture a :> sublayout) =
      [a] -> Client m sublayout

  clientWithRoute pm Proxy req vals =
    clientWithRoute
      pm
      (Proxy :: Proxy sublayout)
      (List.foldl' (flip appendToPath) req ps)
    where
      ps = map toEncodedUrlPiece vals

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy sublayout) f . cl

instance
  {-# OVERLAPPABLE #-}
  ( HasClientContentCheck ctypes
  , ReflectMethod method
  , ResponseListUnrender ctypes '[Respond status "" a]
  , RunClient m
  )
  => HasClient m (Verb method status ctypes a)
  where
  type Client m (Verb method status ctypes a) = m a
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy @(MultiVerb method ctypes '[Respond status "" a] a))
  hoistClientMonad _ _ f = f

instance
  {-# OVERLAPPING #-}
  ( KnownStatus status
  , ReflectMethod method
  , RunClient m
  )
  => HasClient m (Verb method status ctypes NoContent)
  where
  type Client m (Verb method status ctypes NoContent) = m NoContent
  clientWithRoute pm Proxy req =
    NoContent
      <$ clientWithRoute pm (Proxy @(MultiVerb method '() '[RespondAs '() status "" ()] ())) req
  hoistClientMonad _ _ f = f

instance
  {-# OVERLAPPING #-}
  ( HasClientContentCheck ctypes
  , ReflectMethod method
  , ResponseListUnrender ctypes '[WithHeaders h (Headers h a) (Respond status "" a)]
  , RunClient m
  )
  => HasClient m (Verb method status ctypes (Headers h a))
  where
  type Client m (Verb method status ctypes (Headers h a)) = m (Headers h a)
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy @(MultiVerb method ctypes '[WithHeaders h (Headers h a) (Respond status "" a)] (Headers h a)))
  hoistClientMonad _ _ f = f

instance
  {-# OVERLAPPING #-}
  ( ReflectMethod method
  , ResponseListUnrender '() '[WithHeaders h (Headers h NoContent) (RespondAs '() status "" ())]
  , RunClient m
  )
  => HasClient m (Verb method status ctypes (Headers h NoContent))
  where
  type Client m (Verb method status ctypes (Headers h NoContent)) = m (Headers h NoContent)
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy @(MultiVerb method '() '[WithHeaders h (Headers h NoContent) (RespondAs '() status "" ())] (Headers h NoContent)))
  hoistClientMonad _ _ f = f

instance
  (ReflectMethod method, RunClient m)
  => HasClient m (NoContentVerb method)
  where
  type Client m (NoContentVerb method) = m NoContent
  clientWithRoute pm Proxy req =
    NoContent
      <$ clientWithRoute pm (Proxy @(MultiVerb method '() '[RespondAs '() 204 "" ()] ())) req
  hoistClientMonad _ _ f = f

data ClientParseError = ClientParseError MediaType String | ClientStatusMismatch | ClientNoMatchingStatus
  deriving (Eq, Show)

class UnrenderResponse (cts :: [Type]) (a :: Type) where
  unrenderResponse
    :: Seq.Seq H.Header
    -> BSL.ByteString
    -> Proxy cts
    -> [Either (MediaType, String) a]

instance {-# OVERLAPPABLE #-} AllMimeUnrender cts a => UnrenderResponse cts a where
  unrenderResponse _ body = map parse . allMimeUnrender
    where
      parse (mediaType, parser) = left (mediaType,) (parser body)

instance
  {-# OVERLAPPING #-}
  forall cts a h
   . (BuildHeadersTo h, UnrenderResponse cts a)
  => UnrenderResponse cts (Headers h a)
  where
  unrenderResponse hs body = (map . fmap) setHeaders . unrenderResponse hs body
    where
      setHeaders :: a -> Headers h a
      setHeaders x = Headers x (buildHeadersTo (toList hs))

instance
  {-# OVERLAPPING #-}
  UnrenderResponse cts a
  => UnrenderResponse cts (WithStatus n a)
  where
  unrenderResponse hs body = (map . fmap) WithStatus . unrenderResponse hs body

instance
  {-# OVERLAPPING #-}
  ( -- ('otherContentTypes' should be '_', but even -XPartialTypeSignatures does not seem
    -- allow this in instance types as of 8.8.3.)

    All (UnrenderResponse contentTypes) as
  , All HasStatus as
  , AllMime contentTypes
  , HasStatuses as'
  , ReflectMethod method
  , RunClient m
  , Unique (Statuses as)
  , as ~ (a ': as')
  , contentTypes ~ (contentType ': otherContentTypes)
  )
  => HasClient m (UVerb method contentTypes as)
  where
  type Client m (UVerb method contentTypes as) = m (Union as)

  clientWithRoute _ _ request = do
    let accept = Seq.fromList . allMime $ Proxy @contentTypes
        -- offering to accept all mime types listed in the api gives best compatibility.  eg.,
        -- we might not own the server implementation, and the server may choose to support
        -- only part of the api.

        method = reflectMethod $ Proxy @method
        acceptStatus = statuses (Proxy @as)
    response@Response{responseBody = body, responseStatusCode = status, responseHeaders = headers} <-
      runRequestAcceptStatus (Just acceptStatus) (request{requestMethod = method, requestAccept = accept})
    responseContentType <- checkContentTypeHeader response
    unless (any (matches responseContentType) accept) $ do
      throwClientError $ UnsupportedContentType responseContentType response

    let res = tryParsers status $ mimeUnrenders (Proxy @contentTypes) headers body
    case res of
      Left errors -> throwClientError $ DecodeFailure (T.pack (show errors)) response
      Right x -> pure x
    where
      -- \| Given a list of parsers of 'mkres', returns the first one that succeeds and all the
      -- failures it encountered along the way
      -- TODO; better name, rewrite haddocs.
      tryParsers :: forall xs. All HasStatus xs => Status -> NP ([] :.: Either (MediaType, String)) xs -> Either [ClientParseError] (Union xs)
      tryParsers _ Nil = Left [ClientNoMatchingStatus]
      tryParsers status (Comp x :* xs)
        | status == statusOf (Comp x) =
            case partitionEithers x of
              (err', []) -> (map (uncurry ClientParseError) err' ++) +++ S $ tryParsers status xs
              (_, res : _) -> Right . inject . I $ res
        | otherwise -- no reason to parse in the first place. This ain't the one we're looking for
          =
            (ClientStatusMismatch :) +++ S $ tryParsers status xs

      -- \| Given a list of types, parses the given response body as each type
      mimeUnrenders
        :: forall cts xs
         . All (UnrenderResponse cts) xs
        => Proxy cts
        -> Seq.Seq H.Header
        -> BSL.ByteString
        -> NP ([] :.: Either (MediaType, String)) xs
      mimeUnrenders ctp headers body =
        cpure_NP
          (Proxy @(UnrenderResponse cts))
          (Comp . unrenderResponse headers body $ ctp)

  hoistClientMonad _ _ nt = nt

instance
  {-# OVERLAPPABLE #-}
  ( FramingUnrender framing
  , FromSourceIO chunk a
  , MimeUnrender ct chunk
  , ReflectMethod method
  , RunStreamingClient m
  )
  => HasClient m (Stream method status framing ct a)
  where
  type Client m (Stream method status framing ct a) = m a

  hoistClientMonad _ _ f = f

  clientWithRoute _pm Proxy req = withStreamingRequest req' $ \Response{responseBody = body} -> do
    let mimeUnrender' = mimeUnrender (Proxy :: Proxy ct) :: BSL.ByteString -> Either String chunk
        framingUnrender' = framingUnrender (Proxy :: Proxy framing) mimeUnrender'
    fromSourceIO $ framingUnrender' body
    where
      req' =
        req
          { requestAccept = fromList [contentType (Proxy :: Proxy ct)]
          , requestMethod = reflectMethod (Proxy :: Proxy method)
          }

instance
  {-# OVERLAPPING #-}
  ( BuildHeadersTo hs
  , FramingUnrender framing
  , FromSourceIO chunk a
  , MimeUnrender ct chunk
  , ReflectMethod method
  , RunStreamingClient m
  )
  => HasClient m (Stream method status framing ct (Headers hs a))
  where
  type Client m (Stream method status framing ct (Headers hs a)) = m (Headers hs a)

  hoistClientMonad _ _ f = f

  clientWithRoute _pm Proxy req = withStreamingRequest req' $
    \Response{responseBody = body, responseHeaders = headers} -> do
      let mimeUnrender' = mimeUnrender (Proxy :: Proxy ct) :: BSL.ByteString -> Either String chunk
          framingUnrender' = framingUnrender (Proxy :: Proxy framing) mimeUnrender'
      val <- fromSourceIO $ framingUnrender' body
      pure $
        Headers
          { getResponse = val
          , getHeadersHList = buildHeadersTo $ toList headers
          }
    where
      req' =
        req
          { requestAccept = fromList [contentType (Proxy :: Proxy ct)]
          , requestMethod = reflectMethod (Proxy :: Proxy method)
          }

type SseClientDelegate method status =
  Stream method status NoFraming EventStream

instance
  ( HasClient m (SseClientDelegate method status (EventMessageStreamT IO))
  , RunClient m
  )
  => HasClient m (ServerSentEvents' method status 'RawEvent EventMessage)
  where
  type
    Client m (ServerSentEvents' method status 'RawEvent EventMessage) =
      Client m (SseClientDelegate method status (EventMessageStreamT IO))

  hoistClientMonad p _ =
    hoistClientMonad
      p
      (Proxy :: Proxy (SseClientDelegate method status (EventMessageStreamT IO)))

  clientWithRoute p _ =
    clientWithRoute
      p
      (Proxy :: Proxy (SseClientDelegate method status (EventMessageStreamT IO)))

instance
  ( HasClient m (SseClientDelegate method status (EventStreamT IO))
  , RunClient m
  )
  => HasClient m (ServerSentEvents' method status 'RawEvent (Event a))
  where
  type
    Client m (ServerSentEvents' method status 'RawEvent (Event a)) =
      Client m (SseClientDelegate method status (EventStreamT IO))

  hoistClientMonad p _ =
    hoistClientMonad
      p
      (Proxy :: Proxy (SseClientDelegate method status (EventStreamT IO)))

  clientWithRoute p _ =
    clientWithRoute
      p
      (Proxy :: Proxy (SseClientDelegate method status (EventStreamT IO)))

instance
  ( HasClient m (SseClientDelegate method status (JsonEventStreamT IO a))
  , RunClient m
  )
  => HasClient m (ServerSentEvents' method status 'JsonEvent a)
  where
  type
    Client m (ServerSentEvents' method status 'JsonEvent a) =
      Client m (SseClientDelegate method status (JsonEventStreamT IO a))

  hoistClientMonad p _ =
    hoistClientMonad
      p
      (Proxy :: Proxy (SseClientDelegate method status (JsonEventStreamT IO a)))

  clientWithRoute p _ =
    clientWithRoute
      p
      (Proxy :: Proxy (SseClientDelegate method status (JsonEventStreamT IO a)))

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
instance
  (HasClient m api, KnownSymbol sym, SBoolI (FoldRequired mods), ToHttpApiData a)
  => HasClient m (Header' mods sym a :> api)
  where
  type
    Client m (Header' mods sym a :> api) =
      RequiredArgument mods a -> Client m api

  clientWithRoute pm Proxy req mval =
    clientWithRoute pm (Proxy :: Proxy api) $
      foldRequiredArgument
        (Proxy :: Proxy mods)
        add
        (maybe req add)
        mval
    where
      hname = fromString $ symbolVal (Proxy :: Proxy sym)

      add :: a -> Request
      add value = addHeader hname value req

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

instance (HasClient m api, KnownSymbol sym) => HasClient m (Host sym :> api) where
  type Client m (Host sym :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute pm (Proxy :: Proxy api) $
      addHeader "Host" (symbolVal (Proxy :: Proxy sym)) req

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- | Using a 'HttpVersion' combinator in your API doesn't affect the client
-- functions.
instance
  HasClient m api
  => HasClient m (HttpVersion :> api)
  where
  type
    Client m (HttpVersion :> api) =
      Client m api

  clientWithRoute pm Proxy =
    clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- | Ignore @'Summary'@ in client functions.
instance HasClient m api => HasClient m (Summary desc :> api) where
  type Client m (Summary desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- | Ignore @'OperationId'@ in client functions.
instance HasClient m api => HasClient m (OperationId operationId :> api) where
  type Client m (OperationId operationId :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- | Ignore @'Description'@ in client functions.
instance HasClient m api => HasClient m (Description desc :> api) where
  type Client m (Description desc :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

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
instance
  (HasClient m api, KnownSymbol sym, SBoolI (FoldRequired mods), ToHttpApiData a)
  => HasClient m (QueryParam' mods sym a :> api)
  where
  type
    Client m (QueryParam' mods sym a :> api) =
      RequiredArgument mods a -> Client m api

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute pm Proxy req mparam =
    clientWithRoute pm (Proxy :: Proxy api) $
      foldRequiredArgument
        (Proxy :: Proxy mods)
        add
        (maybe req add)
        mparam
    where
      add :: a -> Request
      add param = appendToQueryString pname (Just $ encodeQueryParamValue param) req

      pname :: Text
      pname = pack $ symbolVal (Proxy :: Proxy sym)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

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
instance
  (HasClient m api, KnownSymbol sym, ToHttpApiData a)
  => HasClient m (QueryParams sym a :> api)
  where
  type
    Client m (QueryParams sym a :> api) =
      [a] -> Client m api

  clientWithRoute pm Proxy req paramlist =
    clientWithRoute
      pm
      (Proxy :: Proxy api)
      ( List.foldl'
          (\req' -> maybe req' (flip (appendToQueryString pname) req' . Just))
          req
          paramlist'
      )
    where
      pname = pack $ symbolVal (Proxy :: Proxy sym)
      paramlist' = map (Just . encodeQueryParamValue) paramlist

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

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
instance
  (HasClient m api, KnownSymbol sym)
  => HasClient m (QueryFlag sym :> api)
  where
  type
    Client m (QueryFlag sym :> api) =
      Bool -> Client m api

  clientWithRoute pm Proxy req flag =
    clientWithRoute
      pm
      (Proxy :: Proxy api)
      ( if flag
          then appendToQueryString paramname Nothing req
          else req
      )
    where
      paramname = pack $ symbolVal (Proxy :: Proxy sym)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

instance
  HasClient m api
  => HasClient m (QueryString :> api)
  where
  type
    Client m (QueryString :> api) =
      H.Query -> Client m api

  clientWithRoute pm Proxy req query =
    clientWithRoute
      pm
      (Proxy :: Proxy api)
      (setQueryString query req)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

instance
  (HasClient m api, KnownSymbol sym, ToDeepQuery a)
  => HasClient m (DeepQuery sym a :> api)
  where
  type
    Client m (DeepQuery sym a :> api) =
      a -> Client m api

  clientWithRoute pm Proxy req deepObject =
    let params = toDeepQuery deepObject
        withParams = List.foldl' addDeepParam req params
        addDeepParam r' kv =
          let (k, textV) = generateDeepParam paramname kv
           in appendToQueryString k (encodeUtf8 <$> textV) r'
        paramname = pack $ symbolVal (Proxy :: Proxy sym)
     in clientWithRoute
          pm
          (Proxy :: Proxy api)
          withParams

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the full `Response`.
instance RunClient m => HasClient m Raw where
  type
    Client m Raw =
      H.Method -> m Response

  clientWithRoute :: Proxy m -> Proxy Raw -> Request -> Client m Raw
  clientWithRoute _pm Proxy req httpMethod = do
    runRequest req{requestMethod = httpMethod}

  hoistClientMonad _ _ f cl = f . cl

instance RunClient m => HasClient m RawM where
  type
    Client m RawM =
      H.Method -> m Response

  clientWithRoute :: Proxy m -> Proxy RawM -> Request -> Client m RawM
  clientWithRoute _pm Proxy req httpMethod = do
    runRequest req{requestMethod = httpMethod}

  hoistClientMonad _ _ f cl = f . cl

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
instance
  (HasClient m api, MimeRender ct a)
  => HasClient m (ReqBody' mods (ct ': cts) a :> api)
  where
  type
    Client m (ReqBody' mods (ct ': cts) a :> api) =
      a -> Client m api

  clientWithRoute pm Proxy req body =
    clientWithRoute
      pm
      (Proxy :: Proxy api)
      ( let ctProxy = Proxy :: Proxy ct
         in setRequestBodyLBS
              (mimeRender ctProxy body)
              -- We use first contentType from the Accept list
              (contentType ctProxy)
              req
      )

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

instance
  ( FramingRender framing
  , HasClient m api
  , MimeRender ctype chunk
  , ToSourceIO chunk a
  )
  => HasClient m (StreamBody' mods framing ctype a :> api)
  where
  type Client m (StreamBody' mods framing ctype a :> api) = a -> Client m api

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

  clientWithRoute pm Proxy req body =
    clientWithRoute pm (Proxy :: Proxy api) $
      setRequestBody (RequestBodySource sourceIO) (contentType ctypeP) req
    where
      ctypeP = Proxy :: Proxy ctype
      framingP = Proxy :: Proxy framing

      sourceIO =
        framingRender
          framingP
          (mimeRender ctypeP :: chunk -> BSL.ByteString)
          (toSourceIO body)

-- | Make the querying function append @path@ to the request path.
instance (HasClient m api, KnownSymbol path) => HasClient m (path :> api) where
  type Client m (path :> api) = Client m api

  clientWithRoute pm Proxy req =
    clientWithRoute
      pm
      (Proxy :: Proxy api)
      (appendToPath p req)
    where
      p = toEncodedUrlPiece $ pack $ symbolVal (Proxy :: Proxy path)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

instance HasClient m api => HasClient m (Vault :> api) where
  type Client m (Vault :> api) = Client m api

  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

instance HasClient m api => HasClient m (RemoteHost :> api) where
  type Client m (RemoteHost :> api) = Client m api

  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

instance HasClient m api => HasClient m (IsSecure :> api) where
  type Client m (IsSecure :> api) = Client m api

  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

instance
  HasClient m subapi
  => HasClient m (WithNamedContext name context subapi)
  where
  type Client m (WithNamedContext name context subapi) = Client m subapi
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy subapi)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy subapi)

instance
  HasClient m subapi
  => HasClient m (WithResource res :> subapi)
  where
  type Client m (WithResource res :> subapi) = Client m subapi
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy subapi)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy subapi)

instance
  HasClient m api
  => HasClient m (AuthProtect tag :> api)
  where
  type
    Client m (AuthProtect tag :> api) =
      AuthenticatedRequest (AuthProtect tag) -> Client m api

  clientWithRoute pm Proxy req (AuthenticatedRequest (val, func)) =
    clientWithRoute pm (Proxy :: Proxy api) (func val req)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

-- | Ignore @'Fragment'@ in client functions.
-- See <https://ietf.org/rfc/rfc2616.html#section-15.1.3> for more details.
--
-- Example:
--
-- > type MyApi = "books" :> Fragment Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooks :: ClientM [Book]
-- > getBooks = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooks' for all books.
instance
  ( AtMostOneFragment api
  , FragmentUnique (Fragment a :> api)
  , HasClient m api
  )
  => HasClient m (Fragment a :> api)
  where
  type Client m (Fragment a :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- * Basic Authentication

instance HasClient m api => HasClient m (BasicAuth realm usr :> api) where
  type Client m (BasicAuth realm usr :> api) = BasicAuthData -> Client m api

  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api) (basicAuthReq val req)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

-- | A type that specifies that an API record contains a client implementation.
data AsClientT (m :: Type -> Type)

instance GenericMode (AsClientT m) where
  type AsClientT m :- api = Client m api

type GClientConstraints api m =
  ( GenericServant api (AsClientT m)
  , Client m (ToServantApi api) ~ ToServant api (AsClientT m)
  )

class GClient (api :: Type -> Type) m where
  gClientProof :: Dict (GClientConstraints api m)

instance GClientConstraints api m => GClient api m where
  gClientProof = Dict

instance
  ( ErrorIfNoGeneric api
  , HasClient m (ToServantApi api)
  , RunClient m
  , forall n. GClient api n
  )
  => HasClient m (NamedRoutes api)
  where
  type Client m (NamedRoutes api) = api (AsClientT m)

  clientWithRoute :: Proxy m -> Proxy (NamedRoutes api) -> Request -> Client m (NamedRoutes api)
  clientWithRoute pm _ request =
    case gClientProof @api @m of
      Dict -> fromServant $ clientWithRoute pm (Proxy @(ToServantApi api)) request

  hoistClientMonad
    :: forall ma mb
     . Proxy m
    -> Proxy (NamedRoutes api)
    -> (forall x. ma x -> mb x)
    -> Client ma (NamedRoutes api)
    -> Client mb (NamedRoutes api)
  hoistClientMonad _ _ nat clientA =
    case (gClientProof @api @ma, gClientProof @api @mb) of
      (Dict, Dict) ->
        fromServant @api @(AsClientT mb) $
          hoistClientMonad @m @(ToServantApi api) @ma @mb Proxy Proxy nat $
            toServant @api @(AsClientT ma) clientA

infixl 1 //

infixl 2 /:

-- | Helper to make code using records of clients more readable.
--
-- Can be mixed with (/:) for supplying arguments.
--
-- Example:
--
-- @
-- type Api = NamedRoutes RootApi
--
-- data RootApi mode = RootApi
--   { subApi :: mode :- NamedRoutes SubApi
--   , …
--   } deriving Generic
--
-- data SubApi mode = SubApi
--   { endpoint :: mode :- Get '[JSON] Person
--   , …
--   } deriving Generic
--
-- api :: Proxy API
-- api = Proxy
--
-- rootClient :: RootApi (AsClientT ClientM)
-- rootClient = client api
--
-- endpointClient :: ClientM Person
-- endpointClient = client \/\/ subApi \/\/ endpoint
-- @
(//) :: a -> (a -> b) -> b
x // f = f x

-- | Convenience function for supplying arguments to client functions when
-- working with records of clients.
--
-- Intended to be used in conjunction with '(//)'.
--
-- Example:
--
-- @
-- type Api = NamedRoutes RootApi
--
-- data RootApi mode = RootApi
--   { subApi :: mode :- Capture "token" String :> NamedRoutes SubApi
--   , hello :: mode :- Capture "name" String :> Get '[JSON] String
--   , …
--   } deriving Generic
--
-- data SubApi mode = SubApi
--   { endpoint :: mode :- Get '[JSON] Person
--   , …
--   } deriving Generic
--
-- api :: Proxy API
-- api = Proxy
--
-- rootClient :: RootApi (AsClientT ClientM)
-- rootClient = client api
--
-- hello :: String -> ClientM String
-- hello name = rootClient \/\/ hello \/: name
--
-- endpointClient :: ClientM Person
-- endpointClient = client \/\/ subApi \/: "foobar123" \/\/ endpoint
-- @
(/:) :: (a -> b -> c) -> b -> a -> c
(/:) = flip

class HasClientContentCheck cs where
  clientAcceptList :: Proxy cs -> [M.MediaType]
  clientContentTypeOk :: Proxy cs -> M.MediaType -> Bool

instance AllMime cs => HasClientContentCheck cs where
  clientAcceptList = allMime
  clientContentTypeOk p c = any (M.matches c) (allMime p)

instance HasClientContentCheck '() where
  clientAcceptList _ = []
  clientContentTypeOk _ _ = True

instance
  ( AsUnion as r
  , HasClientContentCheck cs
  , ReflectMethod method
  , ResponseListUnrender cs as
  , RunClient m
  )
  => HasClient m (MultiVerb method cs as r)
  where
  type Client m (MultiVerb method cs as r) = m r

  clientWithRoute _ _ req = do
    response@Response{responseBody = body} <-
      runRequestAcceptStatus
        (Just (responseListStatuses @cs @as))
        req
          { requestMethod = method
          , requestAccept = Seq.fromList accept
          }

    c <- getResponseContentType response
    unless (clientContentTypeOk (Proxy @cs) c) $ do
      throwClientError $ UnsupportedContentType c response

    -- NOTE: support streaming in the future
    let sresp =
          if BSL.null body
            then SomeClientResponse $ response{Response.responseBody = ()}
            else SomeClientResponse response
    case responseListUnrender @cs @as c sresp of
      StatusMismatch -> throwClientError (DecodeFailure "Status mismatch" response)
      UnrenderError e -> throwClientError (DecodeFailure (Text.pack e) response)
      UnrenderSuccess x -> pure (fromUnion @as x)
    where
      accept = clientAcceptList (Proxy @cs)
      method = reflectMethod (Proxy @method)

  hoistClientMonad _ _ f = f

getResponseContentType :: RunClient m => Response -> m M.MediaType
getResponseContentType response =
  case lookup "Content-Type" (toList (responseHeaders response)) of
    Nothing -> pure $ "application" M.// "octet-stream"
    Just t -> case M.parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> pure t'

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
    Nothing -> pure $ "application" Media.// "octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> pure t'

-------------------------------------------------------------------------------
-- Custom type errors
-------------------------------------------------------------------------------

-- Erroring instance for HasClient' when a combinator is not fully applied
instance (RunClient m, TypeError (PartialApplication HasClient arr)) => HasClient m ((arr :: a -> b) :> sub) where
  type Client m (arr :> sub) = TypeError (PartialApplication HasClient arr)
  clientWithRoute _ _ _ = error "unreachable"
  hoistClientMonad _ _ _ _ = error "unreachable"

-- Erroring instances for 'HasClient' for unknown API combinators
instance {-# OVERLAPPABLE #-} (RunClient m, TypeError (NoInstanceForSub (HasClient m) ty)) => HasClient m (ty :> sub)

instance {-# OVERLAPPABLE #-} (RunClient m, TypeError (NoInstanceFor (HasClient m api))) => HasClient m api
