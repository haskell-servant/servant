{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Server.Internal where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (unfoldr)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Monoid, mempty, mappend)
import Data.String (fromString)
import Data.String.Conversions (cs, (<>))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types hiding (Header)
import Network.Wai ( Response, Request, ResponseReceived, Application
                   , pathInfo, requestBody, strictRequestBody
                   , lazyRequestBody, requestHeaders, requestMethod,
                    rawQueryString, responseLBS)
import Servant.API ( QueryParams, QueryParam, QueryFlag, ReqBody, Header
                   , MatrixParams, MatrixParam, MatrixFlag,
                   , Capture, Get, Delete, Put, Post, Patch, Raw, (:>), (:<|>)(..))
import Servant.Server.ContentTypes ( AllCTRender(..), AcceptHeader(..)
                                   , AllCTUnrender(..) )
import Servant.Common.Text (FromText, fromText)


data ReqBodyState = Uncalled
                  | Called !B.ByteString
                  | Done !B.ByteString

toApplication :: RoutingApplication -> Application
toApplication ra request respond = do
  reqBodyRef <- newIORef Uncalled
  -- We may need to consume the requestBody more than once.  In order to
  -- maintain the illusion that 'requestBody' works as expected,
  -- 'ReqBodyState' is introduced, and the complete body is memoized and
  -- returned as many times as requested with empty "Done" marker chunks in
  -- between.
  -- See https://github.com/haskell-servant/servant/issues/3
  let memoReqBody = do
          ior <- readIORef reqBodyRef
          case ior of
            Uncalled -> do
                r <- BL.toStrict <$> strictRequestBody request
                writeIORef reqBodyRef $ Done r
                return r
            Called bs -> do
                writeIORef reqBodyRef $ Done bs
                return bs
            Done bs -> do
                writeIORef reqBodyRef $ Called bs
                return B.empty

  ra request{ requestBody = memoReqBody } (routingRespond . routeResult)
 where
  routingRespond :: Either RouteMismatch Response -> IO ResponseReceived
  routingRespond (Left NotFound) =
    respond $ responseLBS notFound404 [] "not found"
  routingRespond (Left WrongMethod) =
    respond $ responseLBS methodNotAllowed405 [] "method not allowed"
  routingRespond (Left (InvalidBody err)) =
    respond $ responseLBS badRequest400 [] $ fromString $ "Invalid JSON in request body: " ++ err
  routingRespond (Left (HttpError status body)) =
    respond $ responseLBS status [] $ fromMaybe (BL.fromStrict $ statusMessage status) body
  routingRespond (Right response) =
    respond response

-- * Route mismatch
data RouteMismatch =
    NotFound           -- ^ the usual "not found" error
  | WrongMethod        -- ^ a more informative "you just got the HTTP method wrong" error
  | InvalidBody String -- ^ an even more informative "your json request body wasn't valid" error
  | HttpError Status (Maybe BL.ByteString)  -- ^ an even even more informative arbitrary HTTP response code error.
  deriving (Eq, Show)

-- |
-- @
-- > mempty = NotFound
-- >
-- > _             `mappend` HttpError s b = HttpError s b
-- > HttpError s b `mappend`             _ = HttpError s b
-- > NotFound      `mappend`             x = x
-- > WrongMethod   `mappend` InvalidBody s = InvalidBody s
-- > WrongMethod   `mappend`             _ = WrongMethod
-- > InvalidBody s `mappend`             _ = InvalidBody s
-- @
instance Monoid RouteMismatch where
  mempty = NotFound

  _             `mappend` HttpError s b = HttpError s b
  HttpError s b `mappend`             _ = HttpError s b
  NotFound      `mappend`             x = x
  WrongMethod   `mappend` InvalidBody s = InvalidBody s
  WrongMethod   `mappend`             _ = WrongMethod
  InvalidBody s `mappend`             _ = InvalidBody s

-- | A wrapper around @'Either' 'RouteMismatch' a@.
newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Eq, Show)

failWith :: RouteMismatch -> RouteResult a
failWith = RR . Left

succeedWith :: a -> RouteResult a
succeedWith = RR . Right

isMismatch :: RouteResult a -> Bool
isMismatch (RR (Left _)) = True
isMismatch _             = False

-- | Like `null . pathInfo`, but works with redundant trailing slashes.
pathIsEmpty :: Request -> Bool
pathIsEmpty = f . processedPathInfo
  where
    f []   = True
    f [""] = True
    f _    = False

-- | If we get a `Right`, it has precedence over everything else.
--
-- This in particular means that if we could get several 'Right's,
-- only the first we encounter would be taken into account.
instance Monoid (RouteResult a) where
  mempty = RR $ Left mempty

  RR (Left x)  `mappend` RR (Left y)  = RR $ Left (x <> y)
  RR (Left _)  `mappend` RR (Right y) = RR $ Right y
  r            `mappend` _            = r

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

splitMatrixParameters :: Text -> (Text, Text)
splitMatrixParameters = T.break (== ';')

parsePathInfo :: Request -> [Text]
parsePathInfo = filter (/= "") . mergePairs . map splitMatrixParameters . pathInfo
  where mergePairs = concat . unfoldr pairToList
        pairToList []          = Nothing
        pairToList ((a, b):xs) = Just ([a, b], xs)

-- | Returns a processed pathInfo from the request.
--
-- In order to handle matrix parameters in the request correctly, the raw pathInfo needs to be
-- processed, so routing works as intended. Therefor this function should be used to access
-- the pathInfo for routing purposes.
processedPathInfo :: Request -> [Text]
processedPathInfo r =
  case pinfo of
    (x:xs) | T.head x == ';' -> xs
    _                        -> pinfo
  where pinfo = parsePathInfo r

class HasServer layout where
  type Server layout :: *
  route :: Proxy layout -> Server layout -> RoutingApplication



-- * Instances

-- | A server for @a ':<|>' b@ first tries to match the request against the route
--   represented by @a@ and if it fails tries @b@. You must provide a request
--   handler for each route.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (a :<|> b) request respond =
    route pa a request $ \ mResponse ->
      if isMismatch mResponse
        then route pb b request $ \mResponse' -> respond (mResponse <> mResponse')
        else respond mResponse

    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = fromText

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> EitherT (Int, String) IO Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type Server (Capture capture a :> sublayout) =
     a -> Server sublayout

  route Proxy subserver request respond = case processedPathInfo request of
    (first : rest)
      -> case captured captureProxy first of
           Nothing  -> respond $ failWith NotFound
           Just v   -> route (Proxy :: Proxy sublayout) (subserver v) request{
                         pathInfo = rest
                       } respond
    _ -> respond $ failWith NotFound

    where captureProxy = Proxy :: Proxy (Capture capture a)


-- | If you have a 'Delete' endpoint in your API,
-- the handler for this endpoint is meant to delete
-- a resource.
--
-- The code of the handler will, just like
-- for 'Servant.API.Get.Get', 'Servant.API.Post.Post' and
-- 'Servant.API.Put.Put', run in @EitherT (Int, String) IO ()@.
-- The 'Int' represents the status code and the 'String' a message
-- to be returned. You can use 'Control.Monad.Trans.Either.left' to
-- painlessly error out if the conditions for a successful deletion
-- are not met.
instance HasServer Delete where
  type Server Delete = EitherT (Int, String) IO ()

  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodDelete = do
        e <- runEitherT action
        respond $ succeedWith $ case e of
          Right () ->
            responseLBS status204 [] ""
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | pathIsEmpty request && requestMethod request /= methodDelete =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

-- | When implementing the handler for a 'Get' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Post.Post'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT (Int, String) IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 200). If there was no @Accept@ header or it
-- was @*/*@, we return encode using the first @Content-Type@ type on the
-- list.
instance ( AllCTRender ctypes a
         ) => HasServer (Get ctypes a) where
  type Server (Get ctypes a) = EitherT (Int, String) IO a
  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodGet = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right output -> do
            let accH = fromMaybe "*/*" $ lookup hAccept $ requestHeaders request
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
              Nothing -> responseLBS (mkStatus 406 "") [] ""
              Just (contentT, body) -> responseLBS ok200 [ ("Content-Type"
                                                         , cs contentT)] body
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | pathIsEmpty request && requestMethod request /= methodGet =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromText' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> EitherT (Int, String) IO referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (Header sym a :> sublayout) where

  type Server (Header sym a :> sublayout) =
    Maybe a -> Server sublayout

  route Proxy subserver request respond = do
    let mheader = fromText . decodeUtf8 =<< lookup str (requestHeaders request)
    route (Proxy :: Proxy sublayout) (subserver mheader) request respond

      where str = fromString $ symbolVal (Proxy :: Proxy sym)

-- | When implementing the handler for a 'Post' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT (Int, String) IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 201). If there was no @Accept@ header or it
-- was @*/*@, we return encode using the first @Content-Type@ type on the
-- list.
instance ( AllCTRender ctypes a
         ) => HasServer (Post ctypes a) where
  type Server (Post ctypes a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodPost = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right output -> do
            let accH = fromMaybe "*/*" $ lookup hAccept $ requestHeaders request
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
              Nothing -> responseLBS (mkStatus 406 "") [] ""
              Just (contentT, body) -> responseLBS status201 [ ("Content-Type"
                                                             , cs contentT)] body
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | pathIsEmpty request && requestMethod request /= methodPost =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

-- | When implementing the handler for a 'Put' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Post.Post', the handler code runs in the
-- @EitherT (Int, String) IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 201). If there was no @Accept@ header or it
-- was @*/*@, we return encode using the first @Content-Type@ type on the
-- list.
instance ( AllCTRender ctypes a
         ) => HasServer (Put ctypes a) where
  type Server (Put ctypes a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodPut = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right output -> do
            let accH = fromMaybe "*/*" $ lookup hAccept $ requestHeaders request
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
              Nothing -> responseLBS (mkStatus 406 "") [] ""
              Just (contentT, body) -> responseLBS status200 [ ("Content-Type"
                                                             , cs contentT)] body
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | pathIsEmpty request && requestMethod request /= methodPut =
        respond $ failWith WrongMethod

    | otherwise = respond $ failWith NotFound

-- | When implementing the handler for a 'Patch' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT (Int, String) IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we just require that its type has
-- a 'ToJSON' instance and servant takes care of encoding it for you,
-- yielding status code 201 along the way.
instance (Typeable a, ToJSON a) => HasServer (Patch a) where
  type Server (Patch a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodPost = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right out -> case cast out of
              Nothing -> responseLBS status200 [("Content-Type", "application/json")] (encode out)
              Just () -> responseLBS status204 [] ""
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | pathIsEmpty request && requestMethod request /= methodPost =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

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
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
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
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
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

-- | If you use @'QueryFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
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

parseMatrixText :: B.ByteString -> QueryText
parseMatrixText = parseQueryText

-- | If you use @'MatrixParam' "author" Text@ in one of the endpoints for your API,
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
-- > type MyApi = "books" :> MatrixParam "author" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> EitherT (Int, String) IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (MatrixParam sym a :> sublayout) where

  type Server (MatrixParam sym a :> sublayout) =
    Maybe a -> Server sublayout

  route Proxy subserver request respond = case parsePathInfo request of
    (first : _)
      -> do let querytext = parseMatrixText . encodeUtf8 $ T.tail first
                param = case lookup paramname querytext of
                  Nothing       -> Nothing -- param absent from the query string
                  Just Nothing  -> Nothing -- param present with no value -> Nothing
                  Just (Just v) -> fromText v -- if present, we try to convert to
                                        -- the right type
            route (Proxy :: Proxy sublayout) (subserver param) request respond
    _ -> route (Proxy :: Proxy sublayout) (subserver Nothing) request respond

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | If you use @'MatrixParams' "authors" Text@ in one of the endpoints for your API,
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
-- > type MyApi = "books" :> MatrixParams "authors" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT (Int, String) IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (MatrixParams sym a :> sublayout) where

  type Server (MatrixParams sym a :> sublayout) =
    [a] -> Server sublayout

  route Proxy subserver request respond = case parsePathInfo request of
    (first : _)
      -> do let matrixtext = parseMatrixText . encodeUtf8 $ T.tail first
                -- if sym is "foo", we look for matrix parameters
                -- named "foo" or "foo[]" and call fromText on the
                -- corresponding values
                parameters = filter looksLikeParam matrixtext
                values = catMaybes $ map (convert . snd) parameters
            route (Proxy :: Proxy sublayout) (subserver values) request respond
    _ -> route (Proxy :: Proxy sublayout) (subserver []) request respond

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = fromText v

-- | If you use @'MatrixFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixFlag "published" :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> EitherT (Int, String) IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (MatrixFlag sym :> sublayout) where

  type Server (MatrixFlag sym :> sublayout) =
    Bool -> Server sublayout

  route Proxy subserver request respond =  case parsePathInfo request of
    (first : _)
      -> do let matrixtext = parseMatrixText . encodeUtf8 $ T.tail first
                param = case lookup paramname matrixtext of
                  Just Nothing  -> True  -- param is there, with no value
                  Just (Just v) -> examine v -- param with a value
                  Nothing       -> False -- param not in the query string

            route (Proxy :: Proxy sublayout) (subserver param) request respond

    _ -> route (Proxy :: Proxy sublayout) (subserver False) request respond

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False

-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> Raw
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer Raw where
  type Server Raw = Application
  route Proxy rawApplication request respond =
    rawApplication request (respond . succeedWith)

-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- The @Content-Type@ header is inspected, and the list provided is used to
-- attempt deserialization. If the request does not have a @Content-Type@
-- header, it is treated as @application/octet-stream@.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
--
-- All it asks is for a 'FromJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = postBook
-- >   where postBook :: Book -> EitherT (Int, String) IO Book
-- >         postBook book = ...insert into your db...
instance ( AllCTUnrender list a, HasServer sublayout
         ) => HasServer (ReqBody list a :> sublayout) where

  type Server (ReqBody list a :> sublayout) =
    a -> Server sublayout

  route Proxy subserver request respond = do
    -- See HTTP RFC 2616, section 7.2.1
    -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
    -- See also "W3C Internet Media Type registration, consistency of use"
    -- http://www.w3.org/2001/tag/2002/0129-mime
    let contentTypeH = fromMaybe "application/octet-stream"
                     $ lookup hContentType $ requestHeaders request
    mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH)
           <$> lazyRequestBody request
    case mrqbody of
      Left e -> respond . failWith $ InvalidBody e
      Right v  -> route (Proxy :: Proxy sublayout) (subserver v) request respond

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where
  type Server (path :> sublayout) = Server sublayout
  route Proxy subserver request respond = case processedPathInfo request of
    (first : rest)
      | first == cs (symbolVal proxyPath)
      -> route (Proxy :: Proxy sublayout) subserver request{
           pathInfo = rest
         } respond
    _ -> respond $ failWith NotFound

    where proxyPath = Proxy :: Proxy path
