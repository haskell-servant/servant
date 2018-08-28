module Servant.QuickCheck.Internal.Predicates where

import           Control.Exception     (catch, throw)
import           Control.Monad         (liftM2, unless, when)
import           Data.Aeson            (Object, decode)
import           Data.Bifunctor        (first)
import qualified Data.ByteString       as SBS
import qualified Data.ByteString.Char8 as SBSC
import qualified Data.ByteString.Lazy  as LBS
import           Data.CaseInsensitive  (foldCase, foldedCase, mk)
import           Data.Either           (isRight)
import           Data.List.Split       (wordsBy)
import           Data.Maybe            (fromMaybe, isJust)
import           Data.Semigroup        (Semigroup (..))
import qualified Data.Text             as T
import           Data.Time             (UTCTime, defaultTimeLocale, parseTimeM,
                                        rfc822DateFormat)
import           GHC.Generics          (Generic)
import           Network.HTTP.Client   (Manager, Request, Response, httpLbs,
                                        method, parseRequest, requestHeaders,
                                        responseBody, responseHeaders,
                                        responseStatus)
import           Network.HTTP.Media    (matchAccept)
import           Network.HTTP.Types    (methodGet, methodHead, parseMethod,
                                        renderStdMethod, status100, status200,
                                        status201, status300, status401,
                                        status405, status500)
import           Prelude.Compat
import           System.Clock          (Clock (Monotonic), diffTimeSpec,
                                        getTime, toNanoSecs)

import Servant.QuickCheck.Internal.ErrorTypes


-- | [__Best Practice__]
--
-- @500 Internal Server Error@ should be avoided - it may represent some
-- issue with the application code, and it moreover gives the client little
-- indication of how to proceed or what went wrong.
--
-- This function checks that the response code is not 500.
--
-- /Since 0.0.0.0/
not500 :: ResponsePredicate
not500 = ResponsePredicate $ \resp ->
  when (responseStatus resp == status500) $ throw $ PredicateFailure "not500" Nothing resp

-- | [__Optional__]
--
-- This function checks that the response from the server does not take longer
-- than the specified number of nanoseconds.
--
-- /Since 0.0.2.1/
notLongerThan :: Integer -> RequestPredicate
notLongerThan maxAllowed
  = RequestPredicate $ \req mgr -> do
     start <- getTime Monotonic
     resp <- httpLbs req mgr
     end <- getTime Monotonic
     when (toNanoSecs (end `diffTimeSpec` start) > maxAllowed) $
       throw $ PredicateFailure "notLongerThan" (Just req) resp
     return []

-- | [__Best Practice__]
--
-- Returning anything other than an object when returning JSON is considered
-- bad practice, as:
--
--   (1) it is hard to modify the returned value while maintaining backwards
--   compatibility
--   (2) many older tools do not support top-level arrays
--   (3) whether top-level numbers, booleans, or strings are valid JSON depends
--   on what RFC you're going by
--   (4) there are security issues with top-level arrays
--
-- This function checks that any @application/json@ responses only return JSON
-- objects (and not arrays, strings, numbers, or booleans) at the top level.
--
-- __References__:
--
--   * JSON Grammar: <https://tools.ietf.org/html/rfc7159#section-2 RFC 7159 Section 2>
--   * JSON Grammar: <https://tools.ietf.org/html/rfc4627#section-2 RFC 4627 Section 2>
--
-- /Since 0.0.0.0/
onlyJsonObjects :: ResponsePredicate
onlyJsonObjects
  = ResponsePredicate (\resp -> do
    case lookup "content-type" (first foldedCase <$> responseHeaders resp) of
      Nothing    -> return ()
      Just ctype -> when ("application/json" `SBS.isPrefixOf` ctype) $ do
        case (decode (responseBody resp) :: Maybe Object) of
          Nothing -> throw $ PredicateFailure "onlyJsonObjects" Nothing resp
          Just _  -> return ())

-- | __Optional__
--
-- When creating a new resource, it is good practice to provide a @Location@
-- header with a link to the created resource.
--
-- This function checks that every @201 Created@ response contains a @Location@
-- header, and that the link in it responds with a 2XX response code to @GET@
-- requests.
--
-- This is considered optional because other means of linking to the resource
-- (e.g. via the response body) are also acceptable; linking to the resource in
-- some way is considered best practice.
--
-- __References__:
--
--   * 201 Created: <https://tools.ietf.org/html/rfc7231#section-6.3.2 RFC 7231 Section 6.3.2>
--   * Location header: <https://tools.ietf.org/html/rfc7231#section-7.1.2 RFC 7231 Section 7.1.2>
--
-- /Since 0.0.0.0/
createContainsValidLocation :: RequestPredicate
createContainsValidLocation
  = RequestPredicate $ \req mgr -> do
     let n = "createContainsValidLocation"
     resp <- httpLbs req mgr
     if responseStatus resp == status201
         then case lookup "Location" $ responseHeaders resp of
             Nothing -> throw $ PredicateFailure n (Just req) resp
             Just l  -> case parseRequest $ SBSC.unpack l of
               Nothing -> throw $ PredicateFailure n (Just req) resp
               Just x  -> do
                 resp2 <- httpLbs x mgr
                 status2XX (Just req) resp2 n
                 return [resp, resp2]
         else return [resp]

-- | [__Optional__]
--
-- The @Last-Modified@ header represents the time a resource was last
-- modified. It is used to drive caching and conditional requests.
--
-- When using this mechanism, the server adds the @Last-Modified@ header to
-- responses. Clients may then make requests with the @If-Modified-Since@
-- header to conditionally request resources. If the resource has not
-- changed since that date, the server responds with a status code of 304
-- (@Not Modified@) without a response body.
--
-- The @Last-Modified@ header can also be used in conjunction with the
-- @If-Unmodified-Since@ header to drive optimistic concurrency.
--
-- The @Last-Modified@ date must be in RFC 822 format.
--
-- __References__:
--
--   * 304 Not Modified: <https://tools.ietf.org/html/rfc7232#section-4.1 RFC 7232 Section 4.1>
--   * Last-Modified header: <https://tools.ietf.org/html/rfc7232#section-2.2 RFC 7232 Section 2.2>
--   * If-Modified-Since header: <https://tools.ietf.org/html/rfc7232#section-3.3 RFC 7232 Section 3.3>
--   * If-Unmodified-Since header: <https://tools.ietf.org/html/rfc7232#section-3.4 RFC 7232 Section 3.4>
--   * Date format: <https://tools.ietf.org/html/rfc2616#section-3.3 RFC 2616 Section 3.3>
--
-- /Since 0.0.2.1/
getsHaveLastModifiedHeader :: RequestPredicate
getsHaveLastModifiedHeader
  = RequestPredicate $ \req mgr ->
     if (method req == methodGet)
      then do
        resp <- httpLbs req mgr
        unless (hasValidHeader "Last-Modified" isRFC822Date resp) $ do
          throw $ PredicateFailure "getsHaveLastModifiedHeader" (Just req) resp
        return [resp]
      else return []



-- | [__RFC Compliance__]
--
-- When an HTTP request has a method that is not allowed,
-- a 405 response should be returned. Additionally, it is good practice to
-- return an @Allow@
-- header with the list of allowed methods.
--
-- This function checks that every @405 Method Not Allowed@ response contains
-- an @Allow@ header with a list of standard HTTP methods.
--
-- Note that 'servant' itself does not currently set the @Allow@ headers.
--
-- __References__:
--
--   * @Allow@ header: <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC 2616 Section 14.7>
--   * Status 405: <https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html RFC 2616 Section 10.4.6>
--   * Servant Allow header issue: <https://github.com/haskell-servant/servant/issues/489 Issue #489>
--
-- /Since 0.0.0.0/
notAllowedContainsAllowHeader :: RequestPredicate
notAllowedContainsAllowHeader
  = RequestPredicate $ \req mgr -> do
      resp <- mapM (flip httpLbs mgr) $ [ req { method = renderStdMethod m }
                                        | m <- [minBound .. maxBound ]
                                        , renderStdMethod m /= method req ]
      case filter pred' resp of
        (x:_) -> throw $ PredicateFailure "notAllowedContainsAllowHeader" (Just req) x
        []     -> return resp
    where
      pred' resp = responseStatus resp == status405 && not (hasValidHeader "Allow" go resp)
        where
          go x = all (\y -> isRight $ parseMethod $ SBSC.pack y)
               $ wordsBy (`elem` (", " :: [Char])) (SBSC.unpack x)


-- | [__RFC Compliance__]
--
-- When a request contains an @Accept@ header, the server must either return
-- content in one of the requested representations, or respond with @406 Not
-- Acceptable@.
--
-- This function checks that every *successful* response has a @Content-Type@
-- header that matches the @Accept@ header. It does *not* check that the server
-- matches the quality descriptions of the @Accept@ header correctly.
--
-- __References__:
--
--   * @Accept@ header: <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC 2616 Section 14.1>
--
-- /Since 0.0.0.0/
honoursAcceptHeader :: RequestPredicate
honoursAcceptHeader
  = RequestPredicate $ \req mgr -> do
      resp <- httpLbs req mgr
      let scode = responseStatus resp
          sctype = lookup "Content-Type" $ responseHeaders resp
          sacc  = fromMaybe "*/*" $ lookup "Accept" (requestHeaders req)
      if status100 < scode && scode < status300
        then if isJust $ sctype >>= \x -> matchAccept [x] sacc
          then throw $ PredicateFailure "honoursAcceptHeader" (Just req) resp
          else return [resp]
        else return [resp]


-- | [__Best Practice__]
--
-- Whether or not a representation should be cached, it is good practice to
-- have a @Cache-Control@ header for @GET@ requests. If the representation
-- should not be cached, used @Cache-Control: no-cache@.
--
-- This function checks that @GET@ responses have @Cache-Control@ header.
-- It does NOT currently check that the header is valid.
--
-- __References__:
--
--   * @Cache-Control@ header: <https://tools.ietf.org/html/rfc7234#section-5.2 RFC 7234 Section 5.2>
--
-- /Since 0.0.0.0/
getsHaveCacheControlHeader :: RequestPredicate
getsHaveCacheControlHeader
  = RequestPredicate $ \req mgr ->
     if (method req == methodGet)
      then do
        resp <- httpLbs req mgr
        unless (hasValidHeader "Cache-Control" (const True) resp) $ do
          throw $ PredicateFailure "getsHaveCacheControlHeader" (Just req) resp
        return [resp]
      else return []

-- | [__Best Practice__]
--
-- Like 'getsHaveCacheControlHeader', but for @HEAD@ requests.
--
-- /Since 0.0.0.0/
headsHaveCacheControlHeader :: RequestPredicate
headsHaveCacheControlHeader
  = RequestPredicate $ \req mgr ->
     if (method req == methodHead)
       then do
         resp <- httpLbs req mgr
         unless (hasValidHeader "Cache-Control" (const True) resp) $
           throw $ PredicateFailure "headsHaveCacheControlHeader" (Just req) resp
         return [resp]
       else return []
{-
-- |
--
-- If the original request modifies the resource, this function makes two
-- requests:
--
--   (1) Once, with the original request and a future date as the
--   @If-Unmodified-Since@, which is expected to succeed.
--   (2) Then with the original request again, with a @If-Unmodified-Since@
--   safely in the past. Since presumably the representation has been changed
--   recently (by the first request), this is expected to fail with @412
--   Precondition Failure@.
--
-- Note that the heuristic used to guess whether the original request modifies
-- a resource is simply whether the method is @PUT@ or @PATCH@, which may be
-- incorrect in certain circumstances.
supportsIfUnmodifiedSince :: Predicate b Bool
supportsIfUnmodifiedSince
  = ResponsePredicate "supportsIfUnmodifiedSince" _

-- | @OPTIONS@ responses should contain an @Allow@ header with the list of
-- allowed methods.
--
-- If a request is an @OPTIONS@ request, and if the response is a successful
-- one, this function checks the response for an @Allow@ header. It fails if:
--
--   (1) There is no @Allow@ header
--   (2) The @Allow@ header does not have standard HTTP methods in the correct
--   format
--   (3) Making a request to the same URL with one of those methods results in
--   a 404 or 405.
optionsContainsValidAllow :: Predicate b Bool
optionsContainsValidAllow
  = ResponsePredicate "optionsContainsValidAllow" _

-- | Link headers are a standardized way of presenting links that may be
-- relevant to a client.
--
-- This function checks that any @Link@ headers have values in the correct
-- format.
--
-- __References__:
--
--   * <https://tools.ietf.org/html/rfc5988 RFC 5988 Section 5>
linkHeadersAreValid :: Predicate b Bool
linkHeadersAreValid
  = ResponsePredicate "linkHeadersAreValid" _

-}
-- | [__RFC Compliance__]
--
-- Any @401 Unauthorized@ response must include a @WWW-Authenticate@ header.
--
-- This function checks that, if a response has status code 401, it contains a
-- @WWW-Authenticate@ header.
--
-- __References__:
--
--   * @WWW-Authenticate@ header: <https://tools.ietf.org/html/rfc7235#section-4.1 RFC 7235 Section 4.1>
--
-- /Since 0.0.0.0/
unauthorizedContainsWWWAuthenticate :: ResponsePredicate
unauthorizedContainsWWWAuthenticate
  = ResponsePredicate $ \resp ->
      if responseStatus resp == status401
        then unless (hasValidHeader "WWW-Authenticate" (const True) resp) $
          throw $ PredicateFailure "unauthorizedContainsWWWAuthenticate" Nothing resp
        else return ()


-- | [__RFC Compliance__]
--
-- [An HTML] document will start with exactly this string: <!DOCTYPE html>
--
-- This function checks that HTML documents (those with `Content-Type: text/html...`)
-- include a DOCTYPE declaration at the top. We do not enforce capital case for the string `DOCTYPE`.
--
-- __References__:
--
--  * HTML5 Doctype: <https://tools.ietf.org/html/rfc7992#section-6.1 RFC 7992 Section 6.1>
-- /Since 0.3.0.0/
htmlIncludesDoctype :: ResponsePredicate
htmlIncludesDoctype
  = ResponsePredicate $ \resp ->
      if hasValidHeader "Content-Type" (SBS.isPrefixOf . foldCase $ "text/html") resp
        then do
            let htmlContent = foldCase . LBS.take 20 $ responseBody resp
            unless (LBS.isPrefixOf (foldCase "<!doctype html>") htmlContent) $
              throw $ PredicateFailure "htmlIncludesDoctype" Nothing resp
        else return ()

-- * Predicate logic

-- The idea with all this footwork is to not waste any requests. Rather than
-- generating new requests and only applying one predicate to the response, we
-- apply as many predicates as possible.
--
-- Still, this is all kind of ugly.

-- | A predicate that depends only on the response.
--
-- /Since 0.0.0.0/
newtype ResponsePredicate = ResponsePredicate
  { getResponsePredicate :: Response LBS.ByteString -> IO ()
  } deriving (Generic)

instance Semigroup ResponsePredicate where
  ResponsePredicate a <> ResponsePredicate b = ResponsePredicate $ \x -> a x >> b x

instance Monoid ResponsePredicate where
  mempty = ResponsePredicate $ const $ return ()
  mappend = (<>)

-- | A predicate that depends on both the request and the response.
--
-- /Since 0.0.0.0/
newtype RequestPredicate = RequestPredicate
  { getRequestPredicate :: Request -> Manager -> IO [Response LBS.ByteString]
  } deriving (Generic)

-- TODO: This isn't actually a monoid
instance Monoid RequestPredicate where
  mempty = RequestPredicate (\r m -> httpLbs r m >>= \x -> return ([x]))
  mappend = (<>)

-- TODO: This isn't actually a monoid
instance Semigroup RequestPredicate where
  RequestPredicate a <> RequestPredicate b = RequestPredicate $ \r mgr ->
    liftM2 (<>) (a r mgr) (b r mgr)

-- | A set of predicates. Construct one with 'mempty' and '<%>'.
data Predicates = Predicates
  { requestPredicates  :: RequestPredicate
  , responsePredicates :: ResponsePredicate
  } deriving (Generic)

instance Semigroup Predicates where
  a <> b = Predicates (requestPredicates a <> requestPredicates b)
                      (responsePredicates a <> responsePredicates b)

instance Monoid Predicates where
  mempty = Predicates mempty mempty
  mappend = (<>)

class JoinPreds a where
  joinPreds :: a -> Predicates -> Predicates

instance JoinPreds (RequestPredicate ) where
  joinPreds p (Predicates x y) = Predicates (p <> x) y

instance JoinPreds (ResponsePredicate ) where
  joinPreds p (Predicates x y) = Predicates x (p <> y)

-- | Adds a new predicate (either `ResponsePredicate` or `RequestPredicate`) to
-- the existing predicates.
--
-- > not500 <%> onlyJsonObjects <%> empty
--
-- /Since 0.0.0.0/
(<%>) :: JoinPreds a => a -> Predicates -> Predicates
(<%>) = joinPreds
infixr 6 <%>

finishPredicates :: Predicates -> Request -> Manager -> IO (Maybe PredicateFailure)
finishPredicates p req mgr = go `catch` \(e :: PredicateFailure) -> return $ Just e
  where
    go = do
     resps <- getRequestPredicate (requestPredicates p) req mgr
     mapM_  (getResponsePredicate $ responsePredicates p) resps
     return Nothing

-- * helpers

hasValidHeader :: SBS.ByteString -> (SBS.ByteString -> Bool) -> Response b -> Bool
hasValidHeader hdr p r = case lookup (mk hdr) (responseHeaders r) of
  Nothing -> False
  Just v  -> p v

isRFC822Date :: SBS.ByteString -> Bool
isRFC822Date s
  = case parseTimeM True defaultTimeLocale rfc822DateFormat (SBSC.unpack s) of
    Nothing -> False
    Just (_ :: UTCTime) -> True

status2XX :: Monad m => Maybe Request -> Response LBS.ByteString -> T.Text -> m ()
status2XX mreq resp t
  | status200 <= responseStatus resp && responseStatus resp < status300
  = return ()
  | otherwise = throw $ PredicateFailure t mreq resp
