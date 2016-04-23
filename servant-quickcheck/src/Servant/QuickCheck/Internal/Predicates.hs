module Servant.QuickCheck.Internal.Predicates where

import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Control.Monad
import Network.HTTP.Client (Request, Response, responseStatus)
import Network.HTTP.Types (status500)
import Data.Text (Text)

-- | @500 Internal Server Error@ should be avoided - it may represent some
-- issue with the application code, and it moreover gives the client little
-- indication of how to proceed or what went wrong.
--
-- This function checks that the response code is not 500.
not500 :: ResponsePredicate Text b Bool
not500 = ResponsePredicate "not500" (\resp -> responseStatus resp == status500)

{-
-- | Returning anything other than an object when returning JSON is considered
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
onlyJsonObjects :: Response b -> IO Bool
onlyJsonObjects
  = ResponsePredicate "onlyJsonObjects" _

-- | When creating a new resource, it is good practice to provide a @Location@
-- header with a link to the created resource.
--
-- This function checks that every @201 Created@ response contains a @Location@
-- header, and that the link in it responds with a 2XX response code to @GET@
-- requests.
--
-- References: <RFC 7231, Section 6.3.2 https://tools.ietf.org/html/rfc7231#section-6.3.2>
createContainsValidLocation :: Response b -> IO Bool
createContainsValidLocation
  = ResponsePredicate "createContainsValidLocation" _

getsHaveLastModifiedHeader :: Response b -> IO Bool
getsHaveLastModifiedHeader
  = ResponsePredicate "getsHaveLastModifiedHeader" _

-- | When an HTTP request has a method that is not allowed, a 405 response
-- should be returned. Additionally, it is good practice to return an @Allow@
-- header with the list of allowed methods.
--
-- This function checks that every @405 Method Not Allowed@ response contains
-- an @Allow@ header with a list of standard HTTP methods.
notAllowedContainsAllowHeader :: Response b -> IO Bool
notAllowedContainsAllowHeader
  = ResponsePredicate "notAllowedContainsAllowHeader" _

-- | When a request contains an @Accept@ header, the server must either return
-- content in one of the requested representations, or respond with @406 Not
-- Acceptable@.
--
-- This function checks that every *successful* response has a @Content-Type@
-- header that matches the @Accept@ header.
honoursAcceptHeader :: Predicate b Bool
honoursAcceptHeader
  = RequestPredicate "honoursAcceptHeader" _

-- | Whether or not a representation should be cached, it is good practice to
-- have a @Cache-Control@ header for @GET@ requests. If the representation
-- should not be cached, used @Cache-Control: no-cache@.
--
-- This function checks that @GET@ responses have a valid @Cache-Control@
-- header.
--
-- References: RFC 7234 Section 5.2
-- https://tools.ietf.org/html/rfc7234#section-5.2
getsHaveCacheControlHeader :: Predicate b Bool
getsHaveCacheControlHeader
  = ResponsePredicate "getsHaveCacheControlHeader" _

-- | Like 'getsHaveCacheControlHeader', but for @HEAD@ requests.
headsHaveCacheControlHeader :: Predicate b Bool
headsHaveCacheControlHeader
  = ResponsePredicate "headsHaveCacheControlHeader" _

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
-- References: RFC 5988 Section 5
-- https://tools.ietf.org/html/rfc5988
linkHeadersAreValid :: Predicate b Bool
linkHeadersAreValid
  = ResponsePredicate "linkHeadersAreValid" _

-- | Any @401 Unauthorized@ response must include a @WWW-Authenticate@ header.
--
-- This function checks that, if a response has status code 401, it contains a
-- @WWW-Authenticate@ header.
--
-- References: RFC 7235 Section 4.1
-- https://tools.ietf.org/html/rfc7235#section-4.1
unauthorizedContainsWWWAuthenticate :: Predicate b Bool
unauthorizedContainsWWWAuthenticate
  = ResponsePredicate "unauthorizedContainsWWWAuthenticate" _
-}

data ResponsePredicate n b r = ResponsePredicate
  { respPredName :: n
  , respPred :: Response b -> r
  } deriving (Functor, Generic)

instance (Monoid n, Monoid r) => Monoid (ResponsePredicate n b r) where
  mempty = ResponsePredicate mempty mempty
  a `mappend` b = ResponsePredicate
    { respPredName = respPredName a <> respPredName b
    , respPred = respPred a <> respPred b
    }

data RequestPredicate n b r = RequestPredicate
  { reqPredName :: n
  , reqPred :: Request -> ResponsePredicate n b r -> IO r
  } deriving (Generic)

instance (Monoid n, Monoid r) => Monoid (RequestPredicate n b r) where
  mempty = RequestPredicate mempty (\_ _ -> return mempty)
  a `mappend` b = RequestPredicate
    { reqPredName = reqPredName a <> reqPredName b
    , reqPred = \x y -> liftM2 (<>) (reqPred a x y) (reqPred b x y)
    }

data Predicates n b r = Predicates
  { reqPreds :: RequestPredicate n b r
  , respPreds :: ResponsePredicate n b r
  } deriving (Generic)

instance (Monoid n, Monoid r) => Monoid (Predicates n b r) where
  mempty = Predicates mempty mempty
  a `mappend` b = Predicates (reqPreds a <> reqPreds b) (respPreds a <> respPreds b)

class JoinPreds a n b r where
  joinPreds :: a -> Predicates n b r -> Predicates n b r

instance (Monoid n, Monoid r) => JoinPreds (RequestPredicate n b r) n b r where
  joinPreds p (Predicates x y) = Predicates (p <> x) y

instance (Monoid n, Monoid r) => JoinPreds (ResponsePredicate n b r) n b r where
  joinPreds p (Predicates x y) = Predicates x (p <> y)

infixr 6 <%>
(<%>) :: JoinPreds a n b r => a -> Predicates n b r -> Predicates n b r
(<%>) = joinPreds

finishPredicates :: (Monoid r) => Predicates n b r -> Request -> IO r
finishPredicates p req = (reqPred $ reqPreds p) req (respPreds p)
