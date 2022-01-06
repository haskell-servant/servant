{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Verbs
  ( module Servant.API.Verbs
  , StdMethod(GET, POST, HEAD, PUT, DELETE, TRACE, CONNECT, OPTIONS, PATCH)
  ) where

import           Data.Proxy
                 (Proxy)
import           Data.Typeable
                 (Typeable)
import           GHC.Generics
                 (Generic)
import           GHC.TypeLits
                 (Nat)
import           Network.HTTP.Types.Method
                 (Method, StdMethod (..), methodConnect, methodDelete,
                 methodGet, methodHead, methodOptions, methodPatch, methodPost,
                 methodPut, methodTrace)

-- | @Verb@ is a general type for representing HTTP verbs (a.k.a. methods). For
-- convenience, type synonyms for each verb with a 200 response code are
-- provided, but you are free to define your own:
--
-- >>> type Post204 contentTypes a = Verb 'POST 204 contentTypes a
data Verb (method :: k1) (statusCode :: Nat) (contentTypes :: [*]) (a :: *)
  deriving (Typeable, Generic)

-- | @NoContentVerb@ is a specific type to represent 'NoContent' responses.
-- It does not require either a list of content types (because there's
-- no content) or a status code (because it should always be 204).
data NoContentVerb  (method :: k1)
  deriving (Typeable, Generic)

-- * 200 responses
--
-- The 200 response is the workhorse of web servers, but also fairly generic.
-- When appropriate, you should prefer the more specific success combinators.
-- More information about the definitions of status codes can be found in
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html RFC2615> and
-- <https://tools.ietf.org/html/rfc7231#section-6 RFC7231 Section 6>;
-- the relevant information is summarily presented here.

-- | 'GET' with 200 status code.
type Get    = Verb 'GET    200
-- | 'POST' with 200 status code.
type Post   = Verb 'POST   200
-- | 'PUT' with 200 status code.
type Put    = Verb 'PUT    200
-- | 'DELETE' with 200 status code.
type Delete = Verb 'DELETE 200
-- | 'PATCH' with 200 status code.
type Patch  = Verb 'PATCH  200

-- * Other responses

-- ** 201 Created
--
-- Indicates that a new resource has been created. The URI corresponding to the
-- resource should be given in the @Location@ header field.
--
-- If the operation is idempotent, use 'PutCreated'. If not, use 'PostCreated'
--
-- If the resource cannot be created immediately, use 'PostAccepted'.
--
-- Consider using 'Servant.Links.safeLink' for the @Location@ header
-- field.

-- | 'POST' with 201 status code.
type PostCreated = Verb 'POST 201
-- | 'PUT' with 201 status code.
type PutCreated = Verb 'PUT 201


-- ** 202 Accepted
--
-- Indicates that the request has been accepted for processing, but the
-- processing has not yet completed. The status of the processing should be
-- included, as well as either a link to a status monitoring endpoint or an
-- estimate of when the processing will be finished.

-- | 'GET' with 202 status code.
type GetAccepted    = Verb 'GET 202
-- | 'POST' with 202 status code.
type PostAccepted   = Verb 'POST 202
-- | 'DELETE' with 202 status code.
type DeleteAccepted = Verb 'DELETE 202
-- | 'PATCH' with 202 status code.
type PatchAccepted  = Verb 'PATCH 202
-- | 'PUT' with 202 status code.
type PutAccepted    = Verb 'PUT 202


-- ** 203 Non-Authoritative Information
--
-- Indicates that the request has been successfully processed, but the
-- information may come from a third-party.

-- | 'GET' with 203 status code.
type GetNonAuthoritative    = Verb 'GET 203
-- | 'POST' with 203 status code.
type PostNonAuthoritative   = Verb 'POST 203
-- | 'DELETE' with 203 status code.
type DeleteNonAuthoritative = Verb 'DELETE 203
-- | 'PATCH' with 203 status code.
type PatchNonAuthoritative  = Verb 'PATCH 203
-- | 'PUT' with 203 status code.
type PutNonAuthoritative    = Verb 'PUT 203


-- ** 204 No Content
--
-- Indicates that no response body is being returned. Handlers for these should
-- return 'NoContent', possibly with headers.
--
-- If the document view should be reset, use @205 Reset Content@.

-- | 'GET' with 204 status code.
type GetNoContent    = NoContentVerb 'GET
-- | 'POST' with 204 status code.
type PostNoContent   = NoContentVerb 'POST
-- | 'DELETE' with 204 status code.
type DeleteNoContent = NoContentVerb 'DELETE
-- | 'PATCH' with 204 status code.
type PatchNoContent  = NoContentVerb 'PATCH
-- | 'PUT' with 204 status code.
type PutNoContent    = NoContentVerb 'PUT
-- | 'HEAD' with 204 status code.
type HeadNoContent   = NoContentVerb 'HEAD

-- ** 205 Reset Content
--
-- Indicates that no response body is being returned. Handlers for these should
-- return 'NoContent', possibly with Headers.
--
-- If the document view should not be reset, use @204 No Content@.

-- | 'GET' with 205 status code.
type GetResetContent    = Verb 'GET 205
-- | 'POST' with 205 status code.
type PostResetContent   = Verb 'POST 205
-- | 'DELETE' with 205 status code.
type DeleteResetContent = Verb 'DELETE 205
-- | 'PATCH' with 205 status code.
type PatchResetContent  = Verb 'PATCH 205
-- | 'PUT' with 205 status code.
type PutResetContent    = Verb 'PUT 205


-- ** 206 Partial Content
--
-- Indicates that the server is delivering part of the resource due to a range
-- header in the request.
--
-- For more information, see <https://tools.ietf.org/html/rfc7233#section-4.1
-- RFC7233 Section 4.1>

-- | 'GET' with 206 status code.
type GetPartialContent = Verb 'GET 206


class ReflectMethod a where
    reflectMethod :: Proxy a -> Method

instance ReflectMethod 'GET where
    reflectMethod _ = methodGet

instance ReflectMethod 'POST where
    reflectMethod _ = methodPost

instance ReflectMethod 'PUT where
    reflectMethod _ = methodPut

instance ReflectMethod 'DELETE where
    reflectMethod _ = methodDelete

instance ReflectMethod 'PATCH where
    reflectMethod _ = methodPatch

instance ReflectMethod 'HEAD where
    reflectMethod _ = methodHead

instance ReflectMethod 'OPTIONS where
    reflectMethod _ = methodOptions

instance ReflectMethod 'TRACE where
    reflectMethod _ = methodTrace

instance ReflectMethod 'CONNECT where
    reflectMethod _ = methodConnect
