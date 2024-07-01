{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# LANGUAGE PolyKinds          #-}
module Servant.API.Verbs
  ( module Servant.API.Verbs
  , StdMethod(GET, POST, HEAD, PUT, DELETE, TRACE, CONNECT, OPTIONS, PATCH)
  ) where

import           Data.Kind
                 (Type)
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
import Servant.API.MultiVerb (MultiVerb1, Respond)

-- | @Verb@ is a general type for representing HTTP verbs (a.k.a. methods). For
-- convenience, type synonyms for each verb with a 200 response code are
-- provided, but you are free to define your own:
--
-- >>> type Post204 contentTypes a = Verb 'POST 204 contentTypes a
-- data Verb (method :: k1) (statusCode :: Nat) (contentTypes :: [Type]) (a :: Type)
type Verb (method :: StdMethod) (statusCode :: Nat) (contentTypes :: [Type]) (returnType :: Type)
    = MultiVerb1 method contentTypes (Respond statusCode "" returnType) 

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
type Get contentTypes returnType   = Verb 'GET    200 contentTypes returnType
-- | 'POST' with 200 status code.
type Post   contentTypes returnType = Verb 'POST   200 contentTypes returnType 
-- | 'PUT' with 200 status code.
type Put    contentTypes returnType = Verb 'PUT    200 contentTypes returnType
-- | 'DELETE' with 200 status code.
type Delete contentTypes returnType = Verb 'DELETE 200 contentTypes returnType
-- | 'PATCH' with 200 status code.
type Patch  contentTypes returnType = Verb 'PATCH  200 contentTypes returnType 

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
type PostCreated contentTypes returnType = Verb 'POST 201 contentTypes returnType
-- | 'PUT' with 201 status code.
type PutCreated contentTypes returnType = Verb 'PUT 201 contentTypes returnType


-- ** 202 Accepted
--
-- Indicates that the request has been accepted for processing, but the
-- processing has not yet completed. The status of the processing should be
-- included, as well as either a link to a status monitoring endpoint or an
-- estimate of when the processing will be finished.

-- | 'GET' with 202 status code.
type GetAccepted    contentTypes returnType = Verb 'GET 202 contentTypes returnType
-- | 'POST' with 202 status code.
type PostAccepted   contentTypes returnType = Verb 'POST 202 contentTypes returnType
-- | 'DELETE' with 202 status code.
type DeleteAccepted contentTypes returnType = Verb 'DELETE 202 contentTypes returnType
-- | 'PATCH' with 202 status code.
type PatchAccepted  contentTypes returnType = Verb 'PATCH 202 contentTypes returnType
-- | 'PUT' with 202 status code.
type PutAccepted    contentTypes returnType = Verb 'PUT 202 contentTypes returnType


-- ** 203 Non-Authoritative Information
--
-- Indicates that the request has been successfully processed, but the
-- information may come from a third-party.

-- | 'GET' with 203 status code.
type GetNonAuthoritative    contentTypes returnType = Verb 'GET 203 contentTypes returnType
-- | 'POST' with 203 status code.
type PostNonAuthoritative   contentTypes returnType = Verb 'POST 203 contentTypes returnType
-- | 'DELETE' with 203 status code.
type DeleteNonAuthoritative contentTypes returnType = Verb 'DELETE 203 contentTypes returnType
-- | 'PATCH' with 203 status code.
type PatchNonAuthoritative  contentTypes returnType = Verb 'PATCH 203 contentTypes returnType
-- | 'PUT' with 203 status code.
type PutNonAuthoritative    contentTypes returnType = Verb 'PUT 203 contentTypes returnType


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
type GetResetContent    contentTypes returnType = Verb 'GET 205 contentTypes returnType
-- | 'POST' with 205 status code.
type PostResetContent   contentTypes returnType = Verb 'POST 205 contentTypes returnType
-- | 'DELETE' with 205 status code.
type DeleteResetContent contentTypes returnType = Verb 'DELETE 205 contentTypes returnType
-- | 'PATCH' with 205 status code.
type PatchResetContent  contentTypes returnType = Verb 'PATCH 205 contentTypes returnType
-- | 'PUT' with 205 status code.
type PutResetContent    contentTypes returnType = Verb 'PUT 205 contentTypes returnType


-- ** 206 Partial Content
--
-- Indicates that the server is delivering part of the resource due to a range
-- header in the request.
--
-- For more information, see <https://tools.ietf.org/html/rfc7233#section-4.1
-- RFC7233 Section 4.1>

-- | 'GET' with 206 status code.
type GetPartialContent contentTypes returnType = Verb 'GET 206 contentTypes returnType


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
