{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Verbs where

import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           GHC.TypeLits              (Nat)
import           Network.HTTP.Types.Method (Method, StdMethod (..),
                                            methodDelete, methodGet, methodHead,
                                            methodPatch, methodPost, methodPut)

-- | @Verb@ is a general type for representing HTTP verbs/methods. For
-- convenience, type synonyms for each verb with a 200 response code are
-- provided, but you are free to define your own:
--
-- >>> type Post204 contentTypes a = Verb 'POST 204 contentTypes a
data Verb (method :: k1) (statusCode :: Nat) (contentTypes :: [*]) a
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
type Get    contentTypes a = Verb 'GET    200 contentTypes a
-- | 'POST' with 200 status code.
type Post   contentTypes a = Verb 'POST   200 contentTypes a
-- | 'PUT' with 200 status code.
type Put    contentTypes a = Verb 'PUT    200 contentTypes a
-- | 'DELETE' with 200 status code.
type Delete contentTypes a = Verb 'DELETE 200 contentTypes a
-- | 'PATCH' with 200 status code.
type Patch  contentTypes a = Verb 'PATCH  200 contentTypes a

-- * Other responses

-- ** 201 Created
--
-- Indicates that a new resource has been created. The URI corresponding to the
-- resource should be given in the @Location@ header field.
--
-- If the resource cannot be created immediately, use 'PostAccepted'.
--
-- Consider using 'Servant.Utils.Links.safeLink' for the @Location@ header
-- field.

-- | 'POST' with 201 status code.
--
type Created contentTypes a = Verb 'POST 201 contentTypes a


-- ** 202 Accepted
--
-- Indicates that the request has been accepted for processing, but the
-- processing has not yet completed. The status of the processing should be
-- included, as well as either a link to a status monitoring endpoint or an
-- estimate of when the processing will be finished.

-- | 'GET' with 202 status code.
type GetAccepted contentTypes a = Verb 'GET 202 contentTypes a
-- | 'POST' with 202 status code.
type PostAccepted contentTypes a = Verb 'POST 202 contentTypes a
-- | 'DELETE' with 202 status code.
type DeleteAccepted contentTypes a = Verb 'DELETE 202 contentTypes a
-- | 'PATCH' with 202 status code.
type PatchAccepted contentTypes a = Verb 'PATCH 202 contentTypes a
-- | 'PUT' with 202 status code.
type PutAccepted contentTypes a = Verb 'PUT 202 contentTypes a


-- ** 203 Non-Authoritative Information
--
-- Indicates that the request has been successfully processed, but the
-- information may come from a third-party.

-- | 'GET' with 203 status code.
type GetNonAuthoritative contentTypes a = Verb 'GET 203 contentTypes a
-- | 'POST' with 203 status code.
type PostNonAuthoritative contentTypes a = Verb 'POST 203 contentTypes a
-- | 'DELETE' with 203 status code.
type DeleteNonAuthoritative contentTypes a = Verb 'DELETE 203 contentTypes a
-- | 'PATCH' with 203 status code.
type PatchNonAuthoritative contentTypes a = Verb 'PATCH 203 contentTypes a
-- | 'PUT' with 203 status code.
type PutNonAuthoritative contentTypes a = Verb 'PUT 203 contentTypes a


-- ** 204 No Content
--
-- Indicates that no response body is being returned. Handlers for these must
-- return 'NoContent'.
--
-- If the document view should be reset, use @205 Reset Content@.

-- | 'GET' with 204 status code.
type GetNoContent contentTypes = Verb 'GET 204 contentTypes NoContent
-- | 'POST' with 204 status code.
type PostNoContent contentTypes = Verb 'POST 204 contentTypes NoContent
-- | 'DELETE' with 204 status code.
type DeleteNoContent contentTypes = Verb 'DELETE 204 contentTypes NoContent
-- | 'PATCH' with 204 status code.
type PatchNoContent contentTypes = Verb 'PATCH 204 contentTypes NoContent
-- | 'PUT' with 204 status code.
type PutNoContent contentTypes = Verb 'PUT 204 contentTypes NoContent


-- ** 205 Reset Content
--
-- Indicates that no response body is being returned. Handlers for these must
-- return 'NoContent'.
--
-- If the document view should not be reset, use @204 No Content@.

-- | 'GET' with 205 status code.
type GetResetContent contentTypes = Verb 'GET 205 contentTypes NoContent
-- | 'POST' with 205 status code.
type PostResetContent contentTypes = Verb 'POST 205 contentTypes NoContent
-- | 'DELETE' with 205 status code.
type DeleteResetContent contentTypes = Verb 'DELETE 205 contentTypes NoContent
-- | 'PATCH' with 205 status code.
type PatchResetContent contentTypes = Verb 'PATCH 205 contentTypes NoContent
-- | 'PUT' with 205 status code.
type PutResetContent contentTypes = Verb 'PUT 205 contentTypes NoContent


-- ** 206 Partial Content
--
-- Indicates that the server is delivering part of the resource due to a range
-- header in the request.
--
-- For more information, see <https://tools.ietf.org/html/rfc7233#section-4.1
-- RFC7233 Section 4.1>

-- | 'GET' with 206 status code.
type GetPartialContent contentTypes = Verb 'GET 205 contentTypes NoContent

data NoContent = NoContent

class ReflectMethod a where
    reflectMethod :: proxy a -> Method

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
