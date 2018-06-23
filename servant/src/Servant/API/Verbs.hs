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
-- >>> type Post204 contentTypes a = Verb 'POST contentTypes (Result a)
data Verb (method :: k1) (contentTypes :: [*]) (api :: *)
  deriving (Typeable, Generic)

-- | A type for responses.
data Result (statusCode :: Nat) (a :: *)
  deriving (Typeable, Generic)

data VerbNoContent (method :: k1) (api :: *)
  deriving (Typeable, Generic)

-- | A type for responses without content-body.
data NoContent (statusCode :: Nat) = NoContent
  deriving (Show, Eq, Read, Generic)

-- * 200 responses
--
-- The 200 response is the workhorse of web servers, but also fairly generic.
-- When appropriate, you should prefer the more specific success combinators.
-- More information about the definitions of status codes can be found in
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html RFC2615> and
-- <https://tools.ietf.org/html/rfc7231#section-6 RFC7231 Section 6>;
-- the relevant information is summarily presented here.

-- | 'GET' with 200 status code.
type Get ct a    = Verb 'GET    ct (Result 200 a)
-- | 'POST' with 200 status code.
type Post ct a   = Verb 'POST   ct (Result 200 a)
-- | 'PUT' with 200 status code.
type Put ct a    = Verb 'PUT    ct (Result 200 a)
-- | 'DELETE' with 200 status code.
type Delete ct a = Verb 'DELETE ct (Result 200 a)
-- | 'PATCH' with 200 status code.
type Patch ct a  = Verb 'PATCH  ct (Result 200 a)

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
type PostCreated ct a = Verb 'POST ct (Result 201 a)


-- ** 202 Accepted
--
-- Indicates that the request has been accepted for processing, but the
-- processing has not yet completed. The status of the processing should be
-- included, as well as either a link to a status monitoring endpoint or an
-- estimate of when the processing will be finished.

-- | 'GET' with 202 status code.
type GetAccepted ct a    = Verb 'GET ct (Result 202 a)
-- | 'POST' with 202 status code.
type PostAccepted ct a   = Verb 'POST ct (Result 202 a)
-- | 'DELETE' with 202 status code.
type DeleteAccepted ct a = Verb 'DELETE ct (Result 202 a)
-- | 'PATCH' with 202 status code.
type PatchAccepted ct a = Verb 'PATCH ct (Result 202 a)
-- | 'PUT' with 202 status code.
type PutAccepted ct a   = Verb 'PUT ct (Result 202 a)


-- ** 203 Non-Authoritative Information
--
-- Indicates that the request has been successfully processed, but the
-- information may come from a third-party.

-- | 'GET' with 203 status code.
type GetNonAuthoritative ct a    = Verb 'GET ct (Result 203 a)
-- | 'POST' with 203 status code.
type PostNonAuthoritative ct a   = Verb 'POST ct (Result 203 a)
-- | 'DELETE' with 203 status code.
type DeleteNonAuthoritative ct a = Verb 'DELETE ct (Result 203 a)
-- | 'PATCH' with 203 status code.
type PatchNonAuthoritative ct a  = Verb 'PATCH ct (Result 203 a)
-- | 'PUT' with 203 status code.
type PutNonAuthoritative ct a    = Verb 'PUT ct (Result 203 a)


-- ** 204 No Content
--
-- Indicates that no response body is being returned. Handlers for these should
-- return 'NoContent', possibly with headers.
--
-- If the document view should be reset, use @205 Reset Content@.

-- | 'GET' with 204 status code.
type GetNoContent    = VerbNoContent 'GET (NoContent 204)
-- | 'POST' with 204 status code.
type PostNoContent   = VerbNoContent 'POST (NoContent 204)
-- | 'DELETE' with 204 status code.
type DeleteNoContent = VerbNoContent 'DELETE (NoContent 204)
-- | 'PATCH' with 204 status code.
type PatchNoContent  = VerbNoContent 'PATCH (NoContent 204)
-- | 'PUT' with 204 status code.
type PutNoContent    = VerbNoContent 'PUT (NoContent 204)


-- ** 205 Reset Content
--
-- Indicates that no response body is being returned. Handlers for these should
-- return 'NoContent', possibly with Headers.
--
-- If the document view should not be reset, use @204 No Content@.

-- | 'GET' with 205 status code.
type GetResetContent    = VerbNoContent 'GET (NoContent 205)
-- | 'POST' with 205 status code.
type PostResetContent   = VerbNoContent 'POST (NoContent 205)
-- | 'DELETE' with 205 status code.
type DeleteResetContent = VerbNoContent 'DELETE (NoContent 205)
-- | 'PATCH' with 205 status code.
type PatchResetContent  = VerbNoContent 'PATCH (NoContent 205)
-- | 'PUT' with 205 status code.
type PutResetContent    = VerbNoContent 'PUT (NoContent 205)


-- ** 206 Partial Content
--
-- Indicates that the server is delivering part of the resource due to a range
-- header in the request.
--
-- For more information, see <https://tools.ietf.org/html/rfc7233#section-4.1
-- RFC7233 Section 4.1>

-- | 'GET' with 206 status code.
type GetPartialContent ct a = Verb 'GET ct (Result 206 a)


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
