-- | This module provides backend-agnostic functionality for generating clients
-- from @servant@ APIs. By "backend," we mean something that concretely
-- executes the request, such as:
--
--  * The @http-client@ library
--  * The @haxl@ library
--  * GHCJS via FFI
--
-- etc.
--
-- Each backend is encapsulated in a monad that is an instance of the
-- 'RunClient' class.
--
-- This library is primarily of interest to backend-writers and
-- combinator-writers. For more information, see the README.md
module Servant.Client.Core
  (
  -- * Client generation
    clientIn
  , HasClient(..)
  , collapseUResp
  , extractUResp

  -- * Request
  , Request
  , RequestF(..)
  , defaultRequest
  , RequestBody(..)

  -- * Authentication
  , mkAuthenticatedRequest
  , basicAuthReq
  , AuthenticatedRequest(..)
  , AuthClientData

  -- * Generic Client
  , ClientError(..)
  , EmptyClient(..)

  -- * Response
  , Response
  , ResponseF (..)
  , RunClient(..)
  -- * BaseUrl
  , BaseUrl (..)
  , Scheme (..)
  , showBaseUrl
  , parseBaseUrl
  , InvalidBaseUrlException (..)

  -- ** Streaming
  , RunStreamingClient(..)
  , StreamingResponse

  -- * Writing HasClient instances
  -- | These functions need not be re-exported by backend libraries.
  , addHeader
  , appendToQueryString
  , appendToPath
  , setRequestBodyLBS
  , setRequestBody
  ) where
import           Servant.Client.Core.Auth
import           Servant.Client.Core.BaseUrl
                 (BaseUrl (..), InvalidBaseUrlException (..), Scheme (..),
                 parseBaseUrl, showBaseUrl)
import           Servant.Client.Core.BasicAuth
import           Servant.Client.Core.ClientError
import           Servant.Client.Core.HasClient
import           Servant.Client.Core.Request
import           Servant.Client.Core.Response
import           Servant.Client.Core.RunClient
