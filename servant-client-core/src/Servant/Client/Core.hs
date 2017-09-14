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
  , ClientLike(..)
  , genericMkClientL
  , genericMkClientP
  , ServantError(..)
  , EmptyClient(..)


  -- * Response
  , Response(..)
  , RunClient(..)
  , module Servant.Client.Core.Internal.BaseUrl

  -- * Writing HasClient instances
  -- | These functions need not be re-exported by backend libraries.
  , addHeader
  , appendToQueryString
  , appendToPath
  , setRequestBodyLBS
  , setRequestBody
  ) where
import           Servant.Client.Core.Internal.Auth
import           Servant.Client.Core.Internal.BaseUrl   (BaseUrl (..),
                                                         InvalidBaseUrlException,
                                                         Scheme (..),
                                                         parseBaseUrl,
                                                         showBaseUrl)
import           Servant.Client.Core.Internal.BasicAuth
import           Servant.Client.Core.Internal.HasClient
import           Servant.Client.Core.Internal.Generic
import           Servant.Client.Core.Internal.Request
import           Servant.Client.Core.Internal.RunClient
