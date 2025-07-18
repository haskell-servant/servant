-- | This module is a utility for @servant-client-core@ backend writers. It
-- contains all the functionality from @servant-client-core@ that should be
-- re-exported.
module Servant.Client.Core.Reexport
  ( -- * HasClient
    HasClient (..)
  , foldMapUnion
  , matchUnion
  , AsClientT
  , (//)
  , (/:)

    -- * Response (for @Raw@)
  , Response
  , StreamingResponse
  , ResponseF (..)

    -- * Data types
  , ClientError (..)
  , EmptyClient (..)

    -- * BaseUrl
  , BaseUrl (..)
  , Scheme (..)
  , showBaseUrl
  , parseBaseUrl
  , InvalidBaseUrlException

    -- * Authentication
  , AuthClientData
  , AuthenticatedRequest (..)
  , mkAuthenticatedRequest
  )
where

import Servant.Client.Core.Auth
import Servant.Client.Core.BaseUrl
import Servant.Client.Core.ClientError
import Servant.Client.Core.HasClient
import Servant.Client.Core.Response
