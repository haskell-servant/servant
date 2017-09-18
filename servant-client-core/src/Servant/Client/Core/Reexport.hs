-- | This module is a utility for @servant-client-core@ backend writers. It
-- contains all the functionality from @servant-client-core@ that should be
-- re-exported.
module Servant.Client.Core.Reexport
  (
    -- * HasClient
    HasClient(..)
    -- * Response (for @Raw@)
  , Response(..)

  -- * Generic Client
  , ClientLike(..)
  , genericMkClientL
  , genericMkClientP
  , ServantError(..)
  , EmptyClient(..)

  -- * BaseUrl
  , BaseUrl(..)
  , Scheme(..)
  , showBaseUrl
  , parseBaseUrl
  , InvalidBaseUrlException
  ) where


import           Servant.Client.Core.Internal.BaseUrl
import           Servant.Client.Core.Internal.HasClient
import           Servant.Client.Core.Internal.Generic
import           Servant.Client.Core.Internal.Request
