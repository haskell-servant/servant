

-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client
  ( client
  , ClientM
  , runClientM
  , ClientEnv(..)
  , mkClientEnv
  , defaultMakeClientRequest
  , hoistClient
  , module Servant.Client.Core.Reexport
  ) where

import           Servant.Client.Core.Reexport
import           Servant.Client.Internal.HttpClient
