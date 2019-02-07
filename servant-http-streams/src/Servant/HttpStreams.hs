

-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.HttpStreams
  ( client
  , ClientM
  , withClientM
  , runClientM
  , ClientEnv(..)
  , mkClientEnv
  , withClientEnvIO
  , hoistClient
  , module Servant.Client.Core.Reexport
  ) where

import           Servant.Client.Core.Reexport
import           Servant.HttpStreams.Internal
