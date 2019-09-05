-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
--
-- This client supports streaming operations.
module Servant.Client.Streaming
    ( client
    , ClientM
    , withClientM
    , runClientM
    , ClientEnv(..)
    , mkClientEnv
    , defaultMakeClientRequest
    , hoistClient
    , module Servant.Client.Core.Reexport
    ) where

import           Servant.Client.Core.Reexport
import           Servant.Client.Internal.HttpClient.Streaming
