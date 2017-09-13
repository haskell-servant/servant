-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client
  ( client
  , ClientM
  , runClientM
  , ClientEnv(..)
  , module X
  ) where

import Servant.Client.Internal.HttpClient
import Servant.Client.Core.Reexport as X
