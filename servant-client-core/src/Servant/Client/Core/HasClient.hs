-- | Wrapper for Servant.Client.Core.HasClient.Internal, which brings in scope the
-- instance declarations in Servant.Client.Core.HasClient.TypeErrors
module Servant.Client.Core.HasClient
  ( module Servant.Client.Core.HasClient.Internal
  ) where

import           Servant.Client.Core.HasClient.Internal
import           Servant.Client.Core.HasClient.TypeErrors ()
