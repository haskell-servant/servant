-- | Wrapper for Servant.Links.Internal, which brings in scope the instance declarations
-- in Servant.Links.TypeErrors
module Servant.Links
  ( module Servant.Links.Internal
  ) where

import           Servant.Links.Internal
import           Servant.Links.TypeErrors ()
