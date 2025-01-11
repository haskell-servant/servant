module Servant.API.Host (Host) where

import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)

-- | Match against the given host.
--
-- Example:
--
-- > type MyApi = Host "example.com" :> PostNoContent
data Host (sym :: Symbol) deriving Typeable
