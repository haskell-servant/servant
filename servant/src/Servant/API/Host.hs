module Servant.API.Host (Host) where

import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)

-- | Match against the given host.
--
--   This allows you to define APIs over multiple domains. For example:
--
-- > type API = Host "api1.example" :> API1
-- >       :<|> Host "api2.example" :> API2
--
data Host (sym :: Symbol) deriving Typeable
