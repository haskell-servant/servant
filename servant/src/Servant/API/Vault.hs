module Servant.API.Vault
  ( -- $vault
    Vault
  ) where

import           Data.Vault.Lazy (Vault)

-- $vault
--
-- | Use 'Vault' in your API types to provide access to the 'Vault'
--   of the request, which is a location shared by middlewares and applications
--   to store arbitrary data. See 'Vault' for more details on how to actually
--   use the vault in your handlers
--
-- Example:
--
-- >>> type API = Vault :> Get '[JSON] String

-- $setup
-- >>> import Servant.API
