{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.RemoteHost
  ( -- $remotehost
    RemoteHost
  ) where

import           Data.Typeable

-- | Provides access to the host or IP address
--   from which the HTTP request was sent.
data RemoteHost deriving Typeable

-- $remotehost
--
-- | Use 'RemoteHost' whenever your request handlers need the host or IP address
--   from which the client issued the HTTP request. The corresponding handlers
--   receive arguments of type @SockAddr@ (from @Network.Socket@).
--
-- Example:
--
-- >>> -- POST /record-ip
-- >>> type API = "record-ip" :> RemoteHost :> Post '[] ()

-- $setup
-- >>> import Servant.API
