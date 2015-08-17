{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.IsSecure
  ( -- $issecure
    IsSecure(..)
  ) where

import           Data.Typeable

-- | Was this request made over an SSL connection?
--
-- Note that this value will not tell you if the client originally
-- made this request over SSL, but rather whether the current
-- connection is SSL. The distinction lies with reverse proxies.
-- In many cases, the client will connect to a load balancer over SSL,
-- but connect to the WAI handler without SSL. In such a case,
-- the handlers would get 'NotSecure', but from a user perspective,
-- there is a secure connection.
data IsSecure = Secure    -- ^ the connection to the server
                          --   is secure (HTTPS)
              | NotSecure -- ^ the connection to the server
                          --   is not secure (HTTP)
  deriving Typeable

-- $issecure
--
-- | Use 'IsSecure' whenever your request handlers need to know whether
--   the connection to the server is secure or not.
--   This would make the request handlers receive an argument of type 'IsSecure',
--   whose value can be one of 'Secure' (HTTPS) or 'NotSecure' (HTTP).
--
-- Example:
--
-- >>> type API = "sensitive-data" :> IsSecure :> Get '[JSON] NationSecrets

-- $setup
-- >>> import Servant.API
-- >>> data NationSecrets
