{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Servant.API.BasicAuth where

import           Data.ByteString (ByteString)
import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol)


-- | Combinator for <https://tools.ietf.org/html/rfc2617#section-2 Basic Access Authentication>.
--
-- *IMPORTANT*: Only use Basic Auth over HTTPS! Credentials are not hashed or
-- encrypted. Note also that because the same credentials are sent on every
-- request, Basic Auth is not as secure as some alternatives. Further, the
-- implementation in servant-server does not protect against some types of
-- timing attacks.
--
-- In Basic Auth, username and password are base64-encoded and transmitted via
-- the @Authorization@ header. Handshakes are not required, making it
-- relatively efficient.
data BasicAuth (realm :: Symbol) (userData :: *)
  deriving (Typeable)

-- | A simple datatype to hold data required to decorate a request
data BasicAuthData = BasicAuthData { basicAuthUsername :: !ByteString
                                   , basicAuthPassword :: !ByteString
                                   }
