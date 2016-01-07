{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Auth where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)


-- | Combinator for <https://tools.ietf.org/html/rfc2617#section-2 Basic Access Authentication>.
--
-- *IMPORTANT*: Only use Basic Auth over HTTPS! Credentials are not hashed or
-- encrypted. Note also that because the same credentials are sent on every
-- request, Basic Auth is not as secure as some alternatives.
--
-- In Basic Auth, username and password are base64-encoded and transmitted via
-- the @Authorization@ header. Handshakes are not required, making it
-- relatively efficient.
data BasicAuth (realm :: Symbol)
  deriving (Typeable)

-- | A generalized Authentication combinator.
data AuthProtect (tag :: k)
  deriving (Typeable)
