{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Servant.Auth where

import           Data.Proxy          (Proxy(..))
import           Servant.API         ((:>))
import           Servant.Links       (HasLink (..))

-- * Authentication

-- | @Auth [auth1, auth2] val :> api@ represents an API protected *either* by
-- @auth1@ or @auth2@
data Auth (auths :: [*]) val

-- | A @HasLink@ instance for @Auth@
instance HasLink sub => HasLink (Auth (tag :: [*]) value :> sub) where
#if MIN_VERSION_servant(0,14,0)
  type MkLink (Auth (tag :: [*]) value :> sub) a = MkLink sub a
  toLink toA _ = toLink toA (Proxy :: Proxy sub)
#else
  type MkLink (Auth (tag :: [*]) value :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)
#endif

-- ** Combinators

-- | A JSON Web Token (JWT) in the the Authorization header:
--
--    @Authorization: Bearer \<token\>@
--
-- Note that while the token is signed, it is not encrypted. Therefore do not
-- keep in it any information you would not like the client to know.
--
-- JWTs are described in IETF's <https://tools.ietf.org/html/rfc7519 RFC 7519>
data JWT

-- | A cookie. The content cookie itself is a JWT. Another cookie is also used,
-- the contents of which are expected to be send back to the server in a
-- header, for XSRF protection.
data Cookie


-- We could use 'servant''s BasicAuth, but then we don't get control over the
-- documentation, and we'd have to polykind everything. (Also, we don't
-- currently depend on servant!)
--
-- | Basic Auth.
data BasicAuth

-- | Login via a form.
data FormLogin form
