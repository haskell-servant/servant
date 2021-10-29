{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ == 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Servant.Auth.Client.Internal where

import qualified Data.ByteString    as BS
import           Data.Monoid
import           Data.Proxy         (Proxy (..))
import           Data.String        (IsString)
import           GHC.Exts           (Constraint)
import           GHC.Generics       (Generic)
import           Servant.API        ((:>))
import           Servant.Auth

import           Servant.Client.Core
import           Data.Sequence ((<|))

-- | A simple bearer token.
newtype Token = Token { getToken :: BS.ByteString }
  deriving (Eq, Show, Read, Generic, IsString)

type family HasBearer xs :: Constraint where
  HasBearer (Bearer ': xs) = ()
  HasBearer (JWT ': xs) = ()
  HasBearer (x ': xs)   = HasBearer xs
  HasBearer '[]         = BearerAuthNotEnabled

class BearerAuthNotEnabled

-- | @'HasBearer' auths@ is nominally a redundant constraint, but ensures we're not
-- trying to send a token to an API that doesn't accept them.
instance (HasBearer auths, HasClient m api) => HasClient m (Auth auths a :> api) where
  type Client m (Auth auths a :> api) = Token -> Client m api

  clientWithRoute m _ req (Token token)
    = clientWithRoute m (Proxy :: Proxy api)
    $ req { requestHeaders = ("Authorization", headerVal) <| requestHeaders req  }
      where
        headerVal = "Bearer " <> token

#if MIN_VERSION_servant_client_core(0,14,0)
  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl
#endif


-- * Authentication combinators

-- | A Bearer token in the Authorization header:
--
--    @Authorization: Bearer <token>@
--
-- This can be any token recognized by the server, for example,
-- a JSON Web Token (JWT).
--
-- Note that, since the exact way the token is validated is not specified,
-- this combinator can only be used in the client. The server would not know
-- how to validate it, while the client does not care.
-- If you want to implement Bearer authentication in your server, you have to
-- choose a specific combinator, such as 'JWT'.
data Bearer
