{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Servant.Server.Internal.Authentication
( AuthProtected (..)
, AuthData (..)
, AuthHandlers (AuthHandlers, onMissingAuthData, onUnauthenticated)
, basicAuthLax
, basicAuthStrict
, laxProtect
, strictProtect
        ) where

import           Control.Monad              (guard)
import qualified Data.ByteString            as B
import           Data.ByteString.Base64     (decodeLenient)
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.Word8                 (isSpace, toLower, _colon)
import           GHC.TypeLits               (KnownSymbol, symbolVal)
import           Network.HTTP.Types.Status  (status401)
import           Network.Wai                (Request, Response, requestHeaders,
                                             responseBuilder)
import           Servant.API.Authentication (AuthPolicy (Strict, Lax),
                                             AuthProtected,
                                             BasicAuth (BasicAuth))

-- | Class to represent the ability to extract authentication-related
-- data from a 'Request' object.
class AuthData a where
    authData :: Request -> Maybe a

-- | handlers to deal with authentication failures.
data AuthHandlers authData = AuthHandlers
    {   -- we couldn't find the right type of auth data (or any, for that matter)
        onMissingAuthData :: IO Response
    ,
        -- we found the right type of auth data in the request but the check failed
        onUnauthenticated :: authData -> IO Response
    }

-- | concrete type to provide when in 'Strict' mode.
data instance AuthProtected authData usr subserver 'Strict =
    AuthProtectedStrict { checkAuthStrict :: authData -> IO (Maybe usr)
                        , authHandlers :: AuthHandlers authData
                        , subServerStrict :: subserver
                        }

-- | concrete type to provide when in 'Lax' mode.
data instance AuthProtected authData usr subserver 'Lax =
    AuthProtectedLax { checkAuthLax :: authData -> IO (Maybe usr)
                     , subServerLax :: subserver
                     }

-- | handy function to build an auth-protected bit of API with a Lax policy
laxProtect :: (authData -> IO (Maybe usr)) -- ^ check auth
           -> subserver                    -- ^ the handlers for the auth-aware bits of the API
           -> AuthProtected authData usr subserver 'Lax
laxProtect = AuthProtectedLax

-- | handy function to build an auth-protected bit of API with a Strict policy
strictProtect :: (authData -> IO (Maybe usr)) -- ^ check auth
              -> AuthHandlers authData        -- ^ functions to call on auth failure
              -> subserver                    -- ^ handlers for the auth-protected bits of the API
              -> AuthProtected authData usr subserver 'Strict
strictProtect = AuthProtectedStrict

-- | 'BasicAuth' instance for authData
instance AuthData (BasicAuth realm) where
    authData request = do
        authBs <- lookup "Authorization" (requestHeaders request)
        let (x,y) = B.break isSpace authBs
        guard (B.map toLower x == "basic")
        -- decode the base64-encoded username and password
        let (username, passWithColonAtHead) = B.break (== _colon) (decodeLenient (B.dropWhile isSpace y))
        (_, password) <- B.uncons passWithColonAtHead
        return $ BasicAuth username password

-- | handlers for Basic Authentication.
basicAuthHandlers :: forall realm. KnownSymbol realm => AuthHandlers (BasicAuth realm)
basicAuthHandlers =
    let realmBytes = (fromString . symbolVal) (Proxy :: Proxy realm)
        headerBytes = "Basic realm=\"" <> realmBytes <> "\""
        authFailure = responseBuilder status401 [("WWW-Authenticate", headerBytes)] mempty in
        AuthHandlers (return authFailure)  ((const . return) authFailure)

-- | Basic authentication combinator with strict failure.
basicAuthStrict :: KnownSymbol realm
                => (BasicAuth realm -> IO (Maybe usr))
                -> subserver
                -> AuthProtected (BasicAuth realm) usr subserver 'Strict
basicAuthStrict check subserver = strictProtect check basicAuthHandlers subserver

-- | Basic authentication combinator with lax failure.
basicAuthLax :: KnownSymbol realm
             => (BasicAuth realm -> IO (Maybe usr))
             -> subserver
             -> AuthProtected (BasicAuth realm) usr subserver 'Lax
basicAuthLax = laxProtect
