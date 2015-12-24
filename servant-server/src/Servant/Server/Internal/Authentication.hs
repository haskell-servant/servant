{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}

module Servant.Server.Internal.Authentication
( AuthData (..)
, authProtect
, basicAuthLax
, basicAuthStrict
, jwtAuthStrict
, SimpleAuthProtected
        ) where

import           Control.Monad              (guard)
import qualified Data.ByteString            as B
import           Data.ByteString.Base64     (decodeLenient)
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid                ((<>), mempty)
#else
import           Data.Monoid                ((<>))
#endif
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.Word8                 (isSpace, toLower, _colon)
import           GHC.TypeLits               (KnownSymbol, symbolVal)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Text                  (splitOn)
import           Network.Wai                (Request, requestHeaders)
import Servant.Server.Internal.ServantErr (err401, ServantErr(errHeaders))
import           Servant.API.Authentication (AuthPolicy (Strict, Lax),
                                             AuthProtected(..),
                                             BasicAuth (BasicAuth),
                                             JWTAuth(..),
                                             OnMissing (..),
                                             OnUnauthenticated (..))
import            Web.JWT                    (decodeAndVerifySignature, JWT, VerifiedJWT, Secret)
import qualified  Web.JWT as JWT             (decode)

-- | Class to represent the ability to extract authentication-related
-- data from a 'Request' object.
class AuthData a e | a -> e where
    authData :: Request -> Either e a

-- | combinator to create authentication protected servers.
authProtect :: OnMissing IO ServantErr missingPolicy missingError
            -> OnUnauthenticated IO ServantErr unauthPolicy unauthError authData
            -> (authData -> IO (Either unauthError usr))
            -> subserver
            -> AuthProtected IO ServantErr missingPolicy missingError unauthPolicy unauthError authData usr subserver
authProtect = AuthProtected

-- | 'BasicAuth' instance for authData
instance AuthData (BasicAuth realm) () where
    authData request = maybe (Left ()) Right $ do
        authBs <- lookup "Authorization" (requestHeaders request)
        let (x,y) = B.break isSpace authBs
        guard (B.map toLower x == "basic")
        -- decode the base64-encoded username and password
        let (username, passWithColonAtHead) = B.break (== _colon) (decodeLenient (B.dropWhile isSpace y))
        (_, password) <- B.uncons passWithColonAtHead
        return $ BasicAuth username password

-- | failure response for Basic Authentication
basicAuthFailure :: forall realm. KnownSymbol realm
                 => Proxy realm
                 -> ServantErr
basicAuthFailure p = let realmBytes = (fromString . symbolVal) p
                         headerBytes = "Basic realm=\"" <> realmBytes <> "\""
                     in err401 { errHeaders = [("WWW-Authenticate", headerBytes)] }

-- | OnMisisng handler for Basic Authentication
basicMissingHandler :: forall realm. KnownSymbol realm
                    => Proxy realm
                    ->  OnMissing IO ServantErr 'Strict ()
basicMissingHandler p = StrictMissing (const $ return (basicAuthFailure p))

-- | OnUnauthenticated handler for Basic Authentication
basicUnauthenticatedHandler :: forall realm. KnownSymbol realm
                            => Proxy realm
                            -> OnUnauthenticated IO ServantErr 'Strict () (BasicAuth realm)
basicUnauthenticatedHandler p = StrictUnauthenticated (const . const (return $ basicAuthFailure p))

-- | Basic authentication combinator with strict failure.
basicAuthStrict :: forall realm usr subserver. KnownSymbol realm
                => (BasicAuth realm -> IO (Maybe usr))
                -> subserver
                -> AuthProtected IO ServantErr 'Strict () 'Strict () (BasicAuth realm) usr subserver
basicAuthStrict check sub =
    let mHandler = basicMissingHandler (Proxy :: Proxy realm)
        unauthHandler = basicUnauthenticatedHandler (Proxy :: Proxy realm)
        check' = \auth -> maybe (Left ()) Right <$> check auth
    in AuthProtected mHandler unauthHandler check' sub

-- | Basic authentication combinator with lax failure.
basicAuthLax :: KnownSymbol realm
             => (BasicAuth realm -> IO (Maybe usr))
             -> subserver
             -> AuthProtected IO ServantErr 'Lax () 'Lax () (BasicAuth realm) usr subserver
basicAuthLax check sub =
    let check' = \a -> maybe (Left ()) Right <$> check a
    in AuthProtected LaxMissing LaxUnauthenticated check' sub

-- | Authentication data we extract from requests for JWT-based authentication.
instance AuthData JWTAuth () where
  authData req = maybe (Left ()) Right $ do
    hdr <- lookup "Authorization" . requestHeaders $ req
    ["Bearer", token] <- return . splitOn " " . decodeUtf8 $ hdr
    _ <- JWT.decode token -- try decode it. otherwise it's not a proper token
    return . JWTAuth $ token

-- | helper method to construct jwt handlers
jwtWithError :: B.ByteString -> ServantErr
jwtWithError e = err401 { errHeaders = [("WWW-Authenticate", "Bearer error=\""<>e<>"\"")] }

-- | OnMissing handler for Strict, JWT-based authentication
jwtAuthStrict :: Secret
              -> subserver
              -> AuthProtected IO ServantErr 'Strict () 'Strict () JWTAuth (JWT VerifiedJWT) subserver
jwtAuthStrict secret sub =
    let missingHandler = StrictMissing (const $ return (jwtWithError "invalid_request"))
        unauthHandler = StrictUnauthenticated (const . const (return $ jwtWithError "invalid_token"))
        check = return . maybe (Left ()) Right . decodeAndVerifySignature secret . unJWTAuth
    in AuthProtected missingHandler unauthHandler check sub

-- | A type alias to make simple authentication endpoints
type SimpleAuthProtected mPolicy uPolicy authData usr subserver =
    AuthProtected IO ServantErr mPolicy () uPolicy () authData usr subserver
