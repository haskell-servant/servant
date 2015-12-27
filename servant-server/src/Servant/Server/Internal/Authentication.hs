{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}

module Servant.Server.Internal.Authentication
( addAuthCheck
, addAuthCheckSS
, addAuthCheckSL
, addAuthCheckLS
, addAuthCheckLL
, AuthData (..)
, authProtect
, authProtectSimple
, basicAuthLax
, basicAuthStrict
, jwtAuthStrict
, SimpleAuthProtected
        ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
#endif
import           Control.Monad              (guard)
import qualified Data.ByteString            as B
import           Data.ByteString.Base64     (decodeLenient)
import           Data.Monoid                ((<>))
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
                                             AuthProtectedSimple (..),
                                             BasicAuth (BasicAuth),
                                             JWTAuth(..),
                                             OnMissing (..),
                                             OnUnauthenticated (..),
                                             SAuthPolicy(..))
import Servant.Server.Internal.RoutingApplication
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
            -> AuthProtected IO ServantErr missingPolicy missingError unauthPolicy unauthError authData usr
authProtect = AuthProtected

-- | combinator to create authentication protected servers.
authProtectSimple :: (Request -> IO (Either ServantErr u))
                  -> AuthProtectedSimple Request ServantErr u
authProtectSimple = AuthProtectedSimple

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
basicAuthStrict :: forall realm usr. KnownSymbol realm
                => (BasicAuth realm -> IO (Maybe usr))
                -> AuthProtected IO ServantErr 'Strict () 'Strict () (BasicAuth realm) usr
basicAuthStrict check =
    let mHandler = basicMissingHandler (Proxy :: Proxy realm)
        unauthHandler = basicUnauthenticatedHandler (Proxy :: Proxy realm)
        check' = \auth -> maybe (Left ()) Right <$> check auth
    in AuthProtected mHandler unauthHandler check'

-- | Basic authentication combinator with lax failure.
basicAuthLax :: KnownSymbol realm
             => (BasicAuth realm -> IO (Maybe usr))
             -> AuthProtected IO ServantErr 'Lax () 'Lax () (BasicAuth realm) usr
basicAuthLax check =
    let check' = \a -> maybe (Left ()) Right <$> check a
    in AuthProtected LaxMissing LaxUnauthenticated check'

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
              -> AuthProtected IO ServantErr 'Strict () 'Strict () JWTAuth (JWT VerifiedJWT)
jwtAuthStrict secret =
    let missingHandler = StrictMissing (const $ return (jwtWithError "invalid_request"))
        unauthHandler = StrictUnauthenticated (const . const (return $ jwtWithError "invalid_token"))
        check = return . maybe (Left ()) Right . decodeAndVerifySignature secret . unJWTAuth
    in AuthProtected missingHandler unauthHandler check

-- | A type alias to make simple authentication endpoints
type SimpleAuthProtected mPolicy uPolicy authData usr =
    AuthProtected IO ServantErr mPolicy () uPolicy () authData usr

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | helper type family to capture server handled values for various policies
type family AuthDelayedReturn (mP :: AuthPolicy) mE (uP :: AuthPolicy) uE usr :: * where
    AuthDelayedReturn 'Strict mE 'Strict uE usr = usr
    AuthDelayedReturn 'Strict mE 'Lax    uE usr = Either uE usr
    AuthDelayedReturn 'Lax    mE 'Strict uE usr = Either mE usr
    AuthDelayedReturn 'Lax    mE 'Lax    uE usr = Either (Either mE uE) usr

-- | Internal method to generate auth checkers for various policies. Scary type signature
-- but it does help with understanding the logic of how each policy works. See
-- examples below.

genAuthCheck :: (OnMissing IO ServantErr mP mE -> mE -> IO (RouteResult (AuthDelayedReturn mP mE uP uE usr)))
             -> (OnUnauthenticated IO ServantErr uP uE auth -> uE -> auth -> IO (RouteResult (AuthDelayedReturn mP mE uP uE usr)))
             -> (usr -> (AuthDelayedReturn mP mE uP uE usr))
             -> AuthProtected IO ServantErr mP mE uP uE auth usr
             -> Delayed (AuthDelayedReturn mP mE uP uE usr -> a)
             -> IO (RouteResult (Either mE auth))
             -> Delayed a
genAuthCheck missingHandler unauthHandler returnHandler authProtection d new =
    let newAuth =
            new `bindRouteResults` \ eAuthData ->
            case eAuthData of
                -- we failed to extract authentication data from the request
                Left mError -> missingHandler (onMissing authProtection) mError
                -- auth data was succesfully extracted from the request
                Right aData -> do
                    eUsr <- checkAuth authProtection aData
                    case eUsr of
                        -- we failed to authenticate the user
                        Left uError -> unauthHandler (onUnauthenticated authProtection) uError aData
                        -- user was authenticated
                        Right usr ->
                            (return . Route . returnHandler) usr
    in addAuth d newAuth

-- | Delayed auth checker for Strict Missing and Strict Unauthentication
addAuthCheckSS :: AuthProtected IO ServantErr 'Strict mError 'Strict uError auth usr
               -> Delayed (usr -> a)
               -> IO (RouteResult (Either mError auth))
               -> Delayed a
addAuthCheckSS = genAuthCheck (\(StrictMissing handler) e -> FailFatal <$> handler e)
                              (\(StrictUnauthenticated handler) e a -> FailFatal <$> handler e a)
                              id

-- | Delayed auth checker for Strict Missing and Lax Unauthentication
addAuthCheckSL :: AuthProtected IO ServantErr 'Strict mError 'Lax uError auth usr
               -> Delayed (Either uError usr -> a)
               -> IO (RouteResult (Either mError auth))
               -> Delayed a
addAuthCheckSL = genAuthCheck (\(StrictMissing handler) e -> FailFatal <$> handler e)
                              (\(LaxUnauthenticated) e _ -> (return . Route . Left) e)
                              Right

-- | Delayed auth checker for Lax Missing and Strict Unauthentication
addAuthCheckLS :: AuthProtected IO ServantErr 'Lax mError 'Strict uError auth usr
               -> Delayed (Either mError usr -> a)
               -> IO (RouteResult (Either mError auth))
               -> Delayed a
addAuthCheckLS = genAuthCheck (\(LaxMissing) e -> (return . Route . Left) e)
                              (\(StrictUnauthenticated handler) e a -> FailFatal <$> handler e a)
                              Right

-- | Delayed auth checker for Lax Missing and Lax Unauthentication
addAuthCheckLL :: AuthProtected IO ServantErr 'Lax mError 'Lax uError auth usr
               -> Delayed (Either (Either mError uError) usr -> a)
               -> IO (RouteResult (Either mError auth))
               -> Delayed a
addAuthCheckLL = genAuthCheck (\(LaxMissing) e -> (return . Route . Left . Left) e)
                              (\(LaxUnauthenticated) e _ -> (return . Route . Left . Right) e)
                              Right

-- | Add an auth check by supplying OnMissing policies and OnUnauthenticated policies.
addAuthCheck :: SAuthPolicy mPolicy
             -> SAuthPolicy uPolicy
             -> AuthProtected IO ServantErr mPolicy mError uPolicy uError auth usr
             -> Delayed (AuthDelayedReturn mPolicy mError uPolicy uError usr -> a)
             -> IO (RouteResult (Either mError auth))
             -> Delayed a
addAuthCheck SStrict SStrict = addAuthCheckSS
addAuthCheck SStrict SLax    = addAuthCheckSL
addAuthCheck SLax    SStrict = addAuthCheckLS
addAuthCheck SLax    SLax    = addAuthCheckLL
