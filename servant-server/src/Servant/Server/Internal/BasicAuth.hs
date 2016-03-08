{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Servant.Server.Internal.BasicAuth where

import           Control.Monad          (guard)
import qualified Data.ByteString        as BS
import           Data.ByteString.Base64 (decodeLenient)
import           Data.Monoid            ((<>))
import           Data.Typeable          (Typeable)
import           Data.Word8             (isSpace, toLower, _colon)
import           GHC.Generics
import           Network.HTTP.Types     (Header)
import           Network.Wai            (Request, requestHeaders)

import Servant.API.BasicAuth (BasicAuthData(BasicAuthData))
import Servant.Server.Internal.RoutingApplication
import Servant.Server.Internal.ServantErr

-- * Basic Auth

-- | servant-server's current implementation of basic authentication is not
-- immune to certian kinds of timing attacks. Decoding payloads does not take
-- a fixed amount of time.

-- | The result of authentication/authorization
data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr
  deriving (Eq, Show, Read, Generic, Typeable, Functor)

-- | Datatype wrapping a function used to check authentication.
newtype BasicAuthCheck usr = BasicAuthCheck
  { unBasicAuthCheck :: BasicAuthData
                     -> IO (BasicAuthResult usr)
  }
  deriving (Generic, Typeable, Functor)

-- | Internal method to make a basic-auth challenge
mkBAChallengerHdr :: BS.ByteString -> Header
mkBAChallengerHdr realm = ("WWW-Authenticate", "Basic realm=\"" <> realm <> "\"")

-- | Find and decode an 'Authorization' header from the request as Basic Auth
decodeBAHdr :: Request -> Maybe BasicAuthData
decodeBAHdr req = do
    ah <- lookup "Authorization" $ requestHeaders req
    let (b, rest) = BS.break isSpace ah
    guard (BS.map toLower b == "basic")
    let decoded = decodeLenient (BS.dropWhile isSpace rest)
    let (username, passWithColonAtHead) = BS.break (== _colon) decoded
    (_, password) <- BS.uncons passWithColonAtHead
    return (BasicAuthData username password)

-- | Run and check basic authentication, returning the appropriate http error per
-- the spec.
runBasicAuth :: Request -> BS.ByteString -> BasicAuthCheck usr -> IO (RouteResult usr)
runBasicAuth req realm (BasicAuthCheck ba) =
  case decodeBAHdr req of
     Nothing -> plzAuthenticate
     Just e  -> ba e >>= \res -> case res of
       BadPassword    -> plzAuthenticate
       NoSuchUser     -> plzAuthenticate
       Unauthorized   -> return $ Fail err403
       Authorized usr -> return $ Route usr
  where plzAuthenticate = return $ Fail err401 { errHeaders = [mkBAChallengerHdr realm] }
