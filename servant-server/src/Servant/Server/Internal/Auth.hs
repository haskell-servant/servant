{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Servant.Server.Internal.Auth where

import           Control.Monad          (guard)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString        as BS
import           Data.ByteString.Base64 (decodeLenient)
import           Data.Monoid            ((<>))
import           Data.Typeable          (Typeable)
import           Data.Word8             (isSpace, toLower, _colon)
import           GHC.Generics
import           Network.HTTP.Types     (Header)
import           Network.Wai            (Request, requestHeaders)

import Servant.Server.Internal.RoutingApplication
import Servant.Server.Internal.ServantErr

-- * General Auth

-- | Specify the type of data returned after we've authenticated a request.
-- quite often this is some `User` datatype.
type family AuthReturnType a :: *

-- | Handlers for AuthProtected resources
newtype AuthHandler r usr = AuthHandler
  { unAuthHandler :: r -> ExceptT ServantErr IO usr }
  deriving (Generic, Typeable)

mkAuthHandler :: (r -> ExceptT ServantErr IO usr) -> AuthHandler r usr
mkAuthHandler = AuthHandler

-- | The result of authentication/authorization
data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr
  deriving (Eq, Show, Read, Generic, Typeable, Functor)

-- * Basic Auth

newtype BasicAuthCheck usr = BasicAuthCheck
  { unBasicAuthCheck :: BS.ByteString  -- Username
                     -> BS.ByteString  -- Password
                     -> IO (BasicAuthResult usr)
  }
  deriving (Generic, Typeable, Functor)

mkBAChallengerHdr :: BS.ByteString -> Header
mkBAChallengerHdr realm = ("WWW-Authenticate", "Basic realm=\"" <> realm <> "\"")

-- | Find and decode an 'Authorization' header from the request as Basic Auth
decodeBAHdr :: Request -> Maybe (BS.ByteString, BS.ByteString)
decodeBAHdr req = do
    ah <- lookup "Authorization" $ requestHeaders req
    let (b, rest) = BS.break isSpace ah
    guard (BS.map toLower b == "basic")
    let decoded = decodeLenient (BS.dropWhile isSpace rest)
    let (username, passWithColonAtHead) = BS.break (== _colon) decoded
    (_, password) <- BS.uncons passWithColonAtHead
    return (username, password)

runBasicAuth :: Request -> BS.ByteString -> BasicAuthCheck usr -> IO (RouteResult usr)
runBasicAuth req realm (BasicAuthCheck ba) =
  case decodeBAHdr req of
     Nothing -> plzAuthenticate
     Just e  -> uncurry ba e >>= \res -> case res of
       BadPassword    -> plzAuthenticate
       NoSuchUser     -> plzAuthenticate
       Unauthorized   -> return $ Fail err403
       Authorized usr -> return $ Route usr
  where plzAuthenticate = return $ Fail err401 { errHeaders = [mkBAChallengerHdr realm] }
