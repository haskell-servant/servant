{-# LANGUAGE CPP #-}
module Servant.Auth.Server.Internal.BasicAuth where

#if !MIN_VERSION_servant_server(0,16,0)
#define ServerError ServantErr
#endif

import qualified Data.ByteString                   as BS
import           Servant                           (BasicAuthData (..),
                                                    ServerError (..), err401)
import           Servant.Server.Internal.BasicAuth (decodeBAHdr,
                                                    mkBAChallengerHdr)

import Servant.Auth.Server.Internal.Types

-- | A 'ServerError' that asks the client to authenticate via Basic
-- Authentication, should be invoked by an application whenever
-- appropriate. The argument is the realm.
wwwAuthenticatedErr :: BS.ByteString -> ServerError
wwwAuthenticatedErr realm = err401 { errHeaders = [mkBAChallengerHdr realm] }

-- | A type holding the configuration for Basic Authentication. 
-- It is defined as a type family with no arguments, so that
-- it can be instantiated to whatever type you need to
-- authenticate your users (use @type instance BasicAuthCfg = ...@).
-- 
-- Note that the instantiation is application-wide,
-- i.e. there can be only one instance.
-- As a consequence, it should not be instantiated in a library.
-- 
-- Basic Authentication expects an element of type 'BasicAuthCfg'
-- to be in the 'Context'; that element is then passed automatically
-- to the instance of 'FromBasicAuthData' together with the
-- authentication data obtained from the client.
-- 
-- If you do not need a configuration for Basic Authentication,
-- you can use just @BasicAuthCfg = ()@, and recall to also
-- add @()@ to the 'Context'.
-- A basic but more interesting example is to take as 'BasicAuthCfg' 
-- a list of authorised username/password pairs:
-- 
-- > deriving instance Eq BasicAuthData
-- > type instance BasicAuthCfg = [BasicAuthData]
-- > instance FromBasicAuthData User where
-- >   fromBasicAuthData authData authCfg =
-- >     if elem authData authCfg then ...
type family BasicAuthCfg

class FromBasicAuthData a where
  -- | Whether the username exists and the password is correct.
  -- Note that, rather than passing a 'Pass' to the function, we pass a
  -- function that checks an 'EncryptedPass'. This is to make sure you don't
  -- accidentally do something untoward with the password, like store it.
  fromBasicAuthData :: BasicAuthData -> BasicAuthCfg -> IO (AuthResult a)

basicAuthCheck :: FromBasicAuthData usr => BasicAuthCfg -> AuthCheck usr
basicAuthCheck cfg = AuthCheck $ \req -> case decodeBAHdr req of
  Nothing -> return Indefinite
  Just baData -> fromBasicAuthData baData cfg
