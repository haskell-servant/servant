module Servant.Auth.Server.Internal.JWT where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Crypto.JOSE          as Jose
import qualified Crypto.JWT           as Jose
import           Data.Aeson           (FromJSON, Result (..), ToJSON, fromJSON,
                                       toJSON)
import           Data.ByteArray       (constEq)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Data.Time            (UTCTime)
import           Network.Wai          (requestHeaders)

import Servant.Auth.JWT               (FromJWT(..), ToJWT(..))
import Servant.Auth.Server.Internal.ConfigTypes
import Servant.Auth.Server.Internal.Types


-- | A JWT @AuthCheck@. You likely won't need to use this directly unless you
-- are protecting a @Raw@ endpoint.
jwtAuthCheck :: FromJWT usr => JWTSettings -> AuthCheck usr
jwtAuthCheck jwtSettings = do
  req <- ask
  token <- maybe mempty return $ do
    authHdr <- lookup "Authorization" $ requestHeaders req
    let bearer = "Bearer "
        (mbearer, rest) = BS.splitAt (BS.length bearer) authHdr
    guard (mbearer `constEq` bearer)
    return rest
  verifiedJWT <- liftIO $ verifyJWT jwtSettings token
  case verifiedJWT of
    Nothing -> mzero
    Just v -> return v

-- | Creates a JWT containing the specified data. The data is stored in the
-- @dat@ claim. The 'Maybe UTCTime' argument indicates the time at which the
-- token expires.
makeJWT :: ToJWT a
  => a -> JWTSettings -> Maybe UTCTime -> IO (Either Jose.Error BSL.ByteString)
makeJWT v cfg expiry = runExceptT $ do
  bestAlg <- Jose.bestJWSAlg $ signingKey cfg
  let alg = fromMaybe bestAlg $ jwtAlg cfg
  ejwt <- Jose.signClaims (signingKey cfg)
                          (Jose.newJWSHeader ((), alg))
                          (addExp $ encodeJWT v)

  return $ Jose.encodeCompact ejwt
  where
   addExp claims = case expiry of
     Nothing -> claims
     Just e  -> claims & Jose.claimExp ?~ Jose.NumericDate e


verifyJWT :: FromJWT a => JWTSettings -> BS.ByteString -> IO (Maybe a)
verifyJWT jwtCfg input = do
  verifiedJWT <- liftIO $ runExceptT $ do
    unverifiedJWT <- Jose.decodeCompact (BSL.fromStrict input)
    Jose.verifyClaims
      (jwtSettingsToJwtValidationSettings jwtCfg)
      (validationKeys jwtCfg)
      unverifiedJWT
  return $ case verifiedJWT of
    Left (_ :: Jose.JWTError) -> Nothing
    Right v -> case decodeJWT v of
      Left _ -> Nothing
      Right v' -> Just v'