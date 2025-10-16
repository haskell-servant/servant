{-# LANGUAGE CPP #-}

module Servant.Auth.Server.Internal.JWT where

import Control.Lens
import Control.Monad (MonadPlus (..), guard)
import Control.Monad.Reader
import qualified Crypto.JOSE as Jose
import qualified Crypto.JWT as Jose
import Data.ByteArray (constEq)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Network.Wai (requestHeaders)
import Servant.Auth.JWT (FromJWT (..), ToJWT (..))

import Servant.Auth.Server.Internal.ConfigTypes
import Servant.Auth.Server.Internal.Types

-- | A JWT @AuthCheck@. You likely won't need to use this directly unless you
-- are protecting a @Raw@ endpoint.
jwtAuthCheck :: FromJWT usr => JWTSettings -> AuthCheck usr
jwtAuthCheck jwtSettings = do
  req <- ask
  token <- maybe mempty pure $ do
    authHdr <- lookup "Authorization" $ requestHeaders req
    let bearer = "Bearer "
        (mbearer, rest) = BS.splitAt (BS.length bearer) authHdr
    guard (mbearer `constEq` bearer)
    pure rest
  verifiedJWT <- liftIO $ verifyJWT jwtSettings token
  maybe mzero pure verifiedJWT

{- FOURMOLU_DISABLE -}
-- | Creates a JWT containing the specified data. The data is stored in the
-- @dat@ claim. The 'Maybe UTCTime' argument indicates the time at which the
-- token expires.
makeJWT
  :: ToJWT a
  => a
  -> JWTSettings
  -> Maybe UTCTime
  -> IO (Either Jose.Error BSL.ByteString)
makeJWT v cfg expiry = Jose.runJOSE $ do
  bestAlg <- Jose.bestJWSAlg $ signingKey cfg
  let alg = fromMaybe bestAlg $ jwtAlg cfg
  ejwt <-
    Jose.signClaims
      (signingKey cfg)
#if MIN_VERSION_jose(0,12,0)
      (Jose.newJWSHeaderProtected alg)
#else
      (Jose.newJWSHeader ((), alg))
#endif
      (addExp $ encodeJWT v)

  pure $ Jose.encodeCompact ejwt
  where
    addExp claims = case expiry of
      Nothing -> claims
      Just e -> claims & Jose.claimExp ?~ Jose.NumericDate e

verifyJWT :: FromJWT a => JWTSettings -> BS.ByteString -> IO (Maybe a)
verifyJWT jwtCfg input = do
  keys <- validationKeys jwtCfg
  verifiedJWT <- Jose.runJOSE $ do
#if MIN_VERSION_jose(0,12,0)
    unverifiedJWT :: Jose.SignedJWTWithHeader Jose.JWSHeader <-
#else
    unverifiedJWT :: Jose.SignedJWT <-
#endif
      Jose.decodeCompact (BSL.fromStrict input)
    Jose.verifyClaims
      (jwtSettingsToJwtValidationSettings jwtCfg)
      keys
      unverifiedJWT
  pure $ case verifiedJWT of
    Left (_ :: Jose.JWTError) -> Nothing
    Right v -> case decodeJWT v of
      Left _ -> Nothing
      Right v' -> Just v'
