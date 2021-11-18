{-# LANGUAGE CPP #-}

module Servant.Auth.JWT where

import           Control.Lens         ((^.))
import qualified Crypto.JWT           as Jose
import           Data.Aeson           (FromJSON, Result (..), ToJSON, fromJSON,
                                       toJSON)
#if MIN_VERSION_aeson(2,0,0)                                                                                    
import qualified Data.Map as KM                                                                        
#else                                                                                                           
import qualified Data.HashMap.Strict as KM                                                                      
#endif

import qualified Data.Text            as T


-- This should probably also be from ClaimSet
--
-- | How to decode data from a JWT.
--
-- The default implementation assumes the data is stored in the unregistered
-- @dat@ claim, and uses the @FromJSON@ instance to decode value from there.
class FromJWT a where
  decodeJWT :: Jose.ClaimsSet -> Either T.Text a
  default decodeJWT :: FromJSON a => Jose.ClaimsSet -> Either T.Text a
  decodeJWT m = case KM.lookup "dat" (m ^. Jose.unregisteredClaims) of
    Nothing -> Left "Missing 'dat' claim"
    Just v  -> case fromJSON v of
      Error e -> Left $ T.pack e
      Success a -> Right a

-- | How to encode data from a JWT.
--
-- The default implementation stores data in the unregistered @dat@ claim, and
-- uses the type's @ToJSON@ instance to encode the data.
class ToJWT a where
  encodeJWT :: a -> Jose.ClaimsSet
  default encodeJWT :: ToJSON a => a -> Jose.ClaimsSet
  encodeJWT a = Jose.addClaim "dat" (toJSON a) Jose.emptyClaimsSet