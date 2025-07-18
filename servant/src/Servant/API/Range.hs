{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.API.Range (Range (unRange), unsafeRange, mkRange) where

import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import Data.Bifunctor (first)
import Data.Ix
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.TypeLits

import Servant.API

-- | A newtype wrapper around 'Natural' that ensures the value is within a given range.
--
-- Example:
--
-- >>> :{
--   let validRange = mkRange 5 :: Maybe (Range 1 10)
--   in case validRange of
--        Just r  -> "Valid range: " ++ show (unRange r)
--        Nothing -> "Invalid range"
-- :}
-- "Valid range: 5"
--
-- >>> :{
--   let invalidRange = mkRange 15 :: Maybe (Range 1 10)
--   in case invalidRange of
--        Just r  -> "Valid range: " ++ show (unRange r)
--        Nothing -> "Invalid range"
-- :}
-- "Invalid range"
--
-- >>> decode "5" :: Maybe (Range 1 10)
-- Just (MkRange {unRange = 5})
--
-- >>> decode "15" :: Maybe (Range 1 10)
-- Nothing
newtype Range (min :: Nat) (max :: Nat) = MkRange {unRange :: Natural}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Ix, ToHttpApiData, ToJSON)

unsafeRange :: Natural -> Range min max
unsafeRange = MkRange

instance (KnownNat max, KnownNat min) => Bounded (Range min max) where
  minBound = MkRange . fromInteger $ natVal (Proxy @min)
  maxBound = MkRange . fromInteger $ natVal (Proxy @max)

parseErrorMsg :: forall min max. (KnownNat max, KnownNat min) => Proxy (Range min max) -> String
parseErrorMsg _ =
  "Expecting a natural number between " <> show (natVal (Proxy @min)) <> " and " <> show (natVal (Proxy @max)) <> "."

mkRange :: forall min max. (KnownNat max, KnownNat min) => Natural -> Maybe (Range min max)
mkRange n
  | inRange (minBound :: Range min max, maxBound :: Range min max) (MkRange n) = Just (MkRange n)
  | otherwise = Nothing

instance (KnownNat max, KnownNat min) => FromJSON (Range min max) where
  parseJSON v = do
    n <- modifyFailure (const $ parseErrorMsg @min @max Proxy) $ parseJSON v
    maybe (fail $ parseErrorMsg @min @max Proxy) pure $ mkRange n

instance (KnownNat max, KnownNat min) => FromHttpApiData (Range min max) where
  parseQueryParam v = do
    n <- first (const . T.pack $ parseErrorMsg @min @max Proxy) $ parseQueryParam v
    maybe (Left . T.pack $ parseErrorMsg @min @max Proxy) Right $ mkRange n
