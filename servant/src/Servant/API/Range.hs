{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.API.Range (Range (unRange), unsafeRange) where

import           Control.Monad    (unless)
import           Data.Aeson
import           Data.Aeson.Types (modifyFailure)
import           Data.Bifunctor   (bimap)
import           Data.Ix
import           Data.Proxy       (Proxy (Proxy))
import qualified Data.Text        as T
import           GHC.Generics     (Generic)
import           GHC.TypeLits
import           Servant.API

-- | A newtype wrapper around 'Natural' that ensures the value is within a given range.
--
-- Example:
--
-- >>> :set -XDataKinds
-- >>> import Data.Proxy
-- >>> let val = unsafeRange 5 :: Range 1 10
-- >>> unRange val
-- 5
-- >>> let invalidVal = unsafeRange 15 :: Range 1 10
-- >>> unRange invalidVal
-- 15
-- >>> checkInRange (const $ fail "Out of range") val
-- MkRange {unRange = 5}
-- >>> checkInRange (const $ fail "Out of range") invalidVal
-- *** Exception: user error (Out of range)

newtype Range (min :: Nat) (max :: Nat) = MkRange {unRange :: Natural}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Ix, ToJSON)

unsafeRange :: Natural -> Range min max
unsafeRange = MkRange

instance (KnownNat min, KnownNat max) => Bounded (Range min max) where
    minBound = MkRange . fromInteger $ natVal (Proxy @min)
    maxBound = MkRange . fromInteger $ natVal (Proxy @max)

parseErrorMsg :: forall min max. (KnownNat min, KnownNat max) => Proxy (Range min max) -> String
parseErrorMsg _ =
    "Expecting an integer between " <> show (natVal (Proxy @min)) <> " and " <> show (natVal (Proxy @max)) <> "."

checkInRange ::
    forall min max m.
    (KnownNat min, KnownNat max, Monad m) =>
    (String -> m ()) ->
    Range min max ->
    m (Range min max)
checkInRange f val = do
    unless (inRange (minBound, maxBound) val) . f $ parseErrorMsg @min @max Proxy
    pure val

instance (KnownNat min, KnownNat max) => FromJSON (Range min max) where
    parseJSON v = do
        val <- fmap MkRange . modifyFailure (const $ parseErrorMsg @min @max Proxy) $ parseJSON v
        checkInRange fail val

instance (KnownNat min, KnownNat max) => ToHttpApiData (Range min max) where
    toQueryParam = T.pack . show . unRange

instance (KnownNat min, KnownNat max) => FromHttpApiData (Range min max) where
    parseQueryParam v = do
        val <- bimap (const . T.pack $ parseErrorMsg @min @max Proxy) MkRange $ parseQueryParam v
        checkInRange (Left . T.pack) val
