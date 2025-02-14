{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.API.BoundedNat (BoundedNat (unBoundedNat), unsafeBoundedNat) where

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

-- | A newtype wrapper around 'Word' that ensures the value is within a given range.
--
-- Example:
--
-- >>> :set -XDataKinds
-- >>> import Data.Proxy
-- >>> let val = unsafeBoundedNat 5 :: BoundedNat 1 10
-- >>> unBoundedNat val
-- 5
-- >>> let invalidVal = unsafeBoundedNat 15 :: BoundedNat 1 10
-- >>> unBoundedNat invalidVal
-- 15
-- >>> checkBoundedRange (const $ fail "Out of range") val
-- MkBoundedNat {unBoundedNat = 5}
-- >>> checkBoundedRange (const $ fail "Out of range") invalidVal
-- *** Exception: user error (Out of range)

newtype BoundedNat (min :: Nat) (max :: Nat) = MkBoundedNat {unBoundedNat :: Word}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Ix, ToJSON)

unsafeBoundedNat :: Word -> BoundedNat min max
unsafeBoundedNat = MkBoundedNat

instance (KnownNat min, KnownNat max) => Bounded (BoundedNat min max) where
    minBound = MkBoundedNat . fromInteger $ natVal (Proxy @min)
    maxBound = MkBoundedNat . fromInteger $ natVal (Proxy @max)

parseErrorMsg :: forall min max. (KnownNat min, KnownNat max) => Proxy (BoundedNat min max) -> String
parseErrorMsg _ =
    "Expecting an integer between " <> show (natVal (Proxy @min)) <> " and " <> show (natVal (Proxy @max)) <> "."

checkBoundedRange ::
    forall min max m.
    (KnownNat min, KnownNat max, Monad m) =>
    (String -> m ()) ->
    BoundedNat min max ->
    m (BoundedNat min max)
checkBoundedRange f val = do
    unless (inRange (minBound, maxBound) val) . f $ parseErrorMsg @min @max Proxy
    pure val

instance (KnownNat min, KnownNat max) => FromJSON (BoundedNat min max) where
    parseJSON v = do
        val <- fmap MkBoundedNat . modifyFailure (const $ parseErrorMsg @min @max Proxy) $ parseJSON v
        checkBoundedRange fail val

instance (KnownNat min, KnownNat max) => ToHttpApiData (BoundedNat min max) where
    toQueryParam = T.pack . show . unBoundedNat

instance (KnownNat min, KnownNat max) => FromHttpApiData (BoundedNat min max) where
    parseQueryParam v = do
        val <- bimap (const . T.pack $ parseErrorMsg @min @max Proxy) MkBoundedNat $ parseQueryParam v
        checkBoundedRange (Left . T.pack) val
