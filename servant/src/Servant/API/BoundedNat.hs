{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.API.BoundedNat (BoundedNat (unBoundedNat), unsafeBoundedNat) where

import           Data.Aeson
import           Data.Aeson.Types (modifyFailure)
import           Data.Bifunctor   (bimap)
import           Data.Ix

-- import Data.OpenApi

import           Control.Monad    (unless)
import           Data.Proxy       (Proxy (Proxy))
import qualified Data.Text        as T
import           GHC.Generics     (Generic)
import           GHC.TypeLits
import           Servant.API

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

-- instance (KnownNat min, KnownNat max) => ToSchema (BoundedNat min max) where
--     declareNamedSchema _ =
--         pure . NamedSchema Nothing $
--             mempty
--                 & #type ?~ OpenApiInteger
--                 & #minimum ?~ fromIntegral (minBound @(BoundedNat min max)) . unBoundedNat
--                 & #maximum ?~ fromIntegral (maxBound @(BoundedNat min max)) . unBoundedNat

-- instance (KnownNat min, KnownNat max) => ToParamSchema (BoundedNat min max) where
--     toParamSchema = toSchema
