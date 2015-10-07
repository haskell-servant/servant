{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_HADDOCK not-home #-}

module Servant.API.Raw where

import Control.Monad (liftM)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Foldable
import Data.Ix (Ix(..))
import GHC.Generics (Generic, Generic1)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Data.Traversable (Traversable(..))
import Control.Applicative (liftA2, (<$>), Applicative(..))
#else
import Control.Applicative (liftA2)
#endif
-- | Endpoint for plugging in your own Wai 'Application's.
--
-- The given 'Application' will get the request as received by the server,
-- potentially with a modified (stripped) 'pathInfo' if the 'Application' is
-- being routed with 'Servant.API.Sub.:>'.
--
-- In addition to just letting you plug in your existing WAI 'Application's,
-- this can also be used with 'Servant.Utils.StaticFiles.serveDirectory' to serve
-- static files stored in a particular directory on your filesystem
--
-- The phantom type (@m@) is used internally, and can generally be ignored.
newtype Raw (m :: * -> *) a = Raw {
    unRaw :: a
    } deriving (Eq, Read, Show, Ord, Typeable, Ix, Bounded, Data, Generic, Generic1)

instance Monoid a => Monoid (Raw m a) where
    mempty = Raw mempty
    mappend (Raw a) (Raw b) = Raw (mappend a b)

instance Functor (Raw m) where
    fmap f (Raw x) = Raw (f x)

instance Applicative (Raw m) where
    pure = Raw
    Raw f <*> Raw x = Raw (f x)

instance Monad (Raw m) where
    return = Raw
    Raw m >>= k = k m
    _ >> n = n

instance Foldable (Raw m) where
    foldMap f (Raw x) = f x
    fold (Raw x) = x
    foldr f z (Raw x) = f x z
    foldl f z (Raw x) = f z x
    foldl1 _ (Raw x) = x
    foldr1 _ (Raw x) = x

instance Traversable (Raw m) where
    traverse f (Raw x) = Raw <$> f x
    sequenceA (Raw x) = Raw <$> x
    mapM f (Raw x) = liftM Raw (f x)
    sequence (Raw x) = liftM Raw x

instance Enum a => Enum (Raw m a) where
    succ = fmap succ
    pred = fmap pred
    toEnum = Raw . toEnum
    fromEnum (Raw x) = fromEnum x
    enumFrom (Raw x) = map Raw (enumFrom x)
    enumFromThen (Raw x) (Raw y) = map Raw (enumFromThen x y)
    enumFromTo (Raw x) (Raw y) = map Raw (enumFromTo x y)
    enumFromThenTo (Raw x) (Raw y) (Raw z) = map Raw (enumFromThenTo x y z)

instance Num a => Num (Raw m a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Raw . fromInteger

instance Real a => Real (Raw m a) where
    toRational (Raw x) = toRational x

instance Integral a => Integral (Raw m a) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem (Raw x) (Raw y) = (Raw a, Raw b) where
        (a, b) = quotRem x y
    divMod (Raw x) (Raw y) = (Raw a, Raw b) where
        (a, b) = divMod x y
    toInteger (Raw x) = toInteger x

instance Fractional a => Fractional (Raw m a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = Raw . fromRational

instance Floating a => Floating (Raw m a) where
    pi = Raw pi
    exp = fmap exp
    log = fmap log
    sqrt = fmap sqrt
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh
    (**) = liftA2 (**)
    logBase = liftA2 (**)

instance RealFrac a => RealFrac (Raw m a) where
    properFraction (Raw x) = (a, Raw b) where
        (a, b) = properFraction x
    truncate (Raw x) = truncate x
    round (Raw x) = round x
    ceiling (Raw x) = ceiling x
    floor (Raw x) = floor x

instance RealFloat a => RealFloat (Raw m a) where
    floatRadix (Raw x) = floatRadix x
    floatDigits (Raw x) = floatDigits x
    floatRange (Raw x) = floatRange x
    decodeFloat (Raw x) = decodeFloat x
    encodeFloat m n = Raw (encodeFloat m n)
    exponent (Raw x) = exponent x
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN (Raw x) = isNaN x
    isInfinite (Raw x) = isInfinite x
    isDenormalized (Raw x) = isDenormalized x
    isNegativeZero (Raw x) = isNegativeZero x
    isIEEE (Raw x) = isIEEE x
    atan2 = liftA2 atan2
