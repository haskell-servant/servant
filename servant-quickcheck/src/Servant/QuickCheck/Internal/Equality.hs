module Servant.QuickCheck.Internal.Equality where

import Network.HTTP.Client
import Data.Function (on)

newtype ResponseEquality b
  = ResponseEquality { getResponseEquality :: Response b -> Response b -> Bool }

instance Monoid (ResponseEquality b) where
  mempty = ResponseEquality $ \_ _ -> True
  ResponseEquality a `mappend` ResponseEquality b = ResponseEquality $ \x y ->
    a x y && b x y

-- | Use `Eq` instance for `Response`
allEquality :: Eq b => ResponseEquality b
allEquality = ResponseEquality (==)

-- | ByteString `Eq` instance over the response body.
bodyEquality :: Eq b => ResponseEquality b
bodyEquality = ResponseEquality ((==) `on` responseBody)
