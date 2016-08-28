module Servant.QuickCheck.Internal.Equality where

import Data.Function       (on)
import Network.HTTP.Client (Response, responseBody)
import Prelude.Compat

newtype ResponseEquality b
  = ResponseEquality { getResponseEquality :: Response b -> Response b -> Bool }

instance Monoid (ResponseEquality b) where
  mempty = ResponseEquality $ \_ _ -> True
  ResponseEquality a `mappend` ResponseEquality b = ResponseEquality $ \x y ->
    a x y && b x y

-- | Use `Eq` instance for `Response`
--
-- /Since 0.0.0.0/
allEquality :: Eq b => ResponseEquality b
allEquality = ResponseEquality (==)

-- | ByteString `Eq` instance over the response body.
--
-- /Since 0.0.0.0/
bodyEquality :: Eq b => ResponseEquality b
bodyEquality = ResponseEquality ((==) `on` responseBody)
