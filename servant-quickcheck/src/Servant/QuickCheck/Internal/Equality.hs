module Servant.QuickCheck.Internal.Equality where

import Network.HTTP.Client
import Data.Function (on)

-- | Often the normal equality of responses is not what we want. For example,
-- if responses contain a @Date@ header with the time of the response,
-- responses will fail to be equal even though they morally are. This datatype
-- represents other means of checking equality
newtype ResponseEquality b
  = ResponseEquality { getResponseEquality :: Response b -> Response b -> Bool }

instance Monoid (ResponseEquality b) where
  mempty = ResponseEquality $ \_ _ -> True
  ResponseEquality a `mappend` ResponseEquality b = ResponseEquality $ \x y ->
    a x y && b x y

allEquality :: Eq b => ResponseEquality b
allEquality = ResponseEquality (==)

bodyEquality :: Eq b => ResponseEquality b
bodyEquality = ResponseEquality ((==) `on` responseBody)
