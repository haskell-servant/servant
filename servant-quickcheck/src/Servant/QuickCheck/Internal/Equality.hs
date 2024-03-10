module Servant.QuickCheck.Internal.Equality where

import           Data.Aeson           (Value, decode, decodeStrict)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Function        (on)
import           Data.Semigroup       (Semigroup (..))
import           Network.HTTP.Client  (Response (..), equivCookieJar,
                                       responseBody, responseClose)
import           Prelude.Compat

newtype ResponseEquality b = ResponseEquality {getResponseEquality :: Response b -> Response b -> Bool}

instance Semigroup (ResponseEquality b) where
  ResponseEquality a <> ResponseEquality b = ResponseEquality $ \x y ->
    a x y && b x y

instance Monoid (ResponseEquality b) where
  mempty = ResponseEquality $ \_ _ -> True
  mappend = (<>)

{- | Use `Eq` instance for `Response`

/Since 0.0.0.0/
-}
allEquality :: (Eq b) => ResponseEquality b
allEquality = ResponseEquality $ \respa respb ->
  and
    [ responseStatus respa == responseStatus respb
    , responseVersion respa == responseVersion respb
    , responseHeaders respa == responseHeaders respb
    , responseBody respa == responseBody respb
    , responseCookieJar respa `equivCookieJar` responseCookieJar respb
    ]

{- | ByteString `Eq` instance over the response body.

/Since 0.0.0.0/
-}
bodyEquality :: (Eq b) => ResponseEquality b
bodyEquality = ResponseEquality ((==) `on` responseBody)

{- | Equality as 'Value'. This means that if two bodies are equal as JSON
(e.g., insignificant whitespace difference) they are considered equal.

/Since 0.0.3.0/
-}
jsonEquality :: (JsonEq b) => ResponseEquality b
jsonEquality = ResponseEquality (jsonEq `on` responseBody)

class JsonEq a where
  decode' :: a -> Maybe Value
  jsonEq :: a -> a -> Bool
  jsonEq first second = compareDecodedResponses (decode' first) (decode' second)

instance JsonEq LB.ByteString where
  decode' = decode

instance JsonEq ByteString where
  decode' = decodeStrict

compareDecodedResponses :: Maybe Value -> Maybe Value -> Bool
compareDecodedResponses resp1 resp2 =
  case resp1 of
    Nothing -> False -- if decoding fails we assume failure
    (Just r1) -> case resp2 of
      Nothing   -> False -- another decode failure
      (Just r2) -> r1 == r2
