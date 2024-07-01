{-# LANGUAGE ApplicativeDo #-}
module Servant.Client.Core.MultiVerb.ResponseUnrender where

import Control.Applicative
import Control.Monad
import Data.Kind (Type)
import Data.SOP
import Data.Typeable
import GHC.TypeLits
import Network.HTTP.Types.Status (Status)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Media as M

import Servant.API.ContentTypes
import Servant.API.MultiVerb
import Servant.API.Status
import Servant.API.UVerb.Union (Union)
import Servant.Client.Core.Response (ResponseF(..))
import qualified Servant.Client.Core.Response as Response
import Servant.API.Stream (SourceIO)
import Data.ByteString (ByteString)

data SomeClientResponse = forall a. Typeable a => SomeClientResponse (ResponseF a)

fromSomeClientResponse 
  :: forall a m. (Alternative m, Typeable a)
  => SomeClientResponse
  -> m (ResponseF a)
fromSomeClientResponse (SomeClientResponse Response {..}) = do
  body <- maybe empty pure $ cast @_ @a responseBody
  pure $
    Response
      { responseBody = body,
        ..
      }

class ResponseUnrender cs a where
  type ResponseBody a :: Type
  type ResponseStatus a :: Nat
  responseUnrender
    :: M.MediaType
    -> ResponseF (ResponseBody a)
    -> UnrenderResult (ResponseType a)

class (Typeable as) => ResponseListUnrender cs as where
  responseListUnrender
    :: M.MediaType
    -> SomeClientResponse
    -> UnrenderResult (Union (ResponseTypes as))

  responseListStatuses :: [Status]

instance ResponseListUnrender cs '[] where
  responseListUnrender _ _ = StatusMismatch
  responseListStatuses = []

instance
  ( Typeable a,
    Typeable (ResponseBody a),
    ResponseUnrender cs a,
    ResponseListUnrender cs as,
    KnownStatus (ResponseStatus a)
  ) =>
  ResponseListUnrender cs (a ': as)
  where
  responseListUnrender c output =
    Z . I <$> (responseUnrender @cs @a c =<< fromSomeClientResponse output)
      <|> S <$> responseListUnrender @cs @as c output

  responseListStatuses = statusVal (Proxy @(ResponseStatus a)) : responseListStatuses @cs @as

instance
  ( KnownStatus s,
    MimeUnrender ct a
  ) =>
  ResponseUnrender cs (RespondAs (ct :: Type) s desc a)
  where
  type ResponseStatus (RespondAs ct s desc a) = s
  type ResponseBody (RespondAs ct s desc a) = BSL.ByteString

  responseUnrender _ output = do
    guard (responseStatusCode output == statusVal (Proxy @s))
    either UnrenderError UnrenderSuccess $
      mimeUnrender (Proxy @ct) (Response.responseBody output)

instance (KnownStatus s) => ResponseUnrender cs (RespondAs '() s desc ()) where
  type ResponseStatus (RespondAs '() s desc ()) = s
  type ResponseBody (RespondAs '() s desc ()) = ()

  responseUnrender _ output =
    guard (responseStatusCode output == statusVal (Proxy @s))

instance
  (KnownStatus s) 
  => ResponseUnrender cs (RespondStreaming s desc framing ct)
  where
  type ResponseStatus (RespondStreaming s desc framing ct) = s
  type ResponseBody (RespondStreaming s desc framing ct) = SourceIO ByteString

  responseUnrender _ resp = do
    guard (Response.responseStatusCode resp == statusVal (Proxy @s))
    pure $ Response.responseBody resp

instance
  (AllMimeUnrender cs a, KnownStatus s)
  => ResponseUnrender cs (Respond s desc a) where
  type ResponseStatus (Respond s desc a) = s
  type ResponseBody (Respond s desc a) = BSL.ByteString

  responseUnrender c output = do
    guard (responseStatusCode output == statusVal (Proxy @s))
    let results = allMimeUnrender (Proxy @cs)
    case lookup c results of
      Nothing -> empty
      Just f -> either UnrenderError UnrenderSuccess (f (responseBody output))

instance
  ( AsHeaders xs (ResponseType r) a,
    ServantHeaders hs xs,
    ResponseUnrender cs r
  ) =>
  ResponseUnrender cs (WithHeaders hs a r)
  where
  type ResponseStatus (WithHeaders hs a r) = ResponseStatus r
  type ResponseBody (WithHeaders hs a r) = ResponseBody r

  responseUnrender c output = do
    x <- responseUnrender @cs @r c output
    case extractHeaders @hs (responseHeaders output) of
      Nothing -> UnrenderError "Failed to parse headers"
      Just hs -> pure $ fromHeaders @xs (hs, x)
