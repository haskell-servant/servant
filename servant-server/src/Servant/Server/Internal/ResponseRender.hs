{-# LANGUAGE EmptyCase #-}

module Servant.Server.Internal.ResponseRender where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Typeable
import GHC.TypeLits
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai as Wai
import Network.HTTP.Types (Status, hContentType)
import Data.SOP
import qualified Servant.Types.SourceT                      as S
import qualified Data.ByteString.Builder                    as BB
import qualified Data.Sequence as Seq 

import Servant.API.ContentTypes (AcceptHeader (..), AllMimeRender, MimeRender, Accept, allMimeRender, mimeRender, contentType)
import Servant.API.MultiVerb
import Servant.API.Status
import Servant.API.Stream (SourceIO)
import Servant.API.UVerb.Union
import Servant.Types.Internal.ResponseList
import qualified Network.HTTP.Media as M
import Data.Foldable (toList)
import Data.Sequence ((<|))

class (Typeable a) => IsWaiBody a where
  responseToWai :: InternalResponse a -> Wai.Response

instance IsWaiBody BSL.ByteString where
  responseToWai r =
    Wai.responseLBS
      (statusCode r)
      (toList (headers r))
      (responseBody r)

instance IsWaiBody () where
  responseToWai r =
    Wai.responseLBS
      (statusCode r)
      (toList (headers r))
      mempty

instance IsWaiBody (SourceIO ByteString) where
  responseToWai r =
    Wai.responseStream
      (statusCode r)
      (toList (headers r))
      $ \output flush -> do
        S.foreach
          (const (pure ()))
          (\chunk -> output (BB.byteString chunk) *> flush)
          (responseBody r)

data SomeResponse = forall a. (IsWaiBody a) => SomeResponse (InternalResponse a)

class ResponseListRender cs as where
  responseListRender
    :: AcceptHeader
    -> Union (ResponseTypes as)
    -> Maybe SomeResponse
  responseListStatuses :: [Status]

instance ResponseListRender cs '[] where
  responseListRender _ x = case x of {}
  responseListStatuses = []

class (IsWaiBody (ResponseBody a)) => ResponseRender cs a where
  type ResponseStatus a :: Nat
  type ResponseBody a :: Type
  responseRender
    :: AcceptHeader
    -> ResponseType a
    -> Maybe (InternalResponse (ResponseBody a))

instance
  ( ResponseRender cs a,
    ResponseListRender cs as,
    KnownStatus (ResponseStatus a)
  ) =>
  ResponseListRender cs (a ': as)
  where
  responseListRender acc (Z (I x)) = fmap SomeResponse (responseRender @cs @a acc x)
  responseListRender acc (S x) = responseListRender @cs @as acc x

  responseListStatuses = statusVal (Proxy @(ResponseStatus a)) : responseListStatuses @cs @as

instance
  ( AsHeaders xs (ResponseType r) a,
    ServantHeaders hs xs,
    ResponseRender cs r
  ) =>
  ResponseRender cs (WithHeaders hs a r)
  where
  type ResponseStatus (WithHeaders hs a r) = ResponseStatus r
  type ResponseBody (WithHeaders hs a r) = ResponseBody r

  responseRender acc x = addHeaders <$> responseRender @cs @r acc y
    where
      (hs, y) = toHeaders @xs x
      addHeaders r =
        r
          { headers = headers r <> Seq.fromList (constructHeaders @hs hs)
          }

instance
  ( KnownStatus s,
    MimeRender ct a
  ) =>
  ResponseRender cs (RespondAs (ct :: Type) s desc a)
  where
  type ResponseStatus (RespondAs ct s desc a) = s
  type ResponseBody (RespondAs ct s desc a) = BSL.ByteString

  responseRender _ x =
    pure . addContentType @ct $
      InternalResponse
        { statusCode = statusVal (Proxy @s),
          responseBody = mimeRender (Proxy @ct) x,
          headers = mempty
        }

instance (KnownStatus s) => ResponseRender cs (RespondAs '() s desc ()) where
  type ResponseStatus (RespondAs '() s desc ()) = s
  type ResponseBody (RespondAs '() s desc ()) = ()

  responseRender _ _ =
    pure $
      InternalResponse
        { statusCode = statusVal (Proxy @s),
          responseBody = (),
          headers = mempty
        }

instance
  (Accept ct, KnownStatus s)
  => ResponseRender cs (RespondStreaming s desc framing ct)
  where
  type ResponseStatus (RespondStreaming s desc framing ct) = s
  type ResponseBody (RespondStreaming s desc framing ct) = SourceIO ByteString 
  responseRender _ x =
    pure . addContentType @ct $
      InternalResponse
        { statusCode = statusVal (Proxy @s),
          responseBody = x,
          headers = mempty
        }

instance
  (AllMimeRender cs a, KnownStatus s)
  => ResponseRender cs (Respond s desc a) where
  type ResponseStatus (Respond s desc a) = s
  type ResponseBody (Respond s desc a) = BSL.ByteString

  -- Note: here it seems like we are rendering for all possible content types,
  -- only to choose the correct one afterwards. However, render results besides the
  -- one picked by 'M.mapAcceptMedia' are not evaluated, and therefore nor are the
  -- corresponding rendering functions.
  responseRender (AcceptHeader acc) x =
    M.mapAcceptMedia (map (uncurry mkRenderOutput) (allMimeRender (Proxy @cs) x)) acc
    where
      mkRenderOutput :: M.MediaType -> BSL.ByteString -> (M.MediaType, InternalResponse BSL.ByteString)
      mkRenderOutput c body =
        (c,) . addContentType' c $
          InternalResponse
            { statusCode = statusVal (Proxy @s),
              responseBody = body,
              headers = mempty
            }

addContentType :: forall ct a. (Accept ct) => InternalResponse a -> InternalResponse a
addContentType = addContentType' (contentType (Proxy @ct))

addContentType' :: M.MediaType -> InternalResponse a -> InternalResponse a
addContentType' c r = r {headers = (hContentType, M.renderHeader c) <| headers r}

setEmptyBody :: SomeResponse -> SomeResponse
setEmptyBody (SomeResponse r) = SomeResponse (go r)
  where
    go :: InternalResponse a -> InternalResponse BSL.ByteString
    go InternalResponse {..} = InternalResponse {responseBody = mempty, ..}

someResponseToWai :: SomeResponse -> Wai.Response
someResponseToWai (SomeResponse r) = responseToWai r
