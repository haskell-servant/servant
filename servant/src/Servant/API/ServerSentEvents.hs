{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Server-sent events
--
-- See <https://www.w3.org/TR/2009/WD-eventsource-20090421/>.
module Servant.API.ServerSentEvents
  ( ServerSentEvents'
  , ServerSentEvents
  , EventKind (..)

    -- * Sending events
  , ServerEvent (..)
  , EventFields (..)
  , noEventFields
  , serverEvent
  , renderServerEvent
  , RenderEventData (..)
  )
where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import Network.HTTP.Types (StdMethod (GET))
import Numeric.Natural (Natural)

-- | Determines the shape of events you may receive (i.e. the @a@ in
-- 'ServerSentEvents\'')
data EventKind
  = -- | 'EventMessage' or 'Event' 'ByteString'
    RawEvent
  | -- | Anything that implements 'FromJSON'
    JsonEvent

-- | Server-sent events (SSE)
--
-- See <https://www.w3.org/TR/2009/WD-eventsource-20090421/>.
--
-- A server handler for this endpoint returns a stream of 'ServerEvent's (see
-- @servant-server@). For a 'JsonEvent' endpoint, the data of each event is the
-- JSON encoding of an @a@. For a 'RawEvent' endpoint, it is a lazy
-- 'BSL.ByteString' that is sent as it is.
data ServerSentEvents' (method :: k) (status :: Nat) (kind :: EventKind) (a :: Type)
  deriving (Generic, Typeable)

type ServerSentEvents = ServerSentEvents' 'GET 200

-- | One item that a server writes to an event stream.
data ServerEvent a
  = -- | An event. The client dispatches it to its listeners.
    ServerEvent EventFields a
  | -- | A comment. The client ignores it. Send one periodically to keep an
    -- idle connection open through proxies that close idle connections.
    ServerComment Text
  deriving (Eq, Foldable, Functor, Generic, Show, Traversable)

-- | The optional fields of an event, besides its data.
data EventFields = EventFields
  { eventFieldName :: Maybe Text
  -- ^ The @event@ field: the event type. The client uses @message@ when it is
  -- absent.
  , eventFieldId :: Maybe Text
  -- ^ The @id@ field. The client sends the last one it saw in the
  -- @Last-Event-ID@ header when it reconnects.
  , eventFieldRetry :: Maybe Natural
  -- ^ The @retry@ field: the reconnection time, in milliseconds.
  }
  deriving (Eq, Generic, Show)

-- | An event with no name, id or retry field.
noEventFields :: EventFields
noEventFields = EventFields Nothing Nothing Nothing

-- | An event with only data.
serverEvent :: a -> ServerEvent a
serverEvent = ServerEvent noEventFields

-- | Render one 'ServerEvent' in the event stream format, given how to render
-- the data of an event.
--
-- >>> renderServerEvent id (ServerEvent noEventFields{eventFieldId = Just "7"} "one\ntwo")
-- "id: 7\ndata: one\ndata: two\n\n"
--
-- >>> renderServerEvent id (ServerComment "keepalive" :: ServerEvent BSL.ByteString)
-- ": keepalive\n"
--
-- The data is split at line breaks into one @data@ line each, so that the
-- client joins them back to the same data. Line breaks in the name and id
-- cannot be represented, so they are dropped. A comment is not followed by a
-- blank line: a blank line ends an event, and some clients, the
-- @servant-client@ one among them, dispatch an event with empty data on it.
renderServerEvent :: (a -> BSL.ByteString) -> ServerEvent a -> BSL.ByteString
renderServerEvent _ (ServerComment comment) =
  Builder.toLazyByteString $
    foldMap (line ":") (eventLines (BSL.fromStrict (Text.encodeUtf8 comment)))
renderServerEvent renderData (ServerEvent EventFields{..} a) =
  Builder.toLazyByteString $
    foldMap (line "event:" . singleLine) eventFieldName
      <> foldMap (line "id:" . singleLine) eventFieldId
      <> foldMap (line "retry:" . BSL.fromStrict . Text.encodeUtf8 . Text.pack . show) eventFieldRetry
      <> foldMap (line "data:") (eventLines (renderData a))
      <> Builder.char7 '\n'
  where
    singleLine = BSL.fromStrict . Text.encodeUtf8 . Text.filter (\c -> c /= '\n' && c /= '\r')

-- | A field line: the field name with its colon, a space, the value.
line :: Builder.Builder -> BSL.ByteString -> Builder.Builder
line field value =
  field <> Builder.char7 ' ' <> Builder.lazyByteString value <> Builder.char7 '\n'

-- | Split at each CRLF, CR or LF, the line endings of the event stream format.
eventLines :: BSL.ByteString -> [BSL.ByteString]
eventLines s =
  let (l, rest) = BSL.break (\w -> w == 0x0A || w == 0x0D) s
   in case BSL.uncons rest of
        Nothing -> [l]
        Just (0x0D, rest')
          | Just (0x0A, rest'') <- BSL.uncons rest' -> l : eventLines rest''
        Just (_, rest') -> l : eventLines rest'

-- | How the data of an event on a @'ServerSentEvents'' method status kind a@
-- endpoint is given and rendered.
class RenderEventData (kind :: EventKind) a where
  -- | The data of one event.
  type ServerEventData kind a :: Type

  renderEventData :: Proxy kind -> Proxy a -> ServerEventData kind a -> BSL.ByteString

-- | The data is the JSON encoding of an @a@.
instance ToJSON a => RenderEventData 'JsonEvent a where
  type ServerEventData 'JsonEvent a = a
  renderEventData _ _ = encode

-- | The data is a lazy 'BSL.ByteString', sent as it is. The @a@ names what
-- a @servant-client@ client receives ('EventMessage' or 'Event').
instance RenderEventData 'RawEvent a where
  type ServerEventData 'RawEvent a = BSL.ByteString
  renderEventData _ _ = id

-- $setup
-- >>> :set -XOverloadedStrings
