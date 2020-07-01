{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Server-sent events
--
-- See <https://www.w3.org/TR/2009/WD-eventsource-20090421/> for more details
-- on server-sent events (SSE).
--
module Servant.Client.Core.ServerSentEvents (
    EventMessage (..),
    EventIgnoreReason (..),
    Event (..),
    EventStreamT (..),
    JsonEventStreamT (..),
    EventMessageStreamT (..)
) where

import           Control.Applicative
                 (Alternative ((<|>)))
import           Control.Monad.IO.Class
                 (MonadIO)
import qualified Data.Aeson                 as Aeson
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Char8      as ByteString.Char8
import qualified Data.ByteString.Lazy       as ByteString.Lazy
import           Data.Char
                 (chr)
import           Data.Coerce
                 (coerce)
import           Data.Foldable
                 (traverse_)
import           Data.Functor
                 (void)
import qualified Data.Text                  as Text
import           Data.Text.Encoding
                 (encodeUtf8)
import           GHC.Generics
                 (Generic)
import           Numeric.Natural
                 (Natural)
import           Servant.API.ContentTypes
                 (EventStreamChunk (..))
import           Servant.API.Stream
                 (FromSourceIO (..))
import           Servant.Types.SourceT
                 (SourceT, StepT (..), foreachYieldStep, mapStepT,
                 transformStepWithAtto)

-- | Line (or frame) of an event stream
newtype EventStreamLine = EventStreamLine
    { unEventStreamLine :: ByteString.ByteString }
    deriving Show

-- | Consume chunks to produce event stream lines.
eventLinesFromRawChunks
    :: Monad m
    => StepT m ByteString.ByteString
    -> StepT m EventStreamLine
eventLinesFromRawChunks =
    transformStepWithAtto eventLine

-- | Consume event stream chunks to produce event stream lines.
eventLinesFromChunks
    :: Monad m
    => StepT m EventStreamChunk
    -> StepT m EventStreamLine
eventLinesFromChunks =
    -- 'coerce' efficiently unpacks the 'EventStreamChunk'
    eventLinesFromRawChunks . fmap (coerce ByteString.Lazy.toStrict)

-- | Apply a 'Attoparsec.Parser' to each line of the event stream individually.
parseEventLines
    :: Monad m
    => Attoparsec.Parser a
    -> StepT m EventStreamLine
    -> StepT m a
parseEventLines parser =
    foreachYieldStep $ \(EventStreamLine line) next ->
        case Attoparsec.parseOnly parser line of
            Left err    -> Error err
            Right value -> Yield value next

-- | A line of an event stream
eventLine :: Attoparsec.Parser EventStreamLine
eventLine = do
    Attoparsec.option () byteOrderMark -- A line may be prefixed with a byte order mark
    EventStreamLine <$> untilLineEnd <* lineEnd

-- | Byte order mark (U+FEFF) in UTF-8 representation
byteOrderMark :: Attoparsec.Parser ()
byteOrderMark =
    traverse_ Attoparsec.word8
    $ ByteString.unpack
    $ encodeUtf8
    $ Text.singleton
    $ chr 0xFEFF

-- | Event stream line ending
lineEnd :: Attoparsec.Parser ()
lineEnd =
    (cr >> lf) <|> cr <|> lf <|> Attoparsec.endOfInput
    where
        cr = void (Attoparsec.word8 0x0D)
        lf = void (Attoparsec.word8 0x0A)

-- | Consume all contents until the end of the line.
untilLineEnd :: Attoparsec.Parser ByteString.ByteString
untilLineEnd = Attoparsec.takeWhile (\w8 -> w8 /= 0x0D && w8 /= 0x0A)

-- | Structured variant of an event line of an event stream
data EventMessage
    = EventDispatch
        -- ^ Dispatch on the accumulated event.
    | EventSetName ByteString.ByteString
        -- ^ Set the name of the current event.
    | EventSetLastId ByteString.ByteString
        -- ^ Set the last event identifier.
    | EventData ByteString.ByteString
        -- ^ Append data to the event's data buffer.
    | EventRetry Natural
        -- ^ Set the event stream's reconnection time.
    | EventIgnore EventIgnoreReason
        -- ^ Ignored
    deriving (Show, Eq, Ord)

-- | Reason why a event line can be ignored
data EventIgnoreReason
    = EventFieldNameUnknown ByteString.ByteString
    | EventRetryNonNumeric ByteString.ByteString
    | EventComment ByteString.ByteString
    deriving (Show, Eq, Ord)

-- | Parse the event stream lines into more structured messages.
eventMessagesFromLines
    :: Monad m
    => StepT m EventStreamLine
    -> StepT m EventMessage
eventMessagesFromLines =
    ensureLastDispatch False . parseEventLines eventMessage
    where
        -- | Make sure the last event message is a dispatch.
        ensureLastDispatch didDispatch step = case step of
            Stop ->
                if not didDispatch then Yield EventDispatch Stop else Stop
            Yield other next ->
                Yield other $ ensureLastDispatch (other == EventDispatch) next
            Skip next ->
                Skip $ ensureLastDispatch didDispatch next
            Effect eff ->
                Effect $ ensureLastDispatch didDispatch <$> eff
            err@Error{} ->
                 err

-- | Event line parser for an event message.
eventMessage :: Attoparsec.Parser EventMessage
eventMessage =
    ignore <|> field <|> dispatch
    where
        ignore = do
            _ <- Attoparsec.word8 0x3A -- ':'
            EventIgnore . EventComment <$> Attoparsec.takeByteString

        dispatch = do
            Attoparsec.endOfInput
            pure EventDispatch

        field = do
            name <- Attoparsec.takeWhile1 (/= 0x3A) -- Up to ':' or the end

            value <- Attoparsec.option ByteString.empty $ do
                _ <- Attoparsec.word8 0x3A -- ':'
                _ <- Attoparsec.option 0x20 $ Attoparsec.word8 0x20 -- Optional ' '
                Attoparsec.takeByteString

            pure $ case name of
                "event" -> EventSetName value

                "data" -> EventData value

                "id" -> EventSetLastId value

                "retry" ->
                    -- The retry value consist of digits.
                    if ByteString.all (\w8 -> 0x30 <= w8 && w8 <= 0x39) value then
                        EventRetry (read (ByteString.Char8.unpack value))
                    else
                        EventIgnore (EventRetryNonNumeric value)

                _ -> EventIgnore (EventFieldNameUnknown name)

-- | Event sent by the remote
data Event a = Event
    { eventName :: Maybe ByteString.ByteString
    , eventData :: a
    }
    deriving (Show, Eq, Ord, Functor, Generic)

-- | Accumulate event messages to build individual 'Event's.
eventsFromMessages
    :: Functor m
    => StepT m EventMessage
    -> StepT m (Event ByteString.ByteString)
eventsFromMessages =
    initGo
    where
        initGo = go Nothing ByteString.Lazy.empty

        combineData dataBuffer newData =
            if ByteString.Lazy.null dataBuffer then
                ByteString.Lazy.fromStrict newData
            else
                ByteString.Lazy.concat
                    [ dataBuffer
                    , ByteString.Lazy.singleton 0x0A -- Line feed
                    , ByteString.Lazy.fromStrict newData
                    ]

        go name dataBuffer step = case step of
            Stop ->
                Stop
            Skip next ->
                go name dataBuffer next
            Effect eff ->
                Effect (go name dataBuffer <$> eff)
            Error err ->
                Error err
            Yield message next -> case message of
                EventSetName newName ->
                    go (Just newName) dataBuffer next
                EventData newData ->
                    go name (combineData dataBuffer newData) next
                EventDispatch ->
                    Yield
                        (Event name (ByteString.Lazy.toStrict dataBuffer))
                        (initGo next)
                _ ->
                    -- We ignore other message because they don't fit into
                    -- the 'Event' type. If a user needs more fine grained
                    -- control, the 'EventMessage' interface is better suited.
                    go name dataBuffer next

-- | Server-sent event stream (SSE)
--
-- See <https://www.w3.org/TR/2009/WD-eventsource-20090421/> for more details.
--
newtype EventMessageStreamT m = EventMessageStreamT
    { unEventMessageStreamT :: SourceT m EventMessage }
    deriving (Show, Semigroup, Monoid)

-- | Server-sent event messages
--
-- 'EventMessage' gives you more control over the communication with the server
-- than 'Event'.
--
instance MonadIO m => FromSourceIO EventStreamChunk (EventMessageStreamT m) where
    fromSourceIO =
        EventMessageStreamT
        . mapStepT (eventMessagesFromLines . eventLinesFromChunks)
        . fromSourceIO

-- | Server-sent event stream (SSE)
--
-- See <https://www.w3.org/TR/2009/WD-eventsource-20090421/> for more details.
--
newtype EventStreamT m = EventStreamT
    { unEventStreamT :: SourceT m (Event ByteString.ByteString) }
    deriving (Show, Semigroup, Monoid)

-- | Server-sent events
instance MonadIO m => FromSourceIO EventStreamChunk (EventStreamT m) where
    fromSourceIO input =
        -- 'coerce' is used in place of unpacking and repacking 'EventStreamT'
        coerce
            (mapStepT eventsFromMessages)
            (fromSourceIO input :: EventMessageStreamT m)

-- | Try to parse event data to JSON.
jsonEventsFromEvents
    :: (Functor m, Aeson.FromJSON a)
    => StepT m (Event ByteString.ByteString)
    -> StepT m (Event a)
jsonEventsFromEvents =
    foreachYieldStep $ \(Event name datas) next ->
        either
            Error
            (\value -> Yield (Event name value) next)
            (Aeson.eitherDecode (ByteString.Lazy.fromStrict datas))

-- | Server-sent event stream (SSE) for JSON values
newtype JsonEventStreamT m a = JsonEventStreamT
    { unJsonEventStreamT :: SourceT m (Event a) }
    deriving (Show, Functor, Semigroup, Monoid)

-- | Server-sent JSON event stream
instance (MonadIO m, Aeson.FromJSON a) => FromSourceIO EventStreamChunk (JsonEventStreamT m a) where
    fromSourceIO input =
        -- The 'coerce' efficiently unwraps the 'EventStreamT' and wraps the
        -- JsonEventStreamT.
        coerce
            (mapStepT jsonEventsFromEvents)
            (fromSourceIO input :: EventStreamT m)
