{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# OPTIONS_HADDOCK not-home #-}

module Servant.API.Stream (
    Stream,
    StreamGet,
    StreamPost,
    StreamBody,
    StreamBody',
    -- * Source
    --
    -- | 'SourceIO' are equivalent to some *source* in streaming libraries.
    SourceIO,
    ToSourceIO (..),
    FromSourceIO (..),
    -- ** Auxiliary classes
    SourceToSourceIO (..),
    -- * Framing
    FramingRender (..),
    FramingUnrender (..),
    -- ** Strategies
    NoFraming,
    NewlineFraming,
    NetstringFraming,
    ) where


import           Control.Applicative
                 ((<|>))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Char8       as LBS8
import           Data.List.NonEmpty
                 (NonEmpty (..))
import           Data.Proxy
                 (Proxy)
import           Data.Typeable
                 (Typeable)
import           GHC.Generics
                 (Generic)
import           GHC.TypeLits
                 (Nat)
import           Network.HTTP.Types.Method
                 (StdMethod (..))
import           Servant.Types.SourceT

-- | A Stream endpoint for a given method emits a stream of encoded values at a
-- given @Content-Type@, delimited by a @framing@ strategy.
-- Type synonyms are provided for standard methods.
--
data Stream (method :: k1) (status :: Nat) (framing :: *) (contentType :: *) (a :: *)
  deriving (Typeable, Generic)

type StreamGet  = Stream 'GET 200
type StreamPost = Stream 'POST 200

-- | A stream request body.
type StreamBody = StreamBody' '[]

data StreamBody' (mods :: [*]) (framing :: *) (contentType :: *) (a :: *)
  deriving (Typeable, Generic)

-------------------------------------------------------------------------------
-- Sources
-------------------------------------------------------------------------------

-- | Stream endpoints may be implemented as producing a @'SourceIO' chunk@.
--
-- Clients reading from streaming endpoints can be implemented as consuming a
-- @'SourceIO' chunk@.
--
type SourceIO = SourceT IO

-- | 'ToSourceIO' is intended to be implemented for types such as Conduit, Pipe,
-- etc. By implementing this class, all such streaming abstractions can be used
-- directly as endpoints.
class ToSourceIO chunk a | a -> chunk where
    toSourceIO :: a -> SourceIO chunk

-- | Auxiliary class for @'ToSourceIO' x ('SourceT' m x)@ instance.
class SourceToSourceIO m where
    sourceToSourceIO :: SourceT m a -> SourceT IO a

instance SourceToSourceIO IO where
    sourceToSourceIO = id

-- | Relax to use auxiliary class, have m
instance SourceToSourceIO m => ToSourceIO chunk (SourceT m chunk) where
    toSourceIO = sourceToSourceIO

instance ToSourceIO a (NonEmpty a) where
    toSourceIO (x :| xs) = fromStepT (Yield x (foldr Yield Stop xs))

instance ToSourceIO a [a] where
    toSourceIO = source

-- | 'FromSourceIO' is intended to be implemented for types such as Conduit,
-- Pipe, etc. By implementing this class, all such streaming abstractions can
-- be used directly on the client side for talking to streaming endpoints.
class FromSourceIO chunk a | a -> chunk where
    fromSourceIO :: SourceIO chunk -> a

instance MonadIO m => FromSourceIO a (SourceT m a) where
    fromSourceIO = sourceFromSourceIO

sourceFromSourceIO :: forall m a. MonadIO m => SourceT IO a -> SourceT m a
sourceFromSourceIO src =
    SourceT $ \k ->
    k $ Effect $ liftIO $ unSourceT src (return . go)
  where
    go :: StepT IO a -> StepT m a
    go Stop        = Stop
    go (Error err) = Error err
    go (Skip s)    = Skip (go s)
    go (Effect ms) = Effect (liftIO (fmap go ms))
    go (Yield x s) = Yield x (go s)

-- This fires e.g. in Client.lhs
-- {-# OPTIONS_GHC -ddump-simpl -ddump-rule-firings -ddump-to-file #-}
{-# NOINLINE [2] sourceFromSourceIO #-}
{-# RULES "sourceFromSourceIO @IO" sourceFromSourceIO = id :: SourceT IO a -> SourceT IO a #-}

-------------------------------------------------------------------------------
-- Framing
-------------------------------------------------------------------------------

-- | The 'FramingRender' class provides the logic for emitting a framing strategy.
-- The strategy transforms a @'SourceT' m a@ into @'SourceT' m 'LBS.ByteString'@,
-- therefore it can prepend, append and intercalate /framing/ structure
-- around chunks.
--
-- /Note:/ as the @'Monad' m@ is generic, this is pure transformation.
--
class FramingRender strategy where
    framingRender :: Monad m => Proxy strategy -> (a -> LBS.ByteString) -> SourceT m a -> SourceT m LBS.ByteString

-- | The 'FramingUnrender' class provides the logic for parsing a framing
-- strategy.
class FramingUnrender strategy where
    framingUnrender :: Monad m => Proxy strategy -> (LBS.ByteString -> Either String a) -> SourceT m BS.ByteString -> SourceT m a

-------------------------------------------------------------------------------
-- NoFraming
-------------------------------------------------------------------------------

-- | A framing strategy that does not do any framing at all, it just passes the
-- input data This will be used most of the time with binary data, such as
-- files
data NoFraming

instance FramingRender NoFraming where
    framingRender _ = fmap

-- | As 'NoFraming' doesn't have frame separators, we take the chunks
-- as given and try to convert them one by one.
--
-- That works well when @a@ is a 'ByteString'.
instance FramingUnrender NoFraming where
    framingUnrender _ f = mapStepT go
      where
        go Stop        = Stop
        go (Error err) = Error err
        go (Skip s)    = Skip (go s)
        go (Effect ms) = Effect (fmap go ms)
        go (Yield x s) = case f (LBS.fromStrict x) of
            Right y  -> Yield y (go s)
            Left err -> Error err

-------------------------------------------------------------------------------
-- NewlineFraming
-------------------------------------------------------------------------------

-- | A simple framing strategy that has no header, and inserts a
-- newline character after each frame.  This assumes that it is used with a
-- Content-Type that encodes without newlines (e.g. JSON).
data NewlineFraming

instance FramingRender NewlineFraming where
    framingRender _ f = fmap (\x -> f x <> "\n")

instance FramingUnrender NewlineFraming where
    framingUnrender _ f = transformWithAtto $ do
        bs <- A.takeWhile (/= 10)
        () <$ A.word8 10 <|> A.endOfInput
        either fail pure (f (LBS.fromStrict bs))

-------------------------------------------------------------------------------
-- NetstringFraming
-------------------------------------------------------------------------------

-- | The netstring framing strategy as defined by djb:
-- <http://cr.yp.to/proto/netstrings.txt>
--
-- Any string of 8-bit bytes may be encoded as @[len]":"[string]","@.  Here
-- @[string]@ is the string and @[len]@ is a nonempty sequence of ASCII digits
-- giving the length of @[string]@ in decimal. The ASCII digits are @<30>@ for
-- 0, @<31>@ for 1, and so on up through @<39>@ for 9. Extra zeros at the front
-- of @[len]@ are prohibited: @[len]@ begins with @<30>@ exactly when
-- @[string]@ is empty.
--
-- For example, the string @"hello world!"@ is encoded as
-- @<31 32 3a 68 65 6c 6c 6f 20 77 6f 72 6c 64 21 2c>@,
-- i.e., @"12:hello world!,"@.
-- The empty string is encoded as @"0:,"@.
--
data NetstringFraming

instance FramingRender NetstringFraming where
    framingRender _ f = fmap $ \x ->
        let bs = f x
        in LBS8.pack (show (LBS8.length bs)) <> ":" <> bs <> ","

instance FramingUnrender NetstringFraming where
    framingUnrender _ f = transformWithAtto $ do
        len <- A8.decimal
        _ <- A8.char ':'
        bs <- A.take len
        _ <- A8.char ','
        either fail pure (f (LBS.fromStrict bs))
