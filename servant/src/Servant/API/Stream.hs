{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TupleSections            #-}
{-# OPTIONS_HADDOCK not-home          #-}

module Servant.API.Stream where

import           Data.ByteString.Lazy        (ByteString, empty)
import qualified Data.ByteString.Lazy.Char8  as LB
import           Data.Monoid                 ((<>))
import           Data.Proxy                  (Proxy)
import           Data.Typeable               (Typeable)
import           GHC.Generics                (Generic)
import           Text.Read                   (readMaybe)
import           Control.Arrow               (first)
import           Network.HTTP.Types.Method   (StdMethod (..))

-- | A Stream endpoint for a given method emits a stream of encoded values at a given Content-Type, delimited by a framing strategy. Steam endpoints always return response code 200 on success. Type synonyms are provided for standard methods.
data Stream (method :: k1) (framing :: *) (contentType :: *) (a :: *)
  deriving (Typeable, Generic)

type StreamGet  = Stream 'GET
type StreamPost = Stream 'POST

-- | Stream endpoints may be implemented as producing a @StreamGenerator@ -- a function that itself takes two emit functions -- the first to be used on the first value the stream emits, and the second to be used on all subsequent values (to allow interspersed framing strategies such as comma separation).
newtype StreamGenerator a =  StreamGenerator {getStreamGenerator :: (a -> IO ()) -> (a -> IO ()) -> IO ()}

-- | ToStreamGenerator is intended to be implemented for types such as Conduit, Pipe, etc. By implementing this class, all such streaming abstractions can be used directly as endpoints.
class ToStreamGenerator f a where
   toStreamGenerator :: f a -> StreamGenerator a

instance ToStreamGenerator StreamGenerator a
   where toStreamGenerator x = x

-- | Clients reading from streaming endpoints can be implemented as producing a @ResultStream@ that captures the setup, takedown, and incremental logic for a read, being an IO continuation that takes a producer of Just either values or errors that terminates with a Nothing.
newtype ResultStream a = ResultStream (forall b. (IO (Maybe (Either String a)) -> IO b) -> IO b)

-- | BuildFromStream is intended to be implemented for types such as Conduit, Pipe, etc. By implementing this class, all such streaming abstractions can be used directly on the client side for talking to streaming endpoints.
class BuildFromStream a b where
   buildFromStream :: ResultStream a -> b

instance BuildFromStream a (ResultStream a)
   where buildFromStream x = x

-- | The FramingRender class provides the logic for emitting a framing strategy. The strategy emits a header, followed by boundary-delimited data, and finally a termination character. For many strategies, some of these will just be empty bytestrings.
class FramingRender strategy a where
   header   :: Proxy strategy -> Proxy a -> ByteString
   boundary :: Proxy strategy -> Proxy a -> BoundaryStrategy
   trailer  :: Proxy strategy -> Proxy a -> ByteString

-- | The bracketing strategy generates things to precede and follow the content, as with netstrings.
--   The intersperse strategy inserts seperators between things, as with newline framing.
--   Finally, the general strategy performs an arbitrary rewrite on the content, to allow escaping rules and such.
data BoundaryStrategy = BoundaryStrategyBracket (ByteString -> (ByteString,ByteString))
                      | BoundaryStrategyIntersperse ByteString
                      | BoundaryStrategyGeneral (ByteString -> ByteString)

-- | A type of parser that can never fail, and has different parsing strategies (incremental, or EOF) depending if more input can be sent. The incremental parser should return `Nothing` if it would like to be sent a longer ByteString. If it returns a value, it also returns the remainder following that value.
data ByteStringParser a = ByteStringParser {
  parseIncremental :: ByteString -> Maybe (a, ByteString),
  parseEOF :: ByteString -> (a, ByteString)
}

-- | The FramingUnrender class provides the logic for parsing a framing strategy. The outer @ByteStringParser@ strips the header from a stream of bytes, and yields a parser that can handle the remainder, stepwise. Each frame may be a ByteString, or a String indicating the error state for that frame. Such states are per-frame, so that protocols that can resume after errors are able to do so. Eventually this returns an empty ByteString to indicate termination.
class FramingUnrender strategy a where
   unrenderFrames :: Proxy strategy -> Proxy a -> ByteStringParser (ByteStringParser (Either String ByteString))


-- | A simple framing strategy that has no header or termination, and inserts a newline character between each frame.
--   This assumes that it is used with a Content-Type that encodes without newlines (e.g. JSON).
data NewlineFraming

instance FramingRender NewlineFraming a where
   header    _ _ = empty
   boundary  _ _ = BoundaryStrategyIntersperse "\n"
   trailer   _ _ = empty

instance FramingUnrender NewlineFraming a where
   unrenderFrames _ _ = ByteStringParser (Just . (go,)) (go,)
         where go = ByteStringParser
                        (\x -> case LB.break (== '\n') x of
                              (h,r) -> if not (LB.null r) then Just (Right h, LB.drop 1 r) else Nothing
                        )
                        (\x -> case LB.break (== '\n') x of
                              (h,r) -> (Right h, LB.drop 1 r)
                        )
-- | The netstring framing strategy as defined by djb: <http://cr.yp.to/proto/netstrings.txt>
data NetstringFraming

instance FramingRender NetstringFraming a where
   header    _ _ = empty
   boundary  _ _ = BoundaryStrategyBracket $ \b -> ((<> ":") . LB.pack . show . LB.length $ b, ",")
   trailer   _ _ = empty


instance FramingUnrender NetstringFraming a where
   unrenderFrames _ _ = ByteStringParser (Just . (go,)) (go,)
       where go = ByteStringParser
                 (\b -> let (i,r) = LB.break (==':') b
                        in case readMaybe (LB.unpack i) of
                             Just len -> if LB.length r > len
                                         then Just . first Right . fmap (LB.drop 1) $ LB.splitAt len . LB.drop 1 $ r
                                         else Nothing
                             Nothing -> Just (Left ("Bad netstring frame, couldn't parse value as integer value: " ++ LB.unpack i), LB.drop 1 . LB.dropWhile (/= ',') $ r))
                 (\b -> let (i,r) = LB.break (==':') b
                        in case readMaybe (LB.unpack i) of
                             Just len -> if LB.length r > len
                                         then first Right . fmap (LB.drop 1) $ LB.splitAt len . LB.drop 1 $ r
                                         else (Right $ LB.take len r, LB.empty)
                             Nothing -> (Left ("Bad netstring frame, couldn't parse value as integer value: " ++ LB.unpack i), LB.drop 1 . LB.dropWhile (/= ',') $ r))
