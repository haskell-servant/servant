{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# OPTIONS_HADDOCK not-home          #-}

module Servant.API.Stream where

import           Data.ByteString.Lazy  (ByteString, empty)
import           Data.Proxy            (Proxy)
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic)

-- | A stream endpoint for a given method emits a stream of encoded values at a given Content-Type, delimited by a framing strategy.
data Stream (method :: k1) (framing :: *) (contentType :: *) a
  deriving (Typeable, Generic)

-- | Stream endpoints may be implemented as producing a @StreamGenerator@ -- a function that itself takes two emit functions -- the first to be used on the first value the stream emits, and the second to be used on all subsequent values (to allow interspersed framing strategies such as comma separation).
data StreamGenerator a =  StreamGenerator ((a -> IO ()) -> (a -> IO ()) -> IO ())

-- | The Framing class provides the logic for each framing strategy. The strategy emits a header, followed by boundary-delimited data, and finally a termination character. For many strategies, some of these will just be empty bytestrings.
class Framing strategy a where
   header    :: Proxy strategy -> Proxy a -> ByteString
   boundary  :: Proxy strategy -> Proxy a -> BoundaryStrategy
   terminate :: Proxy strategy -> Proxy a -> ByteString

-- | The bracketing strategy generates things to precede and follow the content, as with netstrings.
-- | The intersperse strategy inserts seperators between things, as with newline framing.
data BoundaryStrategy = BoundaryStrategyBracket (ByteString -> (ByteString,ByteString))
                      | BoundaryStrategyIntersperse ByteString


-- | A simple framing strategy that has no header or termination, and inserts a newline character between each frame.
data NewlineFraming

instance Framing NewlineFraming a where
         header    _ _ = empty
         boundary  _ _ = BoundaryStrategyIntersperse "\n"
         terminate _ _ = empty
