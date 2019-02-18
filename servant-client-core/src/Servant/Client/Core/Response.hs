{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Client.Core.Response (
    Response,
    StreamingResponse,
    ResponseF (..),
    ) where

import           Prelude ()
import           Prelude.Compat

import           Control.DeepSeq
                 (NFData (..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence        as Seq
import           Data.Typeable
                 (Typeable)
import           GHC.Generics
                 (Generic)
import           Network.HTTP.Types
                 (Header, HttpVersion (..), Status (..))

import           Servant.API.Stream
                 (SourceIO)

data ResponseF a = Response
  { responseStatusCode  :: Status
  , responseHeaders     :: Seq.Seq Header
  , responseHttpVersion :: HttpVersion
  , responseBody        :: a
  } deriving (Eq, Show, Generic, Typeable, Functor, Foldable, Traversable)

instance NFData a => NFData (ResponseF a) where
    rnf (Response sc hs hv body) =
        rnfStatus sc `seq`
        rnf hs `seq`
        rnfHttpVersion hv `seq`
        rnf body
      where
        rnfStatus (Status code msg) = rnf code `seq` rnf msg
        rnfHttpVersion (HttpVersion _ _) = () -- HttpVersion fields are strict

type Response = ResponseF LBS.ByteString
type StreamingResponse = ResponseF (SourceIO BS.ByteString)
