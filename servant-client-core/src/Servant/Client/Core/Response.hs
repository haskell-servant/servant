{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Client.Core.Response
  ( Response
  , StreamingResponse
  , ResponseF (..)
  , responseToInternalResponse
  )
where

import Control.DeepSeq (NFData (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Types (Header, HttpVersion (..), Status (..))
import Prelude.Compat
import Servant.API.Stream (SourceIO)
import Servant.Types.Internal.Response
import Prelude ()

data ResponseF a = Response
  { responseStatusCode :: Status
  , responseHeaders :: Seq.Seq Header
  , responseHttpVersion :: HttpVersion
  , responseBody :: a
  }
  deriving (Eq, Foldable, Functor, Generic, Show, Traversable, Typeable)

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

responseToInternalResponse :: ResponseF a -> InternalResponse a
responseToInternalResponse Response{responseStatusCode, responseHeaders, responseBody} =
  InternalResponse responseStatusCode responseHeaders responseBody
