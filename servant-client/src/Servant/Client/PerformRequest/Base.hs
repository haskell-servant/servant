{-# LANGUAGE DeriveDataTypeable #-}

module Servant.Client.PerformRequest.Base where

import           Control.Exception
import           Data.ByteString.Lazy
import           Data.Typeable
import           Network.HTTP.Media
import           Network.HTTP.Types

data ServantError
  = FailureResponse
    { responseStatus            :: Status
    , responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | DecodeFailure
    { decodeError               :: String
    , responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | UnsupportedContentType
    { responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | InvalidContentTypeHeader
    { responseContentTypeHeader :: ByteString
    , responseBody              :: ByteString
    }
  | ConnectionError
    { connectionError           :: SomeException
    }
  deriving (Show, Typeable)

instance Eq ServantError where
  FailureResponse a b c == FailureResponse x y z =
    (a, b, c) == (x, y, z)
  DecodeFailure a b c == DecodeFailure x y z =
    (a, b, c) == (x, y, z)
  UnsupportedContentType a b == UnsupportedContentType x y =
    (a, b) == (x, y)
  InvalidContentTypeHeader a b == InvalidContentTypeHeader x y =
    (a, b) == (x, y)
  ConnectionError a == ConnectionError x =
    show a == show x
  _ == _ = False

instance Exception ServantError
