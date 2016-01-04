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

instance Exception ServantError
