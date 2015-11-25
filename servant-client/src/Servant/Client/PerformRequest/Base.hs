{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.PerformRequest.Base where

import           Control.Arrow
import           Control.Exception
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy
import           Data.CaseInsensitive
import           Data.Char
import           Data.String.Conversions
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

parseHeaders :: String -> ResponseHeaders
parseHeaders s =
  fmap (first mk) $
  fmap (first strip . second strip) $
  fmap parseHeader $
  splitOn "\r\n" (cs s)
  where
    parseHeader :: BS.ByteString -> (BS.ByteString, BS.ByteString)
    parseHeader h = case BS.breakSubstring ":" (cs h) of
      (key, (BS.drop 1 -> value)) -> (key, value)

    splitOn :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
    splitOn separator input = case BS.breakSubstring separator input of
      (prefix, "") -> [prefix]
      (prefix, rest) -> prefix : splitOn separator (BS.drop (BS.length separator) rest)

    strip :: BS.ByteString -> BS.ByteString
    strip = BS.dropWhile isSpace . BS.reverse . BS.dropWhile isSpace . BS.reverse
