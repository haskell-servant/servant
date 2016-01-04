{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.PerformRequest.GHCJS (
  ServantError(..),
  performHttpRequest,

  -- exported for testing
  parseHeaders,
  ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import           Data.Char
import           Data.String.Conversions
import           GHCJS.Foreign.Callback
import           GHCJS.Prim
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal as HttpClient
import           Network.HTTP.Types

import           Servant.Client.PerformRequest.Base

newtype JSXMLHttpRequest = JSXMLHttpRequest JSVal

newtype JSXMLHttpRequestClass = JSXMLHttpRequestClass JSVal

performHttpRequest :: Manager -> Request -> IO (Either ServantError (Response LBS.ByteString))
performHttpRequest _ request = do
  xhr <- initXhr
  performXhr xhr request
  toResponse xhr

-- * initialization

initXhr :: IO JSXMLHttpRequest
initXhr = do
  lib <- requireXMLHttpRequestClass
  newXMLHttpRequest lib

foreign import javascript unsafe
  -- branching between node (for testing) and browsers
  "(function () {if (typeof require !== 'undefined') { return require('xhr2'); } else { return XMLHttpRequest; };})()"
  requireXMLHttpRequestClass :: IO JSXMLHttpRequestClass

foreign import javascript unsafe "new $1()"
  newXMLHttpRequest :: JSXMLHttpRequestClass -> IO JSXMLHttpRequest

-- * performing requests

-- Performs the xhr and blocks until the response was received
performXhr :: JSXMLHttpRequest -> Request -> IO ()
performXhr xhr request = do
  waiter <- newEmptyMVar
  callback <- onReadyStateChange xhr $ do
    state <- readyState xhr
    case state of
      4 -> putMVar waiter ()
      _ -> return ()
  openXhr xhr (cs $ method request) (toUrl request) True
  setHeaders xhr (requestHeaders request)
  sendXhr xhr (toBody request)
  takeMVar waiter
  releaseCallback callback

onReadyStateChange :: JSXMLHttpRequest -> IO () -> IO (Callback (IO ()))
onReadyStateChange xhr action = do
  callback <- asyncCallback action
  js_onReadyStateChange xhr callback
  return callback
foreign import javascript safe "$1.onreadystatechange = $2;"
  js_onReadyStateChange :: JSXMLHttpRequest -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.readyState"
  readyState :: JSXMLHttpRequest -> IO Int

openXhr :: JSXMLHttpRequest -> String -> String -> Bool -> IO ()
openXhr xhr method url async =
  js_openXhr xhr (toJSString method) (toJSString url) async
foreign import javascript unsafe "$1.open($2, $3, $4)"
  js_openXhr :: JSXMLHttpRequest -> JSVal -> JSVal -> Bool -> IO ()

toUrl :: Request -> String
toUrl request =
  let protocol = if secure request then "https" else "http"
      hostS = cs $ host request
      portS = show $ port request
      pathS = cs $ path request
      queryS = cs $ queryString request
  in protocol ++ "://" ++ hostS ++ ":" ++ portS ++ pathS ++ queryS

setHeaders :: JSXMLHttpRequest -> RequestHeaders -> IO ()
setHeaders xhr headers = forM_ headers $ \ (key, value) ->
  js_setRequestHeader xhr (toJSString $ cs $ original key) (toJSString $ cs value)
foreign import javascript unsafe "$1.setRequestHeader($2, $3)"
  js_setRequestHeader :: JSXMLHttpRequest -> JSVal -> JSVal -> IO ()

sendXhr :: JSXMLHttpRequest -> Maybe String -> IO ()
sendXhr xhr Nothing = js_sendXhr xhr
sendXhr xhr (Just body) =
  js_sendXhrWithBody xhr (toJSString body)
foreign import javascript unsafe "$1.send()"
  js_sendXhr :: JSXMLHttpRequest -> IO ()
foreign import javascript unsafe "$1.send($2)"
  js_sendXhrWithBody :: JSXMLHttpRequest -> JSVal -> IO ()

toBody :: Request -> Maybe String
toBody request = case requestBody request of
  RequestBodyLBS "" -> Nothing
  RequestBodyLBS x -> Just $ cs x
  _ -> error "servant-client only uses RequestBodyLBS"

-- * inspecting the xhr response

-- This function is only supposed to handle 'ConnectionError's. Other
-- 'ServantError's are created in Servant.Client.Req.
toResponse :: JSXMLHttpRequest -> IO (Either ServantError (Response LBS.ByteString))
toResponse xhr = do
  status <- getStatus xhr
  case status of
    0 -> return $ Left $ ConnectionError $ SomeException $ ErrorCall "connection error"
    _ -> do
      statusText <- cs <$> getStatusText xhr
      headers <- parseHeaders <$> getAllResponseHeaders xhr
      responseText <- cs <$> getResponseText xhr
      return $ Right $ Response {
        HttpClient.responseStatus = mkStatus status statusText,
        responseVersion = http11, -- this is made up
        responseHeaders = headers,
        HttpClient.responseBody = responseText,
        responseCookieJar = mempty,
        responseClose' = ResponseClose (return ())
      }

foreign import javascript unsafe "$1.status"
  getStatus :: JSXMLHttpRequest -> IO Int

getStatusText :: JSXMLHttpRequest -> IO String
getStatusText = fmap fromJSString . js_statusText
foreign import javascript unsafe "$1.statusText"
  js_statusText :: JSXMLHttpRequest -> IO JSVal

getAllResponseHeaders :: JSXMLHttpRequest -> IO String
getAllResponseHeaders xhr =
  fromJSString <$> js_getAllResponseHeaders xhr
foreign import javascript unsafe "$1.getAllResponseHeaders()"
  js_getAllResponseHeaders :: JSXMLHttpRequest -> IO JSVal

getResponseText :: JSXMLHttpRequest -> IO String
getResponseText xhr = fromJSString <$> js_responseText xhr
foreign import javascript unsafe "$1.responseText"
  js_responseText :: JSXMLHttpRequest -> IO JSVal

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
