{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}


{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.Internal.XhrClient where

import           Control.Arrow
import           Data.ByteString.Builder     (toLazyByteString)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BS
import           Data.CaseInsensitive
import           Data.Char
import           Data.Foldable               (toList)
import           Data.Functor.Alt            (Alt (..))
import           Data.Proxy                  (Proxy (..))
import qualified Data.Sequence           as Seq
import           Data.String.Conversions
import           Foreign.StablePtr
import           GHC.Generics
import           GHCJS.Foreign.Callback
import           GHCJS.Prim
import           GHCJS.Types
import           JavaScript.Web.Location
import           Network.HTTP.Types
import           Network.HTTP.Media (renderHeader)
import           Servant.Client.Core

newtype JSXMLHttpRequest = JSXMLHttpRequest JSVal

newtype JSXMLHttpRequestClass = JSXMLHttpRequestClass JSVal

newtype ClientEnv
   = ClientEnv
   { baseUrl :: BaseUrl }
   deriving (Eq, Show)

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

newtype ClientM a = ClientM
  { runClientM' :: ReaderT ClientEnv (ExceptT ServantError IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ServantError, MonadThrow
           , MonadCatch)

instance MonadBase IO ClientM where
  liftBase = ClientM . liftBase

instance MonadBaseControl IO ClientM where
  type StM ClientM a = Either ServantError a

  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . runClientM')))

  restoreM st = ClientM (restoreM st)

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  runRequest = performRequest
  throwServantError = throwError
  catchServantError = catchError

instance ClientLike (ClientM a) (ClientM a) where
  mkClient = id

runClientMOrigin :: ClientM a -> ClientEnv -> IO (Either ServantError a)
runClientMOrigin cm env = runExceptT $ flip runReaderT env $ runClientM' cm

runClientM :: ClientM a -> IO (Either ServantError a)
runClientM m = do
    curLoc <- getWindowLocation

    jsStr_protocol <- getProtocol curLoc
    jsStr_port     <- getPort     curLoc
    jsStr_hostname <- getHostname curLoc

    let protocol
          | jsStr_protocol == "https:" = Https
          | otherwise                  = Http

        portStr :: String
        portStr = fromJSString $ jsval jsStr_port

        port :: Int
        port | null portStr = case protocol of
                 Http  ->  80
                 Https -> 443
             | otherwise = read portStr

        hostname :: String
        hostname = fromJSString $ jsval jsStr_hostname

    runClientMOrigin m (ClientEnv (BaseUrl protocol hostname port ""))

performRequest :: Request -> ClientM Response
performRequest req = do
  xhr <- liftIO initXhr
  burl <- asks baseUrl
  liftIO $ performXhr xhr burl req
  resp <- toResponse xhr

  let status = statusCode (responseStatusCode resp)
  unless (status >= 200 && status < 300) $
        throwError $ FailureResponse resp

  pure resp

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
performXhr :: JSXMLHttpRequest -> BaseUrl -> Request -> IO ()
performXhr xhr burl request = do

    waiter <- newEmptyMVar

    bracket (acquire waiter) releaseCallback $ \_callback -> do
        t <- myThreadId
        s <- newStablePtr t

        openXhr xhr (cs $ requestMethod request) (toUrl burl request) True
        setHeaders xhr request
        sendXhr xhr (toBody request)
        takeMVar waiter

        freeStablePtr s
  where
    acquire waiter = onReadyStateChange xhr $ do
      state <- readyState xhr
      case state of
        -- onReadyStateChange's callback can fire state 4
        -- (which means "request finished and response is ready")
        -- multiple times. By using tryPutMVar, only the first time
        -- state 4 is fired will cause an MVar to be put. Subsequent
        -- fires are ignored.
        4 -> void $ tryPutMVar waiter ()
        _ -> return ()

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
openXhr xhr method url =
  js_openXhr xhr (toJSString method) (toJSString url)
foreign import javascript unsafe "$1.open($2, $3, $4)"
  js_openXhr :: JSXMLHttpRequest -> JSVal -> JSVal -> Bool -> IO ()

toUrl :: BaseUrl -> Request -> String
toUrl burl request =
  let pathS = cs $ toLazyByteString $ requestPath request
      queryS =
          cs $
          renderQuery True $
          toList $
          requestQueryString request
  in showBaseUrl burl ++ pathS ++ queryS

setHeaders :: JSXMLHttpRequest -> Request -> IO ()
setHeaders xhr request = do
  forM_ (toList $ requestAccept request) $ \mediaType ->
    js_setRequestHeader
      xhr
      (toJSString "Accept")
      (toJSString $ cs $ renderHeader mediaType)

  forM_ (requestBody request) $ \(_, mediaType) ->
    js_setRequestHeader
      xhr
      (toJSString "Content-Type")
      (toJSString $ cs $ renderHeader mediaType)

  forM_ (toList $ requestHeaders request) $ \(key, value) ->
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
  Nothing -> Nothing
  Just (RequestBodyLBS "", _) -> Nothing
  Just (RequestBodyLBS x, _) -> Just $ cs x

-- * inspecting the xhr response

-- This function is only supposed to handle 'ConnectionError's. Other
-- 'ServantError's are created in Servant.Client.Req.
toResponse :: JSXMLHttpRequest -> ClientM Response
toResponse xhr = do
  status <- liftIO $ getStatus xhr
  case status of
    0 -> throwError $ ConnectionError "connection error"
    _ -> liftIO $ do
      statusText <- cs <$> getStatusText xhr
      headers <- parseHeaders <$> getAllResponseHeaders xhr
      responseText <- cs <$> getResponseText xhr
      pure Response
        { responseStatusCode = mkStatus status statusText
        , responseBody = responseText
        , responseHeaders = Seq.fromList headers
        , responseHttpVersion = http11 -- this is made up
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
  (first mk . first strip . second strip . parseHeader) <$>
  splitOn "\r\n" (cs s)
  where
    parseHeader :: BS.ByteString -> (BS.ByteString, BS.ByteString)
    parseHeader h = case BS.breakSubstring ":" (cs h) of
      (key, BS.drop 1 -> value) -> (key, value)

    splitOn :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
    splitOn separator input = case BS.breakSubstring separator input of
      (prefix, "") -> [prefix]
      (prefix, rest) -> prefix : splitOn separator (BS.drop (BS.length separator) rest)

    strip :: BS.ByteString -> BS.ByteString
    strip = BS.dropWhile isSpace . BS.reverse . BS.dropWhile isSpace . BS.reverse
