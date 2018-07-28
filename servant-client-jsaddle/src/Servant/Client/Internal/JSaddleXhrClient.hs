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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.Internal.JSaddleXhrClient where

import           Control.Arrow
import           Data.ByteString.Builder     (toLazyByteString)
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import           Data.CaseInsensitive
import           Data.Char
import           Data.Foldable               (toList)
import           Data.Functor.Alt            (Alt (..))
import           Data.Proxy                  (Proxy (..))
import qualified Data.Sequence           as Seq
import           Data.String.Conversions
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           GHC.Generics
import qualified JSDOM.Types                     as JS
import qualified JSDOM.Custom.XMLHttpRequest     as JS
import qualified JSDOM.Generated.Window          as Window
import qualified JSDOM.Generated.Location        as Location
import qualified JSDOM
import           JSDOM.Types (DOM, askDOM, runDOM, DOMContext)
import qualified JSDOM.EventM as JSDOM
import qualified Language.Javascript.JSaddle.Types as JSaddle
import qualified JavaScript.TypedArray.ArrayBuffer as ArrayBuffer
import qualified GHCJS.Buffer as Buffer
import           Network.HTTP.Types
import           Network.HTTP.Media (renderHeader)
import           Servant.Client.Core

-- Note: assuming encoding UTF-8

data ClientEnv
   = ClientEnv
   { baseUrl :: BaseUrl
     -- | Modify the XMLHttpRequest at will, right before sending.
   , fixUpXhr :: JS.XMLHttpRequest -> DOM ()
   }

-- | Default 'ClientEnv'
mkClientEnv :: BaseUrl -> ClientEnv
mkClientEnv burl = ClientEnv burl (const (pure ()))

instance Show ClientEnv where
  showsPrec prec (ClientEnv burl _) =
    showParen (prec >= 11)
      ( showString "ClientEnv {"
      . showString "baseUrl = "
      . showsPrec 0 burl
      . showString ", fixUpXhr = <function>"
      . showString "}"
      )

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

newtype ClientM a = ClientM
  { fromClientM :: ReaderT ClientEnv (ExceptT ServantError DOM) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ServantError)
deriving instance MonadThrow DOM => MonadThrow ClientM
deriving instance MonadCatch DOM => MonadCatch ClientM

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  throwServantError = throwError
  runRequest r = do
    d <- ClientM askDOM
    performRequest d r

instance ClientLike (ClientM a) (ClientM a) where
  mkClient = id

runClientM :: ClientM a -> ClientEnv -> DOM (Either ServantError a)
runClientM cm env = runExceptT $ flip runReaderT env $ fromClientM cm

runClientM' :: ClientM a -> DOM (Either ServantError a)
runClientM' m = do
    burl <- getDefaultBaseUrl
    runClientM m (mkClientEnv burl)

getDefaultBaseUrl :: DOM BaseUrl
getDefaultBaseUrl = do
    win <- JSDOM.currentWindow >>= \mw -> case mw of
      Just x -> pure x
      Nothing -> fail "Can not determine default base url without window."
    curLoc <- Window.getLocation win

    protocolStr  <- Location.getProtocol curLoc
    portStr      <- Location.getPort     curLoc
    hostname     <- Location.getHostname curLoc

    let protocol
          | (protocolStr :: JS.JSString) == "https:"
                        = Https
          | otherwise   = Http

        port :: Int
        port | null portStr = case protocol of
                 Http  ->  80
                 Https -> 443
             | otherwise = read portStr

    pure (BaseUrl protocol hostname port "")

performRequest :: DOMContext -> Request -> ClientM Response
performRequest domc req = do
  xhr <- JS.newXMLHttpRequest `runDOM` domc
  burl <- asks baseUrl
  fixUp <- asks fixUpXhr
  performXhr xhr burl req fixUp `runDOM` domc
  resp <- toResponse domc xhr

  let status = statusCode (responseStatusCode resp)
  unless (status >= 200 && status < 300) $
        throwError $ FailureResponse resp

  pure resp

-- * performing requests
-- Performs the xhr and blocks until the response was received
performXhr :: JS.XMLHttpRequest -> BaseUrl -> Request -> (JS.XMLHttpRequest -> DOM ()) -> DOM ()
performXhr xhr burl request fixUp = do

    let username, password :: Maybe JS.JSString
        username = Nothing; password = Nothing

    JS.open xhr (decodeUtf8Lenient $ requestMethod request) (toUrl burl request) True username password
    setHeaders xhr request
    fixUp xhr
  
    waiter <- liftIO $ newEmptyMVar

    cleanup <- JSDOM.on xhr JS.readyStateChange $ do
      state <- JS.getReadyState xhr
      case state of
        -- onReadyStateChange's callback can fire state 4
        -- (which means "request finished and response is ready")
        -- multiple times. By using tryPutMVar, only the first time
        -- state 4 is fired will cause an MVar to be put. Subsequent
        -- fires are ignored.
        4 -> void $ liftIO $ tryPutMVar waiter ()
        _ -> return ()

    sendXhr xhr (toBody request)
    
    liftIO $ takeMVar waiter
    
    cleanup
    
toUrl :: BaseUrl -> Request -> JS.JSString
toUrl burl request =
  let pathS = JS.toJSString $ decodeUtf8Lenient $ L.toStrict $ toLazyByteString $
              requestPath request
      queryS =
          JS.toJSString $ decodeUtf8Lenient $
          renderQuery True $
          toList $
          requestQueryString request
  in JS.toJSString (showBaseUrl burl) <> pathS <> queryS :: JS.JSString

setHeaders :: JS.XMLHttpRequest -> Request -> DOM ()
setHeaders xhr request = do
  forM_ (toList $ requestAccept request) $ \mediaType -> -- FIXME review
    JS.setRequestHeader
      xhr
      ("Accept" :: JS.JSString)
      (decodeUtf8Lenient $ renderHeader mediaType)

  forM_ (requestBody request) $ \(_, mediaType) ->
    JS.setRequestHeader
      xhr
      ("Content-Type" :: JS.JSString)
      (decodeUtf8Lenient $ renderHeader mediaType)

  forM_ (toList $ requestHeaders request) $ \(key, value) ->
    JS.setRequestHeader xhr (decodeUtf8Lenient $ original key) (decodeUtf8Lenient value)

-- ArrayBufferView is a type that only exists in the spec and covers many concrete types.
castMutableArrayBufferToArrayBufferView :: ArrayBuffer.MutableArrayBuffer -> DOM JS.ArrayBufferView
castMutableArrayBufferToArrayBufferView x = JS.liftJSM $ do
  JS.fromJSValUnchecked $ JS.pToJSVal x

sendXhr :: JS.XMLHttpRequest -> Maybe L.ByteString -> DOM ()
sendXhr xhr Nothing = JS.send xhr
sendXhr xhr (Just body) = do
  -- Reason for copy: hopefully offset will be 0 and length b == len
  -- FIXME: use a typed array constructor that accepts offset and length and skip the copy
  (b, _offset, _len) <- JSaddle.ghcjsPure $ Buffer.fromByteString $ BS.copy $ L.toStrict body
  b' <- Buffer.thaw b
  b'' <- JSaddle.ghcjsPure $ Buffer.getArrayBuffer b'
  JS.sendArrayBuffer xhr =<< castMutableArrayBufferToArrayBufferView b''

toBody :: Request -> Maybe L.ByteString
toBody request = case requestBody request of
  Nothing -> Nothing
  Just (RequestBodyLBS "", _) -> Nothing
  Just (RequestBodyLBS x, _) -> Just x

-- * inspecting the xhr response

-- This function is only supposed to handle 'ConnectionError's. Other
-- 'ServantError's are created in Servant.Client.Req.
toResponse :: DOMContext -> JS.XMLHttpRequest -> ClientM Response
toResponse domc xhr = do
  let inDom :: DOM a -> ClientM a
      inDom = flip runDOM domc
  status <- inDom $ JS.getStatus xhr
  case status of
    0 -> throwError $ ConnectionError "connection error"
    _ -> inDom $ do
      statusText <- BS.pack <$> JS.getStatusText xhr
      headers <- parseHeaders <$> JS.getAllResponseHeaders xhr
      responseText <- maybe "" (L.fromStrict . BS.pack) <$> JS.getResponseText xhr -- FIXME: Text/Binary? Performance? Test?
      pure Response
        { responseStatusCode = mkStatus (fromIntegral status) statusText
        , responseBody = responseText
        , responseHeaders = Seq.fromList headers
        , responseHttpVersion = http11 -- this is made up
        }

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

decodeUtf8Lenient :: BS.ByteString -> JS.JSString
decodeUtf8Lenient = JS.toJSString . T.decodeUtf8With T.lenientDecode
