{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Client.JsSpec where

import           Servant.API
import           Servant.Server
import           Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString as B
import           Data.ByteString(ByteString)
import           Test.Hspec
import           Data.Proxy
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Word
import           GHC.Generics
import qualified Language.Javascript.JSaddle.WebKitGTK as WK
import qualified Language.Javascript.JSaddle.Monad as JSaddle
import           Language.Javascript.JSaddle.Monad(JSM)
import           Control.Concurrent
import           Servant.Client.Js
import qualified JSDOM
import qualified JSDOM.Window as Window
import qualified Network.Wai as Wai
import           Network.Wai.Middleware.AddHeaders
import qualified Network.HTTP.Types as Http
import           Data.String
import           System.IO

type TestApi = ReqBody '[OctetStream] ByteString :> Post '[JSON] TestResponse
testApi :: Proxy TestApi
testApi = Proxy

data TestResponse = TestResponse { byteList :: [Word8] }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

testServer :: Server TestApi
testServer x = do
  liftIO $ putStrLn "Hello tehre"
  liftIO $ print x
  liftIO $ hFlush stdout
  pure . TestResponse . B.unpack $ x

testClient :: Client ClientM TestApi
testClient = client testApi

-- WARNING: approximation!
jsaddleFinally :: JSM b -> JSM a -> JSM a
jsaddleFinally handler m = JSaddle.bracket (pure ()) (const handler) (const m)
-- jsaddleFinally handler m = JSaddle.catch (m <* handler) (\e -> handler >> throw (e :: SomeException))

close :: JSM ()
close = do
  mw <- JSDOM.currentWindow
  case mw of
    Just w -> do
      liftIO $ putStrLn "Closing window..."
      Window.close w
    Nothing -> liftIO $ putStrLn "Can't close the window!"

logRequest :: Wai.Middleware
logRequest app request respond = do
  putStrLn "Request"
  print request
  app request (\response -> do
                  putStrLn "Response Headers"
                  print `mapM_` (Wai.responseHeaders response)
                  respond response)

corsHeaders :: (IsString s1, IsString s2) => [(s1, s2)]
corsHeaders = [ ("Access-Control-Allow-Origin", "null")
              , ("Access-Control-Allow-Methods", "POST")
              , ("Access-Control-Allow-Headers", "content-type")
              ]

addCors :: Wai.Middleware
addCors app request respond =
  if Wai.requestMethod request == "OPTIONS"
  then respond $ Wai.responseLBS Http.status200 corsHeaders ""
  else addHeaders corsHeaders app request respond


spec :: Spec
spec = do
  describe "Servant.Client.Js" $ do
    it "Receive a properly encoded response" $ do
      Warp.testWithApplication (pure $ logRequest $ addCors $ serve testApi testServer) $ \portNr -> do
        let clientEnv = mkClientEnv BaseUrl { baseUrlScheme = Http
                                            , baseUrlHost = "localhost"
                                            , baseUrlPort = fromIntegral portNr
                                            , baseUrlPath = "/"
                                            }
        
        WK.run $ JSaddle.liftJSM $ jsaddleFinally close $ do
          liftIO $ threadDelay $ 1000 * 1000
          -- a mix of valid utf-8 and non-utf8 bytes
          let bytes = [0x01, 0xff, 0x02, 0xfe, 0x03, 0xfd, 0x00, 0x64, 0xc3, 0xbb, 0x68, 0xc3]
          response <- flip runClientM clientEnv $ do
            testClient (B.pack bytes)
          liftIO $ response `shouldBe` Right (TestResponse bytes)
