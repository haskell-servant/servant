{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Client.JSaddleSpec where

import           Control.Concurrent
                 (threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString
                 (ByteString)
import qualified Data.ByteString                        as B
import           Data.Proxy
import           Data.String
import           Data.Word
import           GHC.Generics
import qualified GHCJS.DOM
import qualified GHCJS.DOM.Window                       as Window
import           Language.Javascript.JSaddle.Monad
                 (JSM)
import qualified Language.Javascript.JSaddle.Monad      as JSaddle
import qualified Language.Javascript.JSaddle.Run        as Run
import qualified Language.Javascript.JSaddle.WebSockets as WS
import qualified Network.HTTP.Types                     as Http
import qualified Network.Wai                            as Wai
import           Network.Wai.Handler.Warp               as Warp
import qualified System.Process as P
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.Cors
                 (simpleCors)
import           Network.WebSockets
                 (defaultConnectionOptions)
import           Servant.API
import           Servant.Client.JSaddle
import           Servant.Server
import           Test.Hspec

type TestApi = ReqBody '[OctetStream] ByteString :> Post '[JSON] TestResponse
testApi :: Proxy TestApi
testApi = Proxy

data TestResponse = TestResponse { byteList :: [Word8] }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

testServer :: Server TestApi
testServer x = do
  pure . TestResponse . B.unpack $ x

testClient :: Client ClientM TestApi
testClient = client testApi

-- WARNING: approximation!
jsaddleFinally :: JSM b -> JSM a -> JSM a
jsaddleFinally handler m = JSaddle.bracket (pure ()) (const handler) (const m)
-- jsaddleFinally handler m = JSaddle.catch (m <* handler) (\e -> handler >> throw (e :: SomeException))

close :: JSM ()
close = do
  mw <- GHCJS.DOM.currentWindow
  case mw of
    Just w -> do
      liftIO $ putStrLn "Closing window..."
      Window.close w
    Nothing -> liftIO $ putStrLn "Can't close the window!"



spec :: Spec
spec = do
  describe "Servant.Client.JSaddle" $ do
    it "Receive a properly encoded response" $ do
      -- A mvar to tell promptly when we are done
      done <- newEmptyMVar

      -- How this work:
      --
      -- 1. we start server warp, which serves simple API
      -- 2. we start client warp, which serves jsaddle running the 'action'
      -- 3. we run google-chrome-stable to open jsaddle page and to run the test

      let action :: Int -> JSM ()
          action serverPort = do
              liftIO $ threadDelay $ 500 * 1000
              -- a mix of valid utf-8 and non-utf8 bytes
              let bytes = [0x01, 0xff, 0x02, 0xfe, 0x03, 0xfd, 0x00, 0x64, 0xc3, 0xbb, 0x68, 0xc3]
              response <- flip runClientM clientEnv $ testClient (B.pack bytes)
              liftIO $ print response
              liftIO $ response `shouldBe` Right (TestResponse bytes)

              -- we are done.
              liftIO $ putMVar done ()
            where
              clientEnv = mkClientEnv BaseUrl
                  { baseUrlScheme = Http
                  , baseUrlHost   = "localhost"
                  , baseUrlPort   = fromIntegral serverPort
                  , baseUrlPath   = "/"
                  }

      let serverApp :: IO Application
          serverApp = pure $ logRequest $ addCors $ serve testApi testServer

      Warp.testWithApplication serverApp $ \serverPort -> do

          let clientApp :: IO Application
              clientApp = WS.jsaddleOr defaultConnectionOptions (action serverPort >> Run.syncPoint) WS.jsaddleApp

          Warp.testWithApplication (simpleCors <$> clientApp) $ \clientPort -> do
              putStrLn $ "server http://localhost:" ++ show serverPort
              putStrLn $ "client http://localhost:" ++ show clientPort
              putStrLn $ "google-chrome-stable --headless --disable-gpu --screenshot http://localhost:" ++ show clientPort

              -- threadDelay $ 1000 * 1000 * 1000

              -- Run headless chrome
              -- https://docs.travis-ci.com/user/gui-and-headless-browsers/#using-the-chrome-addon-in-the-headless-mode
              -- https://developers.google.com/web/updates/2017/04/headless-chrome
              hdl <- P.spawnProcess "google-chrome-stable"
                  [ "--headless"
                  , "--disable-gpu"
                  , "--remote-debugging-port=9222" -- TODO: bind to random port
                  , "http://localhost:" ++ show clientPort
                  ]

              -- wait for test to run.
              takeMVar done

              -- kill chrome
              P.terminateProcess hdl

-------------------------------------------------------------------------------
-- Logger middleware
-------------------------------------------------------------------------------

logRequest :: Wai.Middleware
logRequest app request respond = do
    putStrLn "Request"
    print request
    app request $ \response -> do
        putStrLn "Response Headers"
        mapM_ print (Wai.responseHeaders response)
        respond response

-------------------------------------------------------------------------------
-- OPTIONS
-------------------------------------------------------------------------------

corsHeaders :: (IsString s1, IsString s2) => [(s1, s2)]
corsHeaders =
    [ ("Access-Control-Allow-Origin", "*")
    , ("Access-Control-Allow-Methods", "POST")
    , ("Access-Control-Allow-Headers", "content-type")
    ]

addCors :: Wai.Middleware
addCors app request respond =
    if Wai.requestMethod request == "OPTIONS"
    then respond $ Wai.responseLBS Http.status200 corsHeaders ""
    else addHeaders corsHeaders app request respond
