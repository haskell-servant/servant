{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent
                 (threadDelay)
import           Control.Monad.IO.Class
                 (MonadIO (..))
import qualified Data.ByteString          as BS
import           Data.Maybe
                 (fromMaybe)
import           Network.HTTP.Client
                 (defaultManagerSettings, newManager)
import           System.Environment
                 (getArgs, lookupEnv)
import           System.IO
                 (IOMode (..), openFile, hClose)
import           Text.Read
                 (readMaybe)

import qualified System.IO.Streams        as IOS
import           System.IO.Streams.Combinators
                 (atEndOfInput)
import           System.IO.Streams.Handle
                 (handleToInputStream)
import           Servant
import           Servant.Client.Streaming
import           Servant.IO.Streams ()

import qualified Network.Wai.Handler.Warp as Warp

type FastAPI = "get" :> Capture "num" Int :> StreamGet NewlineFraming JSON (IOS.InputStream Int)

type API = FastAPI
    :<|> "slow" :> Capture "num" Int :> StreamGet NewlineFraming JSON (IOS.InputStream Int)
    :<|> "readme" :> StreamGet NoFraming OctetStream (IOS.InputStream BS.ByteString)
    -- we can have streaming request body
    :<|> "proxy"
        :> StreamBody NoFraming OctetStream (IOS.InputStream BS.ByteString)
        :> StreamPost NoFraming OctetStream (IOS.InputStream BS.ByteString)

api :: Proxy API
api = Proxy

server :: Server API
server = fast :<|> slow :<|> readme :<|> proxy
  where
    fast n = liftIO $ do
        putStrLn ("/get/" ++ show n)
        IOS.fromGenerator $ fastGenerator n

    slow n = liftIO $ do
        putStrLn ("/slow/" ++ show n)
        IOS.fromGenerator $ slowGenerator n

    readme = liftIO $ do
        putStrLn "/readme"
        h <- openFile "README.md" ReadMode
        is <- handleToInputStream h
        atEndOfInput (hClose h) is

    proxy c = liftIO $ do
        putStrLn "/proxy"
        return c

    fastGenerator n
        | n < 0     = return ()
        | otherwise = IOS.yield n >> fastGenerator (n - 1)

    slowGenerator n
        | n < 0     = return ()
        | otherwise = IOS.yield n >> liftIO (threadDelay 1000000) >> slowGenerator (n - 1)

app :: Application
app = serve api server

cli :: Client ClientM FastAPI
cli :<|> _ :<|> _ :<|> _ = client api

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting servant-io-streams:example at http://localhost:8000"
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        ("client":ns:_) -> do
            n <- maybe (fail $ "not a number: " ++ ns) pure $ readMaybe ns
            mgr <- newManager defaultManagerSettings
            burl <- parseBaseUrl "http://localhost:8000/"
            withClientM (cli n) (mkClientEnv mgr burl) $ \me -> case me of
                Left err -> print err
                Right s  -> do
                    x <- IOS.fold (\c _ -> c + 1) (0 :: Int) s
                    print x
        _ -> do
            putStrLn "Try:"
            putStrLn "cabal new-run servant-io-streams:example server"
            putStrLn "cabal new-run servant-io-streams:example client 10"
            putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"
