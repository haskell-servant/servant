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
import           Data.Void
                 (Void)
import           Network.HTTP.Client
                 (defaultManagerSettings, newManager)
import           Network.Wai
                 (Application)
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import           Data.Machine
import           Servant
import           Servant.Client.Streaming
import           Servant.Machines ()

import qualified Network.Wai.Handler.Warp as Warp

type FastAPI = "get" :> Capture "num" Int :> StreamGet NewlineFraming JSON (MachineT IO (Is Void) Int)

type API = FastAPI
    :<|> "slow" :> Capture "num" Int :> StreamGet NewlineFraming JSON (MachineT IO (Is Void) Int)
    :<|> "proxy"
        :> StreamBody NoFraming OctetStream (MachineT IO (Is Void) BS.ByteString)
        :> StreamPost NoFraming OctetStream (MachineT IO (Is Void) BS.ByteString)

api :: Proxy API
api = Proxy

server :: Server API
server = fast :<|> slow :<|> proxy
  where
    fast n = liftIO $ do
        putStrLn ("/get/" ++ show n)
        return $ fastMachine n

    slow n = liftIO $ do
        putStrLn ("/slow/" ++ show n)
        return $ slowMachine n

    proxy c = liftIO $ do
        putStrLn "/proxy"
        return c

    -- for some reason unfold leaks?
    fastMachine m
        | m < 0     = MachineT (return Stop)
        | otherwise = MachineT (return (Yield m (fastMachine (m - 1))))

    slowMachine m
        | m < 0     = MachineT (return Stop)
        | otherwise = MachineT $ do
            threadDelay 1000000
            return (Yield m (slowMachine (m - 1)))

app :: Application
app = serve api server

cli :: Client ClientM FastAPI
cli :<|> _ = client api

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting servant-machines:example at http://localhost:8000"
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        ("client":ns:_) -> do
            n <- maybe (fail $ "not a number: " ++ ns) pure $ readMaybe ns
            mgr <- newManager defaultManagerSettings
            burl <- parseBaseUrl "http://localhost:8000/"
            withClientM (cli n) (mkClientEnv mgr burl) $ \me -> case me of
                Left err -> print err
                Right m  -> do
                    x <- runT $ fold (\p _ -> p + 1) (0 :: Int) <~ m
                    print x
        _ -> do
            putStrLn "Try:"
            putStrLn "cabal new-run servant-machines:example server"
            putStrLn "cabal new-run servant-machines:example client 10"
            putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"
