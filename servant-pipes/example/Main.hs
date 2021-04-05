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
import           Network.Wai
                 (Application)
import           System.Environment
                 (getArgs, lookupEnv)
import           System.IO
                 (IOMode (..))
import           Text.Read
                 (readMaybe)

import qualified Pipes                    as P
import           Pipes.ByteString         as PBS
import qualified Pipes.Prelude            as P
import           Pipes.Safe
                 (SafeT)
import qualified Pipes.Safe.Prelude       as P
import           Servant
import           Servant.Client.Streaming
import           Servant.Pipes ()

import qualified Network.Wai.Handler.Warp as Warp

type FastAPI = "get" :> Capture "num" Int :> StreamGet NewlineFraming JSON (P.Producer Int IO ())

-- TODO: Change IO to something with MonadError ServantUnrenderError
type API = FastAPI
    :<|> "slow" :> Capture "num" Int :> StreamGet NewlineFraming JSON (P.Producer Int IO ())
    -- monad can be SafeT IO too.
    :<|> "readme" :> StreamGet NoFraming OctetStream (P.Producer BS.ByteString (SafeT IO) ())
    -- we can have streaming request body
    :<|> "proxy"
        :> StreamBody NoFraming OctetStream (P.Producer BS.ByteString IO ())
        :> StreamPost NoFraming OctetStream (P.Producer BS.ByteString IO ())

api :: Proxy API
api = Proxy

server :: Server API
server = fast :<|> slow :<|> readme :<|> proxy
  where
    fast n = liftIO $ do
        putStrLn ("/get/" ++ show n)
        return $ fastPipe n

    slow n = liftIO $ do
        putStrLn ("/slow/" ++ show n)
        return $ slowPipe n

    readme = liftIO $ do
        putStrLn "/readme"
        return $ P.withFile "README.md" ReadMode $ \h -> PBS.fromHandle h

    proxy c = liftIO $ do
        putStrLn "/proxy"
        return c

    -- for some reason unfold leaks?
    fastPipe m
        | m < 0     = return ()
        | otherwise = P.yield m >> fastPipe (m - 1)

    slowPipe m = fastPipe m P.>-> P.mapM (<$ threadDelay 1000000)

app :: Application
app = serve api server

cli :: Client ClientM FastAPI
cli :<|> _ :<|> _ :<|> _ = client api

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting servant-pipes:example at http://localhost:8000"
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        ("client":ns:_) -> do
            n <- maybe (fail $ "not a number: " ++ ns) pure $ readMaybe ns
            mgr <- newManager defaultManagerSettings
            burl <- parseBaseUrl "http://localhost:8000/"
            withClientM (cli n) (mkClientEnv mgr burl) $ \me -> case me of
                Left err -> print err
                Right p  -> do
                    x <- P.fold (\c _ -> c + 1) (0 :: Int) id p
                    print x
        _ -> do
            putStrLn "Try:"
            putStrLn "cabal new-run servant-pipes:example server"
            putStrLn "cabal new-run servant-pipes:example client 10"
            putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"
