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
import           Control.Monad.Trans.Resource
                 (ResourceT)
import qualified Data.ByteString              as BS
import           Data.Maybe
                 (fromMaybe)
import           Network.HTTP.Client
                 (defaultManagerSettings, newManager)
import           Network.Wai
                 (Application)
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import           Data.Conduit
import qualified Data.Conduit.Combinators     as C
import           Servant
import           Servant.Client.Streaming
import           Servant.Conduit ()

import qualified Network.Wai.Handler.Warp     as Warp

type FastAPI = "get" :> Capture "num" Int :> StreamGet NewlineFraming JSON (ConduitT () Int IO ())

type API = FastAPI
    :<|> "slow" :> Capture "num" Int :> StreamGet NewlineFraming JSON (ConduitT () Int IO ())
    -- monad can be ResourceT IO too.
    :<|> "readme" :> StreamGet NoFraming OctetStream (ConduitT () BS.ByteString (ResourceT IO) ())
    -- we can have streaming request body
    :<|> "proxy"
        :> StreamBody NoFraming OctetStream (ConduitT () BS.ByteString IO ())
        :> StreamPost NoFraming OctetStream (ConduitT () BS.ByteString IO ())

api :: Proxy API
api = Proxy

server :: Server API
server = fast :<|> slow :<|> readme :<|> proxy
  where
    fast n = liftIO $ do
        putStrLn $ "/get/" ++ show n
        return $ fastConduit n

    slow n = liftIO $ do
        putStrLn $ "/slow/" ++ show n
        return $ slowConduit n

    readme = liftIO $ do
        putStrLn "/proxy"
        return (C.sourceFile "README.md")

    proxy c = liftIO $ do
        putStrLn "/proxy"
        return c

    -- for some reason unfold leaks?
    fastConduit = C.unfold mk where
        mk m
            | m < 0     = Nothing
            | otherwise = Just (m, pred m)

    slowConduit m = fastConduit m .| C.mapM (<$ threadDelay 1000000)

app :: Application
app = serve api server

cli :: Client ClientM FastAPI
cli :<|> _ :<|> _ :<|> _ = client api

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting servant-conduit:example at http://localhost:8000"
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        ("client":ns:_) -> do
            n <- maybe (fail $ "not a number: " ++ ns) pure $ readMaybe ns
            mgr <- newManager defaultManagerSettings
            burl <- parseBaseUrl "http://localhost:8000/"
            withClientM (cli n) (mkClientEnv mgr burl) $ \me -> case me of
                Left err -> print err
                Right c  -> do
                    x <- connect c $ C.foldl (\p _ -> p + 1) (0 :: Int)
                    print x
        _ -> do
            putStrLn "Try:"
            putStrLn "cabal new-run servant-conduit:example server"
            putStrLn "cabal new-run servant-conduit:example client 10"
            putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"
