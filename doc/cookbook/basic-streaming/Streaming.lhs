# Streaming out-of-the-box

In other words, without streaming libraries.

## Introduction

- Servant supports streaming
- Some basic usage doesn't require usage of streaming libraries,
  like `conduit`, `pipes`, `machines` or `streaming`.
  We have bindings for them though.
- Similar example is bundled with each of our streaming library interop packages (see
[servant-pipes](https://github.com/haskell-servant/servant/blob/master/servant-pipes/example/Main.hs),
[servant-conduit](https://github.com/haskell-servant/servant/blob/master/servant-conduit/example/Main.hs) and
[servant-machines](https://github.com/haskell-servant/servant/blob/master/servant-machines/example/Main.hs))
- `SourceT` doesn't have *Prelude* with handy combinators, so we have to write
  things ourselves. (Note to self: `mapM` and `foldM` would be handy to have).

## Code

```haskell
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import           Control.Concurrent
                 (threadDelay)
import           Control.Monad.IO.Class
                 (MonadIO (..))
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

import           Servant
import           Servant.Client.Streaming
import qualified Servant.Types.SourceT as S

import qualified Network.Wai.Handler.Warp     as Warp

type FastAPI = "get" :> Capture "num" Int :> StreamGet NewlineFraming JSON (SourceIO Int)

type API = FastAPI
    :<|> "slow" :> Capture "num" Int :> StreamGet NewlineFraming JSON (SourceIO Int)
    -- monad can be ResourceT IO too.
    :<|> "readme" :> StreamGet NoFraming OctetStream (SourceIO BS.ByteString)
    -- we can have streaming request body
    :<|> "proxy"
        :> StreamBody NoFraming OctetStream (SourceIO BS.ByteString)
        :> StreamPost NoFraming OctetStream (SourceIO BS.ByteString)

api :: Proxy API
api = Proxy

server :: Server API
server = fast :<|> slow :<|> readme :<|> proxy where
    fast n = liftIO $ do
        putStrLn $ "/get/" ++ show n
        pure $ fastSource n

    slow n = liftIO $ do
        putStrLn $ "/slow/" ++ show n
        pure $ slowSource n

    readme = liftIO $ do
        putStrLn "/proxy"
        pure (S.readFile "README.md")

    proxy c = liftIO $ do
        putStrLn "/proxy"
        pure c

    -- for some reason unfold leaks?
    fastSource = S.fromStepT . mk where
        mk m
            | m < 0     = S.Stop
            | otherwise = S.Yield m (mk (m - 1))

    slowSource m = S.mapStepT delay (fastSource m) where
        delay S.Stop        = S.Stop
        delay (S.Error err) = S.Error err
        delay (S.Skip s)    = S.Skip (delay s)
        delay (S.Effect ms) = S.Effect (fmap delay ms)
        delay (S.Yield x s) = S.Effect $
            S.Yield x (delay s) <$ threadDelay 1000000

app :: Application
app = serve api server

cli :: Client ClientM FastAPI
cli :<|> _ :<|> _ :<|> _ = client api

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting cookbook-basic-streaming at http://localhost:8000"
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        ("client":ns:_) -> do
            n <- maybe (fail $ "not a number: " ++ ns) pure $ readMaybe ns
            mgr <- newManager defaultManagerSettings
            burl <- parseBaseUrl "http://localhost:8000/"
            withClientM (cli n) (mkClientEnv mgr burl) $ \me -> case me of
                Left err  -> print err
                Right src -> do
                    x <- S.unSourceT src (go (0 :: Int))
                    print x
                  where
                    go !acc S.Stop        = pure acc
                    go !acc (S.Error err) = print err >> pure acc
                    go !acc (S.Skip s)    = go acc s
                    go !acc (S.Effect ms) = ms >>= go acc
                    go !acc (S.Yield _ s) = go (acc + 1) s
        _ -> do
            putStrLn "Try:"
            putStrLn "cabal run cookbook-basic-streaming server"
            putStrLn "cabal run cookbook-basic-streaming client 10"
            putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"
```
