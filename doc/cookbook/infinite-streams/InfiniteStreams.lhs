# Serving infinite streams

Servant provides facilities to work with streams of data. This is handy for cases where the data may take a while to
fetch, but we can start returning data early. In this cookbook, we will be concerned with serving _infinite_ HTTP
streams.

HTTP streams have many advantages compared to other streaming standards like websockets: they are simple
and are well-supported by a broad range of intermediaries, such as proxy servers, content-delivery networks, and
corporate firewalls.

An _infinite_ HTTP stream differs from a _finite_ stream in two major ways. First, cleaning up resources (such as
connections) associated with infinite streams is not deterministic. We do not know when the handler will
terminate -- if at all! Second, we want to ensure that the connection isn't cut off because no data is flowing.
To keep the connection alive, we will need to send bytes on a regular basis.

This is a Literate Haskell file, so let's get imports out of the way.

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

-- from `aeson`
import Data.Aeson (FromJSON, ToJSON)

-- from `async`
import Control.Concurrent.Async (async, link, withAsync) 

-- from `base`
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, isEmptyMVar)
import Control.Exception (throwIO, bracket)
import Control.Monad (forever, (<=<))
import qualified Data.List
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)

-- from `http-client`
import Network.HTTP.Client (newManager, defaultManagerSettings)

-- from the `resourcet` package
import Control.Monad.Trans.Resource (ReleaseKey)
import Data.Acquire ( mkAcquire, Acquire )

-- from `servant`
import Servant
    ( WithResource,
      type (:>),
      StreamGet,
      NewlineFraming,
      JSON,
      SourceIO,
      Context((:.), EmptyContext),
      Handler,
      Application )
import qualified Servant.Types.SourceT as SourceT

-- from `servant-client`
import Servant.Client.Streaming (ClientM, mkClientEnv, client, withClientM, BaseUrl (..), Scheme (Http))

-- from `servant-server`
import Servant.Server (serveWithContext)

-- from `warp`
import qualified Network.Wai.Handler.Warp as Warp (run)
```

We start with our scenario: we are tasked with creating an API which will serve random numbers in real-time. We are
given a function that creates a producer of integers, and a method for us to stop the producer:


```haskell
createProducer :: IO (Chan Int, IO ())
createProducer = do
    chan <- newChan
    isDone <- newEmptyMVar

    let -- This is the action that the consumer of the stream
        -- can run to stop feeding the channel
        weAreDone = putMVar isDone ()

    -- Writer thread will feed our Chan forever. This is where
    -- your secret random number generation algorithm would go.
    -- For this example, we are using a deterministic stream
    -- of numbers, where [1,5,10,20,45] is repeated forever.
    _ <- forkIO (go (cycle [1,5,10,20,45]) chan isDone)

    pure ( chan
         , weAreDone
         )
  where
    go :: [Int] -> Chan Int -> MVar () -> IO ()
    go stream chan isDone = do
        isEmpty <- isEmptyMVar isDone
        if not isEmpty
            -- We are done
            then pure ()
            else case Data.List.uncons stream of
                Nothing -> throwIO (userError "Impossible!")
                Just (nextNum, restStream) -> do
                    -- We simulate a random delay in how numbers are returned.
                    threadDelay (nextNum * 7_000)
                    chan `writeChan` nextNum
                    go restStream chan isDone
```

This was a lot of set up; we now have the ability to create an infinite stream of integers, and message the producer
that we are not listening anymore. In practice, this might mean connecting and disconnecting from a source of
data upstream, for example.

We will now define our API. It has a single route: a method for a subscriber to subscribe to our infinite stream
of random numbers. As mentioned previously, there might be a long time between integers being generated upstream.
We will need to send some bytes just to keep the connection open. To do this, we create a type for the elements
of our infinite stream:

```haskell
data InfiniteStream a = Element a | KeepAlive
    deriving (Show, Generic)

-- For brevity, we derive these instances generically.
-- In production, you can optimize the representation
-- much better.
instance ToJSON a => ToJSON (InfiniteStream a)
instance FromJSON a => FromJSON (InfiniteStream a)
```

We'll also need to package our resources into a specific type, `Upstream`:

```haskell
data Upstream a =
    Upstream { getNext :: IO (InfiniteStream a)
             -- ^ Fetch the next element to forward to the client
             , pleaseStop :: IO ()
             -- ^ Notify upstream to stop sending data
             }
```

`Upstream` is a data type which we want to allocate for a handler, and deallocate once the connection
is closed, which means we want to involve `WithResource`. The API definition becomes:

```haskell
type InfiniteIntegersAPI
    =  WithResource (Upstream Int)
    :> StreamGet
          NewlineFraming
          JSON
          (SourceIO (InfiniteStream Int))
```

Let's write our handler, which is pretty simple: return an infinite stream by
continuously calling `getNext`:

```haskell
handleInfiniteIntegersAPI :: (ReleaseKey, Upstream Int) -> Handler (SourceIO (InfiniteStream Int))
handleInfiniteIntegersAPI (_, upstream) =
    let neverStop = const False
     in pure (SourceT.fromAction neverStop (getNext upstream))
```

Now for the tricky bit. We need to produce data on a regular basis, even if there are no
numbers available upstream. Typically, a HTTP server will break connections after 30 seconds without data.
For this example, we'll provide data 0.1 seconds so that the example runs quickly. We do this when we
allocate a new `Upstream` in `allocate`:

```haskell
allocate :: IO (Upstream Int)
allocate = do
    -- Channel that will feed the client
    toDownstream <- newChan

    -- Producer from upstream
    (intChan, weAreDone) <- createProducer

    -- See comment below
    (link <=< async) $ interleaveLoop intChan toDownstream

    pure (Upstream { getNext = readChan toDownstream
                   , pleaseStop = weAreDone
                   }
         )
  where
    -- This loop interleaves integers from upstream, with keep-alive
    -- messages.
    --
    -- The logic here is to spawn a thread that feeds the 'toDownstream' channel
    -- with keep-alive messages regularly, until 'readChan intChan' succeeds. At this point,
    -- we feed the integer to downstream, and 'withAsync' exits, cancelling
    -- the loop feeding 'KeepAlive' messages.
    interleaveLoop intChan toDownstream = do
        withAsync
            (forever $ threadDelay 100_000 *> writeChan toDownstream KeepAlive)
            (\_ -> readChan intChan >>= writeChan toDownstream . Element)
        interleaveLoop intChan toDownstream
```

Finally, we must tell our server how to allocate and deallocate an `Upstream Int`. The `allocate` function
below is executed when a client connects, and the `deallocate` function is executed when the connection is
closed in any way:

```haskell
withUpstream :: Acquire (Upstream Int)
withUpstream = mkAcquire allocate pleaseStop
```

We now have all the pieces to assemble our server:

```haskell
server :: Application
server = serveWithContext
            (Proxy :: Proxy InfiniteIntegersAPI)
            (withUpstream :. EmptyContext)
            handleInfiniteIntegersAPI
```

and our client:

```haskell
getInfiniteIntegers :: ClientM   (SourceIO (InfiniteStream Int))
getInfiniteIntegers = client (Proxy :: Proxy InfiniteIntegersAPI)
```

We can see how they interact:

```haskell
main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  let url = (BaseUrl Http "localhost" 8080 "")
  bracket (forkIO (Warp.run 8080 server)) killThread (\_ -> do
    threadDelay 100_000
    withClientM getInfiniteIntegers (mkClientEnv mgr url) (\case
        Left clientError -> throwIO clientError
        Right stream -> SourceT.unSourceT stream go
     )
   )
  where
    go (SourceT.Yield !incoming next) = print incoming >> go next
    go (SourceT.Effect !x) = x >>= go
    go (SourceT.Skip !next) = go next
    -- This cookbook recipe is concerned with infinite streams. While
    -- the following two cases should be unreachable, we handle
    -- them for completeness.
    go (SourceT.Error err) = throwIO (userError err)
    go (SourceT.Stop) = pure ()
```

Running this program shows:

```
Element 1
Element 5
Element 10
KeepAlive
Element 20
KeepAlive
KeepAlive
KeepAlive
Element 45
Element 1
Element 5
Element 10
KeepAlive
Element 20
KeepAlive
KeepAlive
KeepAlive
Element 45
Element 1
Element 5
Element 10
KeepAlive
Element 20
...
```
