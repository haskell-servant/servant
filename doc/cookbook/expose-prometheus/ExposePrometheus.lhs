# Expose Prometheus metrics

Production services require monitoring to operate reliably and efficiently.  In
a production setup, you may want to record a variety of things like the number
of access to a feature when doing some A-B tests, the duration of database queries to
optimize performance when needed, the number of third-party API calls to avoid
hitting rate-limits, the number of failed logins to report suspicious
activity, etc.  Observability is the umbrella term for techniques and
technologies concerned with exposing such _metrics_ and _traces_ about
internals of services.
A prevalent tool and format to expose metrics is
[Prometheus](https://prometheus.io/).

Prometheus proposes a simple mechanism: services who want to expose metrics add
a web-API route returning a series of metrics in a well-known text-format.
Prometheus collectors then periodically (often at a short interval) request
this route for one or many services.

This cookbook shows how to expose Prometheus counters using Servant so that a
Prometheus collector can then collect metrics about your application. We
leverage the [`prometheus-client`](https://hackage.haskell.org/package/prometheus-client) package to provide most of the instrumentation
primitives.  While packages exist to direcly expose Prometheus counters, this
cookbook guides you to write your own exposition handler. Writing your own
handler allows you to tailor the metrics endpoint to your needs. Indeed, you
may want to re-use Servant combinators to expose different subsets of metrics
onto different endpoints. Another usage of Servant combinators would be to
protect the endpoint so that only trusted clients can read the metrics. Here we
propose to augment the endpoint with a
[CORS](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing) header so
that a browser clients (such as
[prometheus-monitor](https://dicioccio.fr/prometheus-monitor.html)) can query the Prometheus
metrics endpoint.

First, the imports.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad (forever)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import qualified Prometheus as Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
```

In this cookbook we will write a dummy "hello-world" API route.  A counter will
count accesses to this API route.  For the purpose of this cookbook the
"hello-world" API route will count how many times whom got a hello. For
instance "Hello, Bob" and "Hello, Alice" means we got "1 for Bob" and "1 for
Alice", in short, we record a counter _breakdown_.  Another counter will report
values counted in a background-thread, here, a counter in a sleep-loop.  Such
counters can serve as watchdog for other applications: if the counter stops
increasing, then something is amiss.

In a real-application you may want to avoid
exposing counters broken-down by a value chosen by an untrusted-user (i.e., if
our hello-world API is public, you open the door to unbounded
memory-requirements as counter breakdowns persist in memory). However, for the
purpose of this cookbook, we assume the risk is mitigated.

The Prometheus library we use requires us to register the counters ahead of
time. Let's define a datatype to refer to all the counters needed
in this cookbook. An `initCounters` function performs all the needed
registration.

``` haskell
data Counters
  = Counters
  { countHellos :: Prometheus.Vector (Text) Prometheus.Counter
  , countBackground :: Prometheus.Counter
  }

initCounters :: IO Counters
initCounters =
  Counters
    <$> Prometheus.register
          (Prometheus.vector "who"
            (Prometheus.counter
              (Prometheus.Info "cookbook_hello" "breakdown of hello worlds")))
    <*> Prometheus.register
          (Prometheus.counter
            (Prometheus.Info "cookbook_background" "number of background thread steps"))
```

We next implement the dummy "hello-world" API route.  We add a Servant type and
implement a Servant Handler.  We want the API route to have a `who` query-param
so that one can call `hello?who=Alice` and  get a greeting like "hello, Alice"
in return. We use our first Prometheus counter to record how many times Alice,
Bob, or anyone got a greeting.

The handler will defaults to `n/a` as a magic value to represent the absence of
`who` query-parameter.

``` haskell
type Greeting = Text

newtype HelloWho = HelloWho { getWho :: Text }
  deriving FromHttpApiData

type HelloAPI =
  Summary "a dummy hello-world route"
    :> "api"
    :> "hello"
    :> QueryParam "who" HelloWho
    :> Get '[JSON] Greeting

-- | A function to turn an input object into a key that we use as breakdown for
-- the `countHellos` counter. In a real-world setting you want to ponder
-- security and privacy risks of recording user-controlled values as breakdown values.
helloWhoToCounterBreakdown :: HelloWho -> Text
helloWhoToCounterBreakdown (HelloWho txt) = txt

handleHello :: Counters -> Maybe HelloWho -> Handler Greeting
handleHello counters Nothing = do
  let breakdown = "n/a"
  liftIO $ Prometheus.withLabel (countHellos counters) breakdown Prometheus.incCounter
  pure "hello, world"
handleHello counters (Just who) = do
  let breakdown = helloWhoToCounterBreakdown who
  liftIO $ Prometheus.withLabel (countHellos counters) breakdown Prometheus.incCounter
  pure $ "hello, " <> getWho who
```

We further instrument our program with a second metrics. This second metrics
consist of a simple counter. A background thread will increment the counter
every second.

``` haskell
startBackgroundThread :: Counters -> IO ThreadId
startBackgroundThread counters = forkIO $ forever go
  where
    go :: IO ()
    go = do
      Prometheus.incCounter (countBackground counters)
      threadDelay 1000000
```

Now we need to implement the part where we expose the Prometheus metrics on the
web API.
Let's define an API route: it's a simple `HTTP GET` returning some _Metrics_.
In this example we also add the CORS header we discussed in intro.

``` haskell
type PrometheusAPI =
  Summary "Prometheus metrics"
    :> "metrics"
    :> Get '[PlainText]
         (Headers '[Header "Access-Control-Allow-Origin" CORSAllowOrigin] Metrics)
```

With this API type, we now need to fill-in the blanks.  We define a `Metrics`
object that serializes as the Prometheus text format.  We want to keep it
simple and use `Prometheus.exportMetricsAsText` to collect the metrics as a
text-formatted payload. This function is an IO object returning the whole text
payload, thus, our `Metrics` object contains the raw payload in a
"pre-rendered" format for the MimeRender instance.

``` haskell
newtype Metrics = Metrics {getMetrics :: ByteString}

instance MimeRender PlainText Metrics where
  mimeRender _ = getMetrics
```

We define the CORS header helper.

``` haskell
newtype CORSAllowOrigin = CORSAllowOrigin Text
  deriving ToHttpApiData
```

Finally, we define the Prometheus collection endpoint proper. The magic
function `Prometheus.exportMetricsAsText` provided by our Prometheus library
ensures that we collect all registered metrics in a process-global variable.
The implementation uses a where-clause to recall the Handler data-type so that everything is front of our eyes.

``` haskell
handlePrometheus :: CORSAllowOrigin -> Server PrometheusAPI
handlePrometheus corsAllow = handleMetrics
  where
    handleMetrics :: Handler (Headers '[Header "Access-Control-Allow-Origin" CORSAllowOrigin] Metrics)
    handleMetrics = do
      metrics <- liftIO $ Prometheus.exportMetricsAsText
      pure $ addHeader corsAllow $ Metrics metrics
```

Finally, we bundle everything in an application.

The complete API consists in the "hello-world" api aside the Prometheus metrics
endpoint.  As a bonus we register the `ghcMetrics` (from the
`prometheus-metrics-ghc` package) which allows to collects a lot of runtime
information (such as the memory usage of the program), provided that your
binary is compiled with `rtsopts` and run your progam with `+RTS -T` (cf. the [GHC docs about the RTS](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html)).

``` haskell
type API
  = HelloAPI
  :<|> PrometheusAPI

api :: Proxy API
api = Proxy

server :: CORSAllowOrigin -> Counters -> Server API
server policy counters = handleHello counters :<|> handlePrometheus policy

runApp :: Counters -> IO ()
runApp counters = do
  let allowEveryone = CORSAllowOrigin "*"
  run 8080 (serve api (server allowEveryone counters))

main :: IO ()
main = do
  _ <- Prometheus.register ghcMetrics
  counters <- initCounters
  _ <- startBackgroundThread counters
  runApp counters
```

Now you can navigate to various pages:
- http://localhost:8080/metrics
- http://localhost:8080/api/hello
- http://localhost:8080/api/hello?who=prometheus
- http://localhost:8080/metrics
- http://localhost:8080/api/hello?who=prometheus again
- http://localhost:8080/api/hello?who=world
- http://localhost:8080/metrics

You can also see counter increases live by installing a Prometheus collector.
For development and troubleshooting purpose, you can use [prometheus-monitor](https://dicioccio.fr/prometheus-monitor.html) also available as [Firefox extension](https://addons.mozilla.org/en-GB/firefox/addon/prometheus-monitor/).
