# Serving web applications over HTTPS

This short recipe shows how one can serve a servant application
over HTTPS, by simply using `warp-tls` instead of `warp` to provide
us a `run` function for running the `Application` that we get by
calling `serve`.

As usual, we start by clearing our throat of a few language extensions
and imports.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
```

No need to work with a complicated API here, let's
make it as simple as it gets:

``` haskell
type API = Get '[JSON] Int

api :: Proxy API
api = Proxy

server :: Server API
server = pure 10

app :: Application
app = serve api server
```

It's now time to actually run the `Application`.
The [`warp-tls`](https://hackage.haskell.org/package/warp-tls/docs/Network-Wai-Handler-WarpTLS.html)
package provides two functions for running an `Application`, called
[`runTLS`](https://hackage.haskell.org/package/warp-tls/docs/Network-Wai-Handler-WarpTLS.html#v:runTLS)
and [`runTLSSocket`](https://hackage.haskell.org/package/warp-tls/docs/Network-Wai-Handler-WarpTLS.html#v:runTLSSocket).
We will be using the first one.

It takes two arguments,
[the TLS settings](https://hackage.haskell.org/package/warp-tls/docs/Network-Wai-Handler-WarpTLS.html#t:TLSSettings)
(certificates, keys, ciphers, etc)
and [the warp settings](https://hackage.haskell.org/package/warp/docs/Network-Wai-Handler-Warp-Internal.html#t:Settings)
(port, logger, etc).

We will be using very simple settings for this example but you are of
course invited to read the documentation for those types to find out
about all the knobs that you can play with.

``` haskell
main :: IO ()
main = runTLS tlsOpts warpOpts app

  where tlsOpts = tlsSettings "cert.pem" "secret-key.pem"
        warpOpts = setPort 8080 defaultSettings
```

This program is available as a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/https).
