# File Upload (`multipart/form-data`)

In this recipe, we will implement a web application
with a single endpoint that can process
`multipart/form-data` request bodies, which most
commonly come from HTML forms that allow file upload.

As usual, a bit of throat clearing.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (withSocketsDo)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart

import qualified Data.ByteString.Lazy as LBS
```

Our API consists in a single `POST` endpoint at `/`
that takes a `multipart/form-data` request body and
pretty-prints the data it got to stdout before returning `0`
(because why not).

``` haskell
type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy
```

Because of some technicalities, multipart form data is not
represented as a good old content type like `JSON` in servant,
that one could use with `ReqBody`, but instead is its own
dedicated `ReqBody`-like combinator named
[`MultiPartForm`](https://hackage.haskell.org/package/servant-multipart/docs/Servant-Multipart.html#t:MultipartForm).

This combinator takes two parameters. The first one is the
"backend" to use. Currently, you only have the choice between
`Mem` and `Tmp`. The former loads the entire input in memory,
even the uploaded files, while `Tmp` will stream uploaded
files to some temporary directory.

The second parameter is the type you want the multipart data
to be decoded to. Indeed there is a `FromMultipart` class that
allows you to specify how to decode multipart form data from
`MultipartData` to a custom type of yours. Here we use the
trivial "decoding" to `MultipartData` itself, and simply
will get our hands on the raw input. If you want to use
a type of yours, see the documentation for
[`FromMultipart`](https://hackage.haskell.org/package/servant-multipart/docs/Servant-Multipart.html#t:FromMultipart).

Our only request handler has type `MultipartData Mem -> Handler Integer`.
All it does is list the textual and file inputs that
were sent in the multipart request body. The textual
inputs are in the `inputs` field while the file inputs
are in the `files` field of `multipartData`.

``` haskell
-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: Server API
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content
  pure 0

startServer :: IO ()
startServer = run 8080 (serve api upload)
```

Finally, a main function that brings up our server and
sends some test request with `http-client` (and not
servant-client this time, as servant-multipart does not
yet have support for client generation).

``` haskell
main :: IO ()
main = withSocketsDo . bracket (forkIO startServer) killThread $ \_threadid -> do
  -- we fork the server in a separate thread and send a test
  -- request to it from the main thread.
  manager <- newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8080/"
  resp <- flip httpLbs manager =<< formDataBody form req
  print resp

  where form =
          [ partBS "title" "World"
          , partBS "text" $ encodeUtf8 "Hello"
          , partFileSource "file" "./README.md"
          ]
```

If you run this, you should get:

``` bash
$ cabal build cookbook-file-upload
[...]
$ dist-newstyle/build/x86_64-linux/ghc-8.2.1/cookbook-file-upload-0.1/x/cookbook-file-upload/build/cookbook-file-upload/cookbook-file-upload
Inputs:
  "title" -> "World"
  "text" -> "Hello"
Content of "README.md"
# servant - A Type-Level Web DSL

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

## Getting Started

We have a [tutorial](http://docs.servant.dev/en/latest/tutorial/index.html) that
introduces the core features of servant. After this article, you should be able
to write your first servant webservices, learning the rest from the haddocks'
examples.

[...]

Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Transfer-Encoding","chunked"),("Date","Fri, 08 Dec 2017 16:50:14 GMT"),("Server","Warp/3.2.13"),("Content-Type","application/json;charset=utf-8")], responseBody = "0", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
```

As usual, the code for this recipe is available in a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/file-upload).
