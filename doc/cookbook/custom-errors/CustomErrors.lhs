# Customizing errors from Servant

Servant handles a lot of parsing and validation of the input request. When it can't parse something: query
parameters, URL parts or request body, it will return appropriate HTTP codes like 400 Bad Request.

These responses will contain the error message in their body without any formatting. However, it is often
desirable to be able to provide custom formatting for these error messages, for example, to wrap them in JSON.

Recently Servant got a way to add such formatting. This Cookbook chapter demonstrates how to use it.

Extensions and imports:
```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Aeson
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant

import           Data.String.Conversions
                 (cs)
import           Servant.API.ContentTypes
```

The API (from `greet.hs` example in Servant sources):

```haskell
-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = pure . Greet $ "Hello, " <> name
        helloH name (Just True) = pure . Greet . Text.toUpper $ "Hello, " <> name

        postGreetH greet = pure greet

        deleteGreetH _ = pure NoContent
```

## Error formatters

`servant-server` provides an `ErrorFormatter` type to specify how the error message will be
formatted. A formatter is just a function accepting three parameters:

- `TypeRep` from `Data.Typeable`: this is a runtime representation of the type of the combinator
  (like `Capture` or `ReqBody`) that generated the error. It can be used to display its name (with
  `show`) or even dynamically dispatch on the combinator type. See the docs for `Data.Typeable` and
  `Type.Reflection` modules.
- `Request`: full information for the request that led to the error.
- `String`: specific error message from the combinator.

The formatter is expected to produce a `ServerError` which will be returned from the handler.

Additionally, there is `NotFoundErrorFormatter`, which accepts only `Request` and can customize the
error in case when no route can be matched (HTTP 404).

Let's make two formatters. First one will wrap our error in a JSON:

```json
{
  "error": "ERROR MESSAGE",
  "combinator": "NAME OF THE COMBINATOR"
}
```

Additionally, this formatter will examine the `Accept` header of the request and generate JSON
message only if client can accept it.

```haskell
customFormatter :: ErrorFormatter
customFormatter tr req err =
  let
    -- aeson Value which will be sent to the client
    value = object ["combinator" .= show tr, "error" .= err]
    -- Accept header of the request
    accH = getAcceptHeader req
  in
  -- handleAcceptH is Servant's function that checks whether the client can accept a
  -- certain message type.
  -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
  case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
    -- If client can't handle JSON, we just return the body the old way
    Nothing -> err400 { errBody = cs err }
    -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
    Just (ctypeH, body) -> err400
      { errBody = body
      , errHeaders = [("Content-Type", cs ctypeH)]
      }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404 { errBody = cs $ "Not found path: " <> rawPathInfo req }
```

If you don't need to react to the `Accept` header, you can just unconditionally return the JSON like
this (with `encode` from `Data.Aeson`):

```
err400
  { errBody = encode body
  , errHeaders = [("Content-Type", "application/json")]
  }
```

## Passing formatters to Servant

Servant uses the Context to configure formatters. You only need to add a value of type
`ErrorFormatters` to your context. This is a record with the following fields:

- `bodyParserErrorFormatter :: ErrorFormatter`
- `urlParseErrorFormatter :: ErrorFormatter`
- `headerParseErrorFormatter :: ErrorFormatter`
- `notFoundErrorFormatter :: NotFoundErrorFormatter`

Default formatters are exported as `defaultErrorFormatters`, so you can use record update syntax to
set the only ones you need:

```haskell
customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = customFormatter
  , notFoundErrorFormatter = notFoundFormatter
  }
```

And at last, use `serveWithContext` to run your server as usual:

```haskell
app :: Application
app = serveWithContext testApi (customFormatters :. EmptyContext) server

main :: IO ()
main = run 8000 app
```

Now if we try to request something with a wrong body, we will get a nice error:

```
$ http -j POST localhost:8000/greet 'foo=bar'
HTTP/1.1 400 Bad Request
Content-Type: application/json;charset=utf-8
Date: Fri, 17 Jul 2020 13:34:18 GMT
Server: Warp/3.3.12
Transfer-Encoding: chunked

{
    "combinator": "ReqBody'",
    "error": "Error in $: parsing Main.Greet(Greet) failed, key \"_msg\" not found"
}
```

Notice the `Content-Type` header set by our combinator.
