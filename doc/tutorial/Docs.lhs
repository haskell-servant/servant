# Documenting an API

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server
```

And we'll import some things from one of our earlier modules
([Serving an API](Server.html)):

``` haskell
import Server (Email(..), ClientInfo(..), Position(..), HelloMessage(..),
  server3, emailForClient)
```

Like client function generation, documentation generation amounts to inspecting the API type and extracting all the data we need to then present it in some format to users of your API.

This time however, we have to assist **servant**. While it is able to deduce a lot of things about our API, it can't magically come up with descriptions of the various pieces of our APIs that are human-friendly and explain what's going on "at the business-logic level". A good example to study for documentation generation is our webservice with the `/position`, `/hello` and `/marketing` endpoints from earlier:

``` haskell
type ExampleAPI = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

exampleAPI :: Proxy ExampleAPI
exampleAPI = Proxy
```

While **servant** can see e.g. that there are 3 endpoints and that the response bodies will be in JSON, it doesn't know what influence the captures, parameters, request bodies and other combinators have on the webservice. This is where some manual work is required.

For every capture, request body, response body, query param, we have to give some explanations about how it influences the response, what values are possible and the likes. Here's how it looks like for the parameters we have above.

``` haskell
instance ToCapture (Capture "x" Int) where
  toCapture _ =
    DocCapture "x"                                -- name
               "(integer) position on the x axis" -- description

instance ToCapture (Capture "y" Int) where
  toCapture _ =
    DocCapture "y"                                -- name
               "(integer) position on the y axis" -- description

instance ToSample Position where
  toSamples _ = singleSample (Position 3 14) -- example of output

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam "name"                     -- name
                  ["Alp", "John Doe", "..."] -- example of values (not necessarily exhaustive)
                  "Name of the person to say hello to." -- description
                  Normal -- Normal, List or Flag

instance ToSample HelloMessage where
  toSamples _ =
    [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp")
    , ("When 'name' is not specified", HelloMessage "Hello, anonymous coward")
    ]
    -- mutliple examples to display this time

ci :: ClientInfo
ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample ClientInfo where
  toSamples _ = singleSample ci

instance ToSample Email where
  toSamples _ = singleSample (emailForClient ci)
```

Types that are used as request or response bodies have to instantiate the `ToSample` typeclass which lets you specify one or more examples of values. `Capture`s and `QueryParam`s have to instantiate their respective `ToCapture` and `ToParam` classes and provide a name and some information about the concrete meaning of that argument, as illustrated in the code above.
The `EmptyAPI` combinator needs no special treatment as it generates no
documentation: an empty API has no endpoints to document.

With all of this, we can derive docs for our API.

``` haskell
apiDocs :: API
apiDocs = docs exampleAPI
```

`API` is a type provided by **servant-docs** that stores all the information one needs about a web API in order to generate documentation in some format. Out of the box, **servant-docs** only provides a pretty documentation printer that outputs [Markdown](http://en.wikipedia.org/wiki/Markdown), but the [**servant-pandoc**](http://hackage.haskell.org/package/servant-pandoc) package can be used to target many useful formats.

**servant**'s markdown pretty printer is a function named `markdown`.

``` haskell ignore
markdown :: API -> String
```

That lets us see what our API docs look down in markdown, by looking at `markdown apiDocs`.

````````` text
## Welcome

This is our super webservice's API.

Enjoy!

## GET /hello

#### GET Parameters:

- name
     - **Values**: *Alp, John Doe, ...*
     - **Description**: Name of the person to say hello to.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- When a value is provided for 'name'

  ```javascript
  {"msg":"Hello, Alp"}
  ```

- When 'name' is not specified

  ```javascript
  {"msg":"Hello, anonymous coward"}
  ```

## POST /marketing

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

  ```javascript
  {"email":"alp@foo.com","interested_in":["haskell","mathematics"],"age":26,"name":"Alp"}
  ```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

  ```javascript
  {"subject":"Hey Alp, we miss you!","body":"Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!","to":"alp@foo.com","from":"great@company.com"}
  ```

## GET /position/:x/:y

#### Captures:

- *x*: (integer) position on the x axis
- *y*: (integer) position on the y axis

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

  ```javascript
  {"x":3,"y":14}
  ```

`````````

However, we can also add one or more introduction sections to the document. We just need to tweak the way we generate `apiDocs`. We will also convert the content to a lazy `ByteString` since this is what **wai** expects for `Raw` endpoints.

``` haskell
docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] exampleAPI

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]
```

`docsWithIntros` just takes an additional parameter, a list of `DocIntro`s that must be displayed before any endpoint docs.

We can now serve the API *and* the API docs with a simple server.

``` haskell
type DocsAPI = ExampleAPI :<|> Raw

api :: Proxy DocsAPI
api = Proxy

server :: Server DocsAPI
server = Server.server3 :<|> Tagged serveDocs where
    serveDocs _ respond =
        respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")

app :: Application
app = serve api server
```

And if you spin up this server and request anything else than `/position`, `/hello` and `/marketing`, you will see the API docs in markdown. This is because `serveDocs` is attempted if the 3 other endpoints don't match and systematically succeeds since its definition is to just return some fixed bytestring with the `text/plain` content type.
