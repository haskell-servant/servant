# OpenAPI

OpenAPI is language-agnostic format for API specifications. It is structured as JSON or YAML
document and can be used to communicate API documentation between the backend and its clients, like
the frontend.

The OpenAPI specification itself is available at https://swagger.io/specification/. It is supported
by various tools, like [swagger-ui](https://swagger.io/tools/swagger-ui/) &mdash; a tool that
visualizes OpenAPI document and allows to send requests to the backend it describes, or
[swagger-codegen](https://swagger.io/tools/swagger-codegen/), which can generate client code in a
variety of languages given the specification.

Since Servant backends already contain a comprehensive description of the API they provide, it is
fairly easy to generate OpenAPI specification based on that description. This is achieved with
[servant-openapi3](https://hackage.haskell.org/package/servant-openapi3) package, which is based on
older `servant-swagger`, targeted at second version of OpenAPI specification (then called Swagger).

This cookbook demonstrates how to use `servant-openapi3` and how to integrate interactive schema
browser with your backend.

## The sample API

Let's start with an API of an example TODO service:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import GHC.Generics
import Data.Text
import Data.Aeson

import Servant

import Data.OpenApi
import Servant.OpenApi
import Servant.Swagger.UI

import Network.Wai.Handler.Warp as Warp

-- | A single Todo entry.
data Todo = Todo
  { created :: Int  -- ^ Creation datetime.
  , summary :: Text -- ^ Task summary.
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema, ToJSON, FromJSON)

-- | A unique Todo entry ID.
newtype TodoId = TodoId Int
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromHttpApiData)
  deriving anyclass (ToParamSchema, ToSchema)

-- | The API of a Todo service.
type TodoAPI
    = "todo" :> Description "Get all TODO items"
             :> Get '[JSON] [Todo]
 :<|> "todo" :> Description "Add a new TODO item"
             :> ReqBody '[JSON] Todo :> Post '[JSON] TodoId
 :<|> "todo" :> Description "Get a TODO item by its id"
             :> Capture "id" TodoId :> Get '[JSON] Todo
 :<|> "todo" :> Description "Update an existing TODO item by its id"
             :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] TodoId
```

Notice that all API endpoints are decorated with `Description` (coming from `servant` itself): these
descriptions will propagate to the OpenAPI document automatically.

## Adding OpenAPI

We are ready to define OpenAPI document for our `TodoAPI`. Everything you need to do for that is to
use `toOpenApi` function from `servant-openapi3` package:

```haskell
-- | OpenAPI spec for Todo API.
todoOpenApi :: OpenApi
todoOpenApi = toOpenApi (Proxy :: Proxy TodoAPI)
```

This is possible since we've derived `ToSchema` for `Todo` and `ToParamSchema` for `TodoId` (needed
since the type is used in URLs) instances &mdash; and this is everything that is needed to generate
the OpenAPI 3.0 specification for our API. All of this is thanks to `Generic`-based schema generator
found in `openapi3` and `servant-openapi3` packages.

Of course, you can customize the schema in many ways, see the documentation for
[`openapi3`](https://hackage.haskell.org/package/openapi3) package.

The generated schema looks something like this:

```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "",
    "version": ""
  },
  "paths": {
    "/todo": {
      "get": {
        "description": "Get all TODO items",
        "responses": {
          "200": {
            "content": {
              "application/json;charset=utf-8": {
                "schema": {
                  "items": {
                    "$ref": "#/components/schemas/Todo"
                  },
                  "type": "array"
                }
              }
            },
            "description": ""
          }
        }
      }
    },
    ........
  }
    "components": {
    "schemas": {
      "Todo": {
        "required": [
          "created",
          "summary"
        ],
        "properties": {
          "summary": {
            "type": "string"
          },
          "created": {
            "minimum": -9223372036854775808,
            "type": "integer",
            "maximum": 9223372036854775807
          }
        },
        "type": "object"
      },
      "TodoId": {
        "minimum": -9223372036854775808,
        "type": "integer",
        "maximum": 9223372036854775807
      }
    }
  }
}
```

The schema can be pasted into the [Swagger editor](https://editor.swagger.io/), which will nicely
display the generated schema.

## Integrating schema browser into the backend

Or, the schema browser can be integrated into the backend itself. This is done via
`servant-swagger-ui` package, which embeds `swagger-ui` into the Servant backend.

First, define a sub-api that will serve the documentation:

```haskell
type DocsAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"
```

And a full API for your backend, which combines your endpoints and `DocsAPI`:

```haskell
type API = DocsAPI :<|> TodoAPI
```

`SwaggerSchemaUI` describes an API that will serve the interactive schema browser at `/swagger-ui`
of your server and the specification in JSON format at `/swagger.json`. Of course, both paths are
customizable.

A handler for `SwaggerSchemaUI`, called `swaggerSchemaUIServer`, expectes one argument: the
specification itself. In our case, it's `todoOpenApi`.

```haskell
todoServer :: Servant.Server API
todoServer = swaggerSchemaUIServer todoOpenApi
  :<|> error "The actual TODO API is not implemented"
```

Now the server can be run as usual:

```haskell
main :: IO ()
main = do
  Warp.run 5000 $ serve (Proxy :: Proxy API) todoServer
```

Run this example, navigate to http://localhost:5000/swagger-ui and you will see the interactive
schema browser:

![](./swagger-ui-example.png)

You can make requests in this UI and they will be sent to your backend as expected.
