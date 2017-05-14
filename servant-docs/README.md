# servant-docs

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

Generate API docs for your *servant* webservice. Feel free to also take a look at [servant-pandoc](https://github.com/mpickering/servant-pandoc) which uses this package to target a broad range of output formats using the excellent **pandoc**.

## Example

See [here](https://github.com/haskell-servant/servant/blob/master/servant-docs/example/greet.md) for the output of the following program.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Proxy
import Data.Text
import Servant.Docs

-- our type for a Greeting message
data Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

-- we get our JSON serialization for free. This will be used by the default
-- 'MimeRender' instance for 'JSON'.
instance FromJSON Greet
instance ToJSON Greet

-- We can also implement 'MimeRender' explicitly for additional formats.
instance MimeRender PlainText Greet where
    mimeRender Proxy (Greet s) = "<h1>" <> cs s <> "</h1>"

-- we provide a sample value for the 'Greet' type
instance ToSample Greet where
  toSample = Just g

    where g = Greet "Hello, haskeller!"

instance ToParam (QueryParam "capital" Bool) where
  toParam _ =
    DocQueryParam "capital"
                  ["true", "false"]
                  "Get the greeting message in uppercase (true) or not (false). Default is false."

instance ToCapture (Capture "name" Text) where
  toCapture _ = DocCapture "name" "name of the person to greet"

instance ToCapture (Capture "greetid" Text) where
  toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"

-- API specification
type TestApi =
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON,PlainText] Greet
  :<|> "greet" :> RQBody '[JSON] Greet :> Post '[JSON] Greet
  :<|> "delete" :> Capture "greetid" Text :> Delete '[] ()

testApi :: Proxy TestApi
testApi = Proxy

-- Generate the Documentation's ADT
greetDocs :: API
greetDocs = docs testApi

main :: IO ()
main = putStrLn $ markdown greetDocs
```
