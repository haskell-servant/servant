# HTTP 4XX/5XX errors with JSON body

Common RESTful design pattern is to return
HTTP 4XX or 5XX errors with JSON as body content.

Servant offer such support via `MonadError` instance: 

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Servant
import GHC.Generics         (Generic)
import Data.Aeson           (ToJSON, encode)
import Data.Text            (Text)
import Network.HTTP.Types   (hContentType)
import Control.Monad.Except (MonadError)


throwJSONError :: (MonadError ServantErr m, ToJSON a) => ServantErr -> a -> m b
throwJSONError err json = throwError $ err
  { errBody = encode json
  , errHeaders = [ jsonHeader ]
  }
  where
    jsonHeader = ( hContentType
                 , "application/json;charset=utf-8" )
```

And simple usage with servant-server:

```haskell
data JSONError = JSONError
  { error :: Text
  } deriving (Generic, ToJSON)

handler :: Handler NoContent
handler = throwJSONError err400 $ JSONError "test"

main :: IO ()
main = undefined
```

There are (at least) two shortcomings with this approach:

1) [throwError sacrifices content negotiation](https://github.com/haskell-servant/servant/issues/732)

2) Errors are not part of the type-level API safety, there are solutions like [servant-checked-exceptions](https://github.com/cdepillabout/servant-checked-exceptions) but they don't implement all HasServer, etc instances.
