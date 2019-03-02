# servant-client-ghcjs

Type safe querying of servant APIs from the browser.

`servant-client-ghcjs` is much like `servant-client`, as both packages allow you to generate functions that query the endpoints of your servant API. Both packages should feel the same in usage. The big difference lies in how they perform the actual requests. `servant-client` (indirectly) uses your operating system's socket mechanisms, whereas `servant-client-ghcjs` uses your browser's [XHR](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest) mechanisms to send requests.

This guide assumes knowledge of servant. Reading its [documentation](http://docs.servant.dev) is recommended if you're new to the subject.

## Using servant-client-ghcjs
`servant-client-ghcjs` should feel familiar if you've worked with `servant-client`.

Take the following API (taken from the [Querying an API](http://docs.servant.dev/en/stable/tutorial/Client.html) section in the servant documentation)

```haskell
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Main where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API          -- From the 'servant' package, to define the API itself
import Servant.Client.Ghcjs -- To generate client functions

type API =
         "position"  :> Capture "x" Int            :> Capture "y" Int          :> Get '[JSON] Position
    :<|> "hello"     :> QueryParam "name" String   :> Get  '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email


-- Data types used in the API

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName         :: String
  , clientEmail        :: String
  , clientAge          :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance ToJSON ClientInfo

data Email = Email
  { from    :: String
  , to      :: String
  , subject :: String
  , body    :: String
  } deriving (Show, Generic)

instance FromJSON Email
```

Client functions are generated with the `client` function, like with `servant-client`:

```haskell
position :: Int
         -> Int
         -> ClientM Position

hello :: Maybe String
      -> ClientM HelloMessage

marketing :: ClientInfo
          -> ClientM Email

api :: Proxy API
api = Proxy

position :<|> hello :<|> marketing = client api
```

To run these requests, they only need to be given to `runClientM`. The type of which is as follows:

```haskell
runClientM :: ClientM a -> IO (Either ServantError a)
```

The requests can then be run as follows:

```haskell
main :: IO ()
main = do
    ePos <- runClientM $ position 10 20
    print ePos

    eHelloMessage <- runClientM $ hello (Just "Servant")
    print eHelloMessage

    eEmail <- runClientM $ marketing ClientInfo
      { clientName         = "Servant"
      , clientEmail        = "servant@example.com"
      , clientAge          = 3
      , clientInterestedIn = ["servant", "haskell", "type safety", "web apps"]
      }
    print eEmail
```

`runClientM` requires no URL, as it assumes that all requests are meant for the server that served the web page on which the code is being run. It is however possible to call REST APIs from other locations with [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) using `runClientMOrigin`:

```haskell
runClientMOrigin :: ClientM a -> ClientEnv -> IO (Either ServantError a)
```

Where `ClientEnv` holds a `BaseURL` that tells servant where to send the request to.

# Common client functions
Specifically in big applications it can be desirable to have client functions that work for both `servant-client` *and* `servant-client-ghcjs`. Luckily, the common bits of those two packages live in a parent package, called `servant-client-core`. This package holds the tools to create generic client functions. Generating clients this way is a bit different, though, as the client functions need to be generic in the monad `m` that runs the actual requests.

In the example below, the client functions are put in a data type called `APIClient`, which has `m` as a type parameter. The lowercase `apiClient` constructs this data type, demanding that `m` is indeed a monad that can run requests.

```haskell
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

...
import Servant.Client.Core -- From the 'servant-client-core' package

...

data APIClient m = APIClient
  { position  :: Int -> Int -> m Position
  , hello     :: Maybe String -> m HelloMessage
  , marketing :: ClientInfo -> m Email
  }

apiClient
    :: forall m
     . RunClient m
    => APIClient m
apiClient = APIClient { .. }
  where
    position
      :<|> hello
      :<|> marketing = Proxy @API `clientIn` Proxy @m
```

The call site changes slightly too, as the functions now need to be taken from `apiClient`:

```haskell
import Servant.Client.Ghcjs

main :: IO ()
main = do
    ePos <- runClientM $ position apiClient 10 20
    print ePos
```

Here's how the requests would be performed using the regular `servant-client` package:

```haskell
import Servant.Client
import Network.HTTP.Client ( newManager, defaultManagerSettings )

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let clientBaseUrl = BaseUrl Http "www.example.com" 80 ""
    ePos <- runClientM (position apiClient 10 20) $ mkClientEnv mgr clientBaseUrl
    print ePos
```
