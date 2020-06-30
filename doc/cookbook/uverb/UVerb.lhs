# Listing alternative responses and exceptions in your API types

Servant allows you to talk about the exceptions you throw in your API
types.  This is not limited to actual exceptions, you can write
handlers that respond with arbitrary open unions of types.

## Preliminaries

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wno-orphans #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.String.Conversions (cs)
import Data.Swagger (ToSchema)
import Data.Typeable (Proxy (Proxy))
import qualified GHC.Generics as GHC
import qualified Network.HTTP.Client as Client
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API
import Servant.API.UVerb
import Servant.Client
import Servant.Client.UVerb
import Servant.Server
import Servant.Server.UVerb
import Servant.Swagger
import Servant.Swagger.UVerb ()
```

## The API

This looks like a `Verb`-based routing table, except that `UVerb` has
no status, and carries a list of response types rather than a single
one.  Each entry in the list carries its own response code.

```haskell
type API =
         "fisx"  :> Capture "bool" Bool
         :> UVerb 'GET '[JSON] '[FisxUser, WithStatus 303 String]
    :<|> "arian"
         :> UVerb 'GET '[JSON] '[WithStatus 201 ArianUser]
```

Here are the details:

```haskell
data FisxUser = FisxUser {name :: String}
  deriving (Eq, Show, GHC.Generic)

instance ToJSON FisxUser
instance FromJSON FisxUser
instance ToSchema FisxUser

-- | 'HasStatus' allows us to can get around 'WithStatus' if we want
-- to, and associate the status code with our resource types directly.
--
-- (To avoid orphan instances and make it more explicit what's in the
-- API and what isn't, we could even introduce a newtype 'Resource'
-- that wraps all the types we're using in our routing table, and then
-- define lots of 'HasStatus' instances for @Resource This@ and
-- @Resource That@.)
instance HasStatus FisxUser where
  type StatusOf FisxUser = 203

data ArianUser = ArianUser
  deriving (Eq, Show, GHC.Generic)

instance ToJSON ArianUser
instance FromJSON ArianUser
instance ToSchema ArianUser
```

## Server, Client, Swagger

You can just respond with any of the elements of the union in handlers.

```haskell
fisx :: Bool -> Handler (Union '[FisxUser, WithStatus 303 String])
fisx True = respond (FisxUser "fisx")
fisx False = respond (WithStatus @303 ("still fisx" :: String))

arian :: Handler (Union '[WithStatus 201 ArianUser])
arian = respond (WithStatus @201 ArianUser)
```

You can create client functions like you're used to:

```
fisxClient :: Bool -> ClientM (Union '[FisxUser, WithStatus 303 String])
arianClient :: ClientM (Union '[WithStatus 201 ArianUser])
(fisxClient :<|> arianClient) = client (Proxy @API)
```

...  and that's basically it!  Here are a few sample commands that
show you how the swagger docs look like and how you can handle the
result unions in clients:

```
main :: IO ()
main = do
  putStrLn . cs . encodePretty $ toSwagger (Proxy @API)
  _ <- async . Warp.run 8080 $ serve (Proxy @API) (fisx :<|> arian)
  threadDelay 50000
  mgr <- Client.newManager Client.defaultManagerSettings
  let cenv = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")
  result <- runClientM (fisxClient True) cenv
  print $ collapseUResp (Proxy @Show) show <$> result
  print $ extractUResp @FisxUser <$> result
  print $ extractUResp @(WithStatus 303 String) <$> result
  pure ()
```
