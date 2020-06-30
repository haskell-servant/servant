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

## Idiomatic exceptions

Since `UVerb` (probably) will mostly be used for error-like responses, it may be desirable to be able to early abort handler, like with current servant one would use `throwError` with `ServerError`.

```haskell
newtype UVerbT xs m a = UVerbT { unUVerbT :: ExceptT (Union xs) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- | Deliberately hide 'ExceptT's 'MonadError' instance to be able to use
-- underlying monad's instance.
instance MonadError e m => MonadError e (UVerbT xs m) where
  throwError = lift . throwError
  catchError (UVerbT act) h = UVerbT $ ExceptT $
    runExceptT act `catchError` (runExceptT . unUVerbT . h)

-- | This combinator runs 'UVerbT'. It applies 'respond' internally, so the handler
-- may use the usual 'return'.
runUVerbT :: (Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'UVerbT' computation returning one of the response types.
throwUVerb :: (Monad m, HasStatus x, IsMember x xs) => x -> UVerbT xs m a
throwUVerb = UVerbT . ExceptT . fmap Left . respond
```

Example usage:

```haskell
h :: Handler (Union '[Foo, WithStatus 400 Bar])
h = runUVerbT $
  when (something bad) $
    throwUVerb $ WithStatus @400 Bar

  when (really bad) $
    throwError $ err500

  -- a lot of code here...

  return $ Foo 1 2 3
```

## Related Work

There is the [issue from
2017](https://github.com/haskell-servant/servant/issues/841) that was
resolved by the introduction of `UVerb`, with a long discussion on
alternative designs.

[servant-checked-exceptions](https://hackage.haskell.org/package/servant-checked-exceptions)
is a good solution to the problem, but it restricts the user to JSON
and a very specific envelop encoding for the union type, which is
often not acceptable.  (One good reason for this design choice is that
it makes writing clients easier, where you need to get to the union
type from one representative, and you don't want to run several
parsers in the hope that the ones that should will always error out so
you can try until the right one returns a value.)

[servant-exceptions](https://github.com/ch1bo/servant-exceptions) is
another shot at at the problem.  It is inspired by
servant-checked-exceptions, so it may be worth taking a closer look.
The README claims that
[cardano-sl](https://github.com/input-output-hk/cardano-sl) also has
some code for generalized error handling.

In an earier version of the `UVerb` implementation, we have used some
code from
[world-peace](https://hackage.haskell.org/package/world-peace), but
that package itself wasn't flexible enough, and we had to use
[sop-core](https://hackage.haskell.org/package/sop-core) to implement
the `HasServer` instance.

Here is a blog post we found on the subject:
https://lukwagoallan.com/posts/unifying-servant-server-error-responses

(If you have anything else, please add it here or let us know.)
