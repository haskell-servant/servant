Working with (multiple) contexts
================================

Occasionally you have a big API with different things needing a context.
*FIXME:* explain what's context here. For example
- file upload needs tweaked (param dependent) configuration
- tow sub apis using different realms of basic authentication

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- This is need for generics-lens instance
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

import Control.Lens
import Data.Generics.Product

import Servant
import Servant.Multipart
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)

-- TODO: re-export from Servant
import Servant.Server.Internal (GetNamedContext (..))
```

```haskell
type API = 
         "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Int
    :<|> "users" :>  WithNamedContext "users"
        ( BasicAuth "User space" User :> Get '[PlainText] Text )
    :<|> "admin" :>  WithNamedContext "admin"
        ( BasicAuth "Admin UI" User :> Get '[PlainText] Text )

api :: Proxy API
api = Proxy

newtype User = User Text
```

We'll use very simple handlers: upload handler returns the total size of uploaded files,
where user space and admin ui handler echo the username.

```haskell
server :: Server API
server = uploadH :<|> usersH :<|> adminH where
    uploadH :: MultipartData Mem -> Handler Int
    uploadH multipartData = return $ sum $ map (fromIntegral . LBS.length . fdPayload) $ files multipartData

    usersH (User n) = return $ "User space for " <> n <> "\n"
    adminH (User n) = return $ "Admin UI for " <> n <> "\n"
```

Serving
-------

So far so good, everything is quite simple. Complication starts when we want to serve our server.

If we try:

```haskell.ignore
app :: Application
app = serve api server
```

we get an error:

```
    • No instance for (Servant.Server.Internal.GetNamedContext
                         () "admin" subContext1)
        arising from a use of ‘serve’

```

*TODO* should we use `data EmptyContext =  EmptyContext` for better error message?

We need to provide a context using `serveWithContext`.
Starting with servant-0.15 we have to define own type,
and write few instances for it. We trade a little of boilerplate
for better errors and overall simplicity.

```haskell

data Ctx = Ctx
    { ctxUploadOpts :: MultipartOptions Mem
    , ctxUserSpace  :: BasicAuthCheck User
    , ctxAdminUI    :: BasicAuthCheck User
    }

instance HasMultipartOptions Ctx Mem where
    getMultipartOptions = ctxUploadOpts

instance GetNamedContext Ctx "users" (BasicAuthCheck User) where
    getNamedContext _ = ctxUserSpace

instance GetNamedContext Ctx "admin" (BasicAuthCheck User) where
    getNamedContext _ = ctxAdminUI
```

```haskell
app :: Application
app = serveWithContext api ctx server where
    ctx = Ctx
        { ctxUploadOpts = uploadOpts
        , ctxUserSpace  = check ["alice", "bob"]
        , ctxAdminUI    = check ["alice"]
        }  

uploadOpts :: MultipartOptions Mem
uploadOpts = MultipartOptions
    { generalOptions
        = setMaxRequestFileSize 1024 
        $ defaultParseRequestBodyOptions
    , backendOptions = ()
    }

check :: [Text] -> BasicAuthCheck User
check names = BasicAuthCheck $ \(BasicAuthData n _) -> do
    let n' = TE.decodeLatin1 n
    if n' `elem` names
    then return $ Authorized $ User n'
    else return NoSuchUser
```

We can test the server with:

```
% curl -D - -F 'data=@cabal.project' 'http://localhost:8000/upload'
HTTP/1.1 100 Continue

HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Tue, 10 Jul 2018 10:00:30 GMT
Server: Warp/3.2.22
Content-Type: application/json;charset=utf-8

843
```

`README.md` is bigger than 1024 bytes:

```
% curl -D - -F 'data=@README.md' 'http://localhost:8000/upload'
HTTP/1.1 100 Continue

HTTP/1.0 500 Internal Server Error
Date: Tue, 10 Jul 2018 10:00:42 GMT
Server: Warp/3.2.22
Content-Type: text/plain; charset=utf-8

Something went wrong
```

and we see in server's output:

```
Maximum size exceeded
CallStack (from HasCallStack):
  error, called at ./Network/Wai/Parse.hs:635:49 in wai-extra-3.0.22.1-ae0cb2bee031e782f33b1ba1ca3b3f58e1eb5a0507374e4f2813a23774fde280:Network.Wai.Parse
```

auth tests:

```
% curl -D - 'http://localhost:8000/users'
HTTP/1.1 401 Unauthorized
...
WWW-Authenticate: Basic realm="User space"
```

```
% curl -D - -u bob:xyzzy 'http://localhost:8000/users'
HTTP/1.1 200 OK
...

User space for bob
```

```
% curl -D - -u bob:xyzzy 'http://localhost:8000/admin'
HTTP/1.1 401 Unauthorized
...
WWW-Authenticate: Basic realm="Admin UI"
```

```
% curl -D - -u alice:xyzzy 'http://localhost:8000/admin'
HTTP/1.1 200 OK
...

Admin UI for alice
```

generics-lens
-------------

With [`generics-lens`](http://hackage.haskell.org/package/generic-lens)
we can reduce the required boilerplate to bare minimum,
it's in fact almost copy & pasteable.
This combines best of both works, we get good error reporting
of nominal typing, but most of the flexibility of structural typing.

TODO: `AppendSymbol` is base-4.10, but can be used to allow prefix.
Is it worth mentioning?

```haskell
data CtxGL = CtxGL
    { upload :: MultipartOptions Mem
    , users  :: BasicAuthCheck User
    , admin  :: BasicAuthCheck User
    }
  deriving Generic

-- | 'typed' application is contrained so it will find required field
instance HasMultipartOptions CtxGL Mem where
    getMultipartOptions = view typed

instance HasField name CtxGL CtxGL sub sub => GetNamedContext CtxGL name sub where
    getNamedContext _ = view (field @name)
```

and an evidence that it works:
```haskell
appGL :: Application
appGL = serveWithContext api ctx server where
    ctx = CtxGL
        { upload = uploadOpts
        , users  = check ["alice", "bob"]
        , admin  = check ["alice"]
        }  
```

```haskell
main :: IO ()
main = run 8000 appGL
```
