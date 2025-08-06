# Authentication in Servant

Once you've established the basic routes and semantics of your API, it's time
to consider protecting parts of it. Authentication and authorization are broad
and nuanced topics; as servant began to explore this space we started small
with one of HTTP's earliest authentication schemes: [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication).

Servant `0.5` shipped with out-of-the-box support for Basic Authentication.
However, we recognize that every web application is its own beautiful snowflake
and are offering experimental support for generalized or ad-hoc authentication.

In this tutorial we'll build two APIs. One protecting certain routes with Basic
Authentication and another protecting the same routes with a custom, in-house
authentication scheme.

## Basic Authentication

When protecting endpoints with basic authentication, we need to specify two
items:

1. The **realm** of authentication as per the Basic Authentication spec.
2. The datatype returned by the server after authentication is verified. This
is usually a `User` or `Customer` datatype.

With those two items in mind, *servant* provides the following combinator:

``` haskell ignore
data BasicAuth (realm :: Symbol) (userData :: *)
```

You can use this combinator to protect an API as follows:

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Authentication where

import Data.Aeson                       (ToJSON)
import Data.ByteString                  (ByteString)
import Data.Map                         (Map, fromList)
import qualified Data.Map            as Map
import Data.Proxy                       (Proxy (Proxy))
import Data.Text                        (Text)
import GHC.Generics                     (Generic)
import Network.Wai                      (Request, requestHeaders)
import Network.Wai.Handler.Warp         (run)
import Servant.API                      ((:<|>) ((:<|>)), (:>), BasicAuth,
                                          Get, JSON)
import Servant.API.BasicAuth            (BasicAuthData (BasicAuthData))
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant                          (throwError)
import Servant.Server                   (BasicAuthCheck (BasicAuthCheck),
                                         BasicAuthResult( Authorized
                                                        , Unauthorized
                                                        ),
                                         Context ((:.), EmptyContext),
                                         err401, err403, errBody, Server,
                                         serveWithContext, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Servant.Server.Experimental.Auth()
import Web.Cookie                       (parseCookies)

-- | private data that needs protection
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | A user we'll grab from the database when we authenticate someone
newtype User = User { userName :: Text }
  deriving (Eq, Show)

-- | a type to wrap our public api
type PublicAPI = Get '[JSON] [PublicData]

-- | a type to wrap our private api
type PrivateAPI = Get '[JSON] PrivateData

-- | our API
type BasicAPI = "public"  :> PublicAPI
           :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI

-- | a value holding a proxy of our API type
basicAuthApi :: Proxy BasicAPI
basicAuthApi = Proxy
```

You can see that we've prefixed our public API with "public" and our private
API with "private." Additionally, the private parts of our API use the
`BasicAuth` combinator to protect them under a Basic Authentication scheme (the
realm for this authentication is `"foo-realm"`).

Unfortunately we're not done. When someone makes a request to our `"private"`
API, we're going to need to provide to servant the logic for validating
usernames and passwords. This adds a certain conceptual wrinkle in servant's
design that we'll briefly discuss. If you want the **TL;DR**: we supply a lookup
function to servant's new `Context` primitive.

Until now, all of servant's API combinators extracted information from a request
or dictated the structure of a response (e.g. a `Capture` param is pulled from
the request path). Now consider an API resource protected by basic
authentication. Once the required `WWW-Authenticate` header is checked, we need
to verify the username and password. But how? One solution would be to force an
API author to provide a function of type `BasicAuthData -> Handler User`
and servant should use this function to authenticate a request. Unfortunately
this didn't work prior to `0.5` because all of servant's machinery was
engineered around the idea that each combinator can extract information from
only the request. We cannot extract the function
`BasicAuthData -> Handler User` from a request! Are we doomed?

Servant `0.5` introduced `Context` to handle this. The type machinery is beyond
the scope of this tutorial, but the idea is simple: provide some data to the
`serve` function, and that data is propagated to the functions that handle each
combinator. Using `Context`, we can supply a function of type
`BasicAuthData -> Handler User` to the `BasicAuth` combinator
handler. This will allow the handler to check authentication and return a `User`
to downstream handlers if successful.

In practice we wrap `BasicAuthData -> Handler User` into a slightly
different function to better capture the semantics of basic authentication:

``` haskell ignore
-- | The result of authentication/authorization
data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr
  deriving (Eq, Show, Read, Generic, Typeable, Functor)

-- | Datatype wrapping a function used to check authentication.
newtype BasicAuthCheck usr = BasicAuthCheck
  { unBasicAuthCheck :: BasicAuthData
                     -> IO (BasicAuthResult usr)
  }
  deriving (Generic, Typeable, Functor)
```

We now use this datatype to supply servant with a method to authenticate
requests. In this simple example the only valid username and password is
`"servant"` and `"server"`, respectively, but in a real, production application
you might do some database lookup here.

```haskell
-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then pure (Authorized (User "servant"))
        else pure Unauthorized
  in BasicAuthCheck check
```

And now we create the `Context` used by servant to find `BasicAuthCheck`:

```haskell
-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext
```

We're now ready to write our `server` method that will tie everything together:

```haskell
-- | an implementation of our server. Here is where we pass all the handlers to our endpoints.
-- In particular, for the BasicAuth protected handler, we need to supply a function
-- that takes 'User' as an argument.
basicAuthServer :: Server BasicAPI
basicAuthServer =
  let publicAPIHandler = pure [PublicData "foo", PublicData "bar"]
      privateAPIHandler (user :: User) = pure (PrivateData (userName user))
  in publicAPIHandler :<|> privateAPIHandler
```

Finally, our main method and a sample session working with our server:

```haskell
-- | hello, server!
basicAuthMain :: IO ()
basicAuthMain = run 8080 (serveWithContext basicAuthApi
                                           basicAuthServerContext
                                           basicAuthServer
                         )

{- Sample session

$ curl -XGET localhost:8080/public
[{"somedata":"foo"},{"somedata":"bar"}

$ curl -iXGET localhost:8080/private
HTTP/1.1 401 Unauthorized
transfer-encoding: chunked
Date: Thu, 07 Jan 2016 22:36:38 GMT
Server: Warp/3.1.8
WWW-Authenticate: Basic realm="foo-realm"

$ curl -iXGET localhost:8080/private -H "Authorization: Basic c2VydmFudDpzZXJ2ZXI="
HTTP/1.1 200 OK
transfer-encoding: chunked
Date: Thu, 07 Jan 2016 22:37:58 GMT
Server: Warp/3.1.8
Content-Type: application/json
{"ssshhh":"servant"}
-}
```

## Generalized Authentication

Sometimes your server's authentication scheme doesn't quite fit with the
standards (or perhaps servant hasn't rolled-out support for that new, fancy
authentication scheme). For such a scenario, servant `0.5` provides easy and
simple experimental support to roll your own authentication.

Why experimental? We worked on the design for authentication for a long time. We
really struggled to find a nice, type-safe niche in the design space. In fact,
`Context` came out of this work, and while it really fit for schemes like Basic
and JWT, it wasn't enough to fully support something like OAuth or HMAC, which
have flows, roles, and other fancy ceremonies. Further, we weren't sure *how*
people will use auth.

So, in typical startup fashion, we developed an MVP of 'generalized auth' and
released it in an experimental module, with the hope of getting feedback from you!
So, if you're reading this or using generalized auth support, please give us
your feedback!

### What is Generalized Authentication?

**TL;DR**: you throw a tagged `AuthProtect` combinator in front of the
endpoints you want protected and then supply a function `Request -> Handler a`,
where `a` is the type of your choice representing the data returned by
successful authentication - e.g., a `User` or, in our example below, `Account`.
This function is run anytime a request matches a protected endpoint. It
precisely solves the "I just need to protect these endpoints with a function
that does some complicated business logic" and nothing more. Behind the scenes
we use a type family instance (`AuthServerData`) and `Context` to accomplish
this.

### Generalized Authentication in Action

Let's implement a trivial authentication scheme. We will protect our API by
looking for a cookie named `"servant-auth-cookie"`. This cookie's value will
contain a key from which we can lookup an `Account`.

```haskell
-- | An account type that we "fetch from the database" after
-- performing authentication
newtype Account = Account { unAccount :: Text }

-- | A (pure) database mapping keys to accounts.
database :: Map ByteString Account
database = fromList [ ("key1", Account "Anne Briggs")
                    , ("key2", Account "Bruce Cockburn")
                    , ("key3", Account "Ghédalia Tazartès")
                    ]

-- | A method that, when given a password, will return an Account.
-- This is our bespoke (and bad) authentication logic.
lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Map.lookup key database of
  Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
  Just usr -> pure usr
```

For generalized authentication, servant exposes the `AuthHandler` type,
which is used to wrap the `Request -> Handler Account` logic. Let's
create a value of type `AuthHandler Request Account` using the above `lookupAccount`
method (note: we depend upon [`cookie`](https://hackage.haskell.org/package/cookie)'s
`parseCookies` for this):

```haskell

--- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 lookupAccount $ do
    cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
    maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie
```

Let's now protect our API with our new, bespoke authentication scheme. We'll
re-use the endpoints from our Basic Authentication example.

```haskell
-- | Our API, with auth-protection
type AuthGenAPI = "private" :> AuthProtect "cookie-auth" :> PrivateAPI
             :<|> "public"  :> PublicAPI

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy
```

Now we need to bring everything together for the server. We have the
`AuthHandler Request Account` value and an `AuthProtected` endpoint. To bind these
together, we need to provide a [Type Family](https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/glasgow_exts.html#type-families)
instance that tells the `HasServer` instance that our `Context` will supply a
`Account` (via `AuthHandler Request Account`) and that downstream combinators will
have access to this `Account` value (or an error will be thrown if authentication
fails).

```haskell

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Account
```

Note that we specify the type-level tag `"cookie-auth"` when defining the type
family instance. This allows us to have multiple authentication schemes
protecting a single API.

We now construct the `Context` for our server, allowing us to instantiate a
value of type `Server AuthGenAPI`, in addition to the server value:

```haskell
-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- argument. We don't worry about the authentication instrumentation here,
-- that is taken care of by supplying context
genAuthServer :: Server AuthGenAPI
genAuthServer =
  let privateDataFunc (Account name) =
          pure (PrivateData ("this is a secret: " <> name))
      publicData = pure [PublicData "this is a public piece of data"]
  in  privateDataFunc :<|> publicData
```

We're now ready to start our server (and provide a sample session)!

```haskell
-- | run our server
genAuthMain :: IO ()
genAuthMain = run 8080 (serveWithContext genAuthAPI genAuthServerContext genAuthServer)

{- Sample Session:

$ curl -XGET localhost:8080/private
Missing auth header

$ curl -XGET localhost:8080/private -H "Cookie: servant-auth-cookie=key3"
[{"ssshhh":"this is a secret: Ghédalia Tazartès"}]

$ curl -XGET localhost:8080/private -H "Cookie: servant-auth-cookie=bad-key"
Invalid Cookie

$ curl -XGET localhost:8080/public
[{"somedata":"this is a public piece of data"}]
-}
```

### Recap

Creating a generalized, ad-hoc authentication scheme was fairly straight
forward:

1. use the `AuthProtect` combinator to protect your API.
2. choose an application-specific data type used by your server when
authentication is successful (in our case this was `Account`).
3. Create a value of `AuthHandler Request Account` which encapsulates the
authentication logic (`Request -> Handler Account`). This function
will be executed every time a request matches a protected route.
4. Provide an instance of the `AuthServerData` type family, specifying your
application-specific data type returned when authentication is successful (in
our case this was `Account`).

Caveats:

1. The module `Servant.Server.Experimental.Auth` contains an orphan `HasServer`
instance for the `AuthProtect` combinator. You may be get orphan instance
warnings when using this.
2. Generalized authentication requires the `UndecidableInstances` extension.

## Client-side Authentication

### Basic Authentication

As of `0.5`, *servant-client* comes with support for basic authentication!
Endpoints protected by Basic Authentication will require a value of type
`BasicAuthData` to complete the request.

You can find more comprehensive Basic Authentication example in the [Cookbook](../cookbook/basic-auth/BasicAuth.html).

### Generalized Authentication

Servant `0.5` also shipped with support for generalized authentication. Similar
to the server-side support, clients need to supply an instance of the
`AuthClientData` type family specifying the datatype the client will use to
marshal an unauthenticated request into an authenticated request. Generally,
this will look like:

```haskell ignore
import           Servant.Common.Req               (Req, addHeader)

-- | The datatype we'll use to authenticate a request. If we were wrapping
-- something like OAuth, this might be a Bearer token.
type instance AuthClientData (AuthProtect "cookie-auth") = String

-- | A method to authenticate a request
authenticateReq :: String -> Req -> Req
authenticateReq s req = addHeader "my-bespoke-header" s req
```

Now, if the client method for our protected endpoint was `getProtected`, then
we could perform authenticated requests as follows:

```haskell ignore
-- | one could curry this to make it simpler to work with.
result = runExceptT (getProtected (mkAuthenticateReq "secret" authenticateReq))
```
