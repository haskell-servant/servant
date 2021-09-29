# A web API as a type

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
```

Consider the following informal specification of an API:

 > The endpoint at `/users` expects a GET request with query string parameter
 > `sortby` whose value can be one of `age` or `name` and returns a
 > list/array of JSON objects describing users, with fields `age`, `name`,
 > `email`, `registration_date`".

You *should* be able to formalize that. And then use the formalized version to
get you much of the way towards writing a web app. And all the way towards
getting some client libraries, and documentation, and more.

How would we describe it with **servant**? An endpoint description is a good old
Haskell **type**:

``` haskell
type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: UTCTime
}
```

Let's break that down:

- `"users"` says that our endpoint will be accessible under `/users`;
- `QueryParam "sortby" SortBy`, where `SortBy` is defined by `data SortBy = Age | Name`,
    says that the endpoint has a query string parameter named `sortby`
    whose value will be extracted as a value of type `SortBy`.
- `Get '[JSON] [User]` says that the endpoint will be accessible through HTTP
   GET requests, returning a list of users encoded as JSON. You will see
   later how you can make use of this to make your data available under different
   formats, the choice being made depending on the [Accept
   header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) specified in
   the client's request.
- The `:>` operator that separates the various "combinators" just lets you
  sequence static path fragments, URL captures and other combinators. The
  ordering only matters for static path fragments and URL captures. `"users" :>
  "list-all" :> Get '[JSON] [User]`, equivalent to `/users/list-all`, is
  obviously not the same as `"list-all" :> "users" :> Get '[JSON] [User]`, which
  is equivalent to `/list-all/users`. This means that sometimes `:>` is somehow
  equivalent to `/`, but sometimes it just lets you chain another combinator.

Tip: If your endpoint responds to `/` (the root path), just omit any combinators
that introduce path segments. E.g. the following api has only one endpoint on `/`:

``` haskell
type RootEndpoint =
  Get '[JSON] User
```

We can also describe APIs with multiple endpoints by using the `:<|>`
combinators. Here's an example:

``` haskell
type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
           :<|> "list-all" :> "users" :> Get '[JSON] [User]
```

**servant** provides a fair amount of combinators out-of-the-box, but you can
always write your own when you need it. Here's a quick overview of the most
often needed combinators that **servant** comes with.

## Combinators

### Static strings

As you've already seen, you can use type-level strings (enabled with the
`DataKinds` language extension) for static path fragments. Chaining
them amounts to `/`-separating them in a URL.

``` haskell
type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
              -- describes an endpoint reachable at:
              -- /users/list-all/now
```

### `Delete`, `Get`, `Patch`, `Post` and `Put`

The `Get` combinator is defined in terms of the more general `Verb`:
``` haskell ignore
data Verb method (statusCode :: Nat) (contentType :: [*]) a
type Get = Verb 'GET 200
```

There are other predefined type synonyms for other common HTTP methods,
such as e.g.:
``` haskell ignore
type Delete = Verb 'DELETE 200
type Patch  = Verb 'PATCH 200
type Post   = Verb 'POST 200
type Put    = Verb 'PUT 200
```

There are also variants that do not return a 200 status code, such
as for example:
``` haskell ignore
type PostCreated  = Verb 'POST 201
type PostAccepted = Verb 'POST 202
```

An endpoint always ends with a variant of the `Verb` combinator
(unless you write your own combinators). Examples:

``` haskell
type UserAPI4 = "users" :> Get '[JSON] [User]
           :<|> "admins" :> Get '[JSON] [User]
```

### `StreamGet` and `StreamPost`

*Note*: Streaming has changed considerably in `servant-0.15`.

The `StreamGet` and `StreamPost` combinators are defined in terms of the more general `Stream`

``` haskell ignore
data Stream (method :: k1) (status :: Nat) (framing :: *) (contentType :: *) (a :: *)

type StreamGet  = Stream 'GET 200
type StreamPost = Stream 'POST 200
```

These describe endpoints that return a stream of values rather than just a
single value. They not only take a single content type as a parameter, but also
a framing strategy -- this specifies how the individual results are delineated
from one another in the stream. The three standard strategies given with
Servant are `NewlineFraming`, `NetstringFraming` and `NoFraming`, but others
can be written to match other protocols.

### `Capture`

URL captures are segments of the path of a URL that are variable and whose actual value is
captured and passed to the request handlers. In many web frameworks, you'll see
it written as in `/users/:userid`, with that leading `:` denoting that `userid`
is just some kind of variable name or placeholder. For instance, if `userid` is
supposed to range over all integers greater or equal to 1, our endpoint will
match requests made to `/users/1`, `/users/143` and so on.

The `Capture` combinator in **servant** takes a (type-level) string representing
the "name of the variable" and a type, which indicates the type we want to
decode the "captured value" to.

``` haskell ignore
data Capture (s :: Symbol) a
-- s :: Symbol just says that 's' must be a type-level string.
```

In some web frameworks, you use regexes for captures. We use a
[`FromHttpApiData`](https://hackage.haskell.org/package/http-api-data/docs/Web-HttpApiData.html#t:FromHttpApiData)
class, which the captured value must be an instance of.

Examples:

``` haskell
type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
                -- equivalent to 'GET /user/:userid'
                -- except that we explicitly say that "userid"
                -- must be an integer

           :<|> "user" :> Capture "userid" Integer :> DeleteNoContent
                -- equivalent to 'DELETE /user/:userid'
```

In the second case, `DeleteNoContent` specifies a 204 response code
and that the response will always be empty.

### `QueryParam`, `QueryParams`, `QueryFlag`

`QueryParam`, `QueryParams` and `QueryFlag` are about parameters in the query string,
i.e., those parameters that come after the question mark
(`?`) in URLs, like `sortby` in `/users?sortby=age`, whose value is
set to `age`. `QueryParams` lets you specify that the query parameter
is actually a list of values, which can be specified using
`?param=value1&param=value2`. This represents a list of values
composed of `value1` and `value2`. `QueryFlag` lets you specify a
boolean-like query parameter where a client isn't forced to specify a
value. The absence or presence of the parameter's name in the query
string determines whether the parameter is considered to have the
value `True` or `False`. For instance, `/users?active` would list only
active users whereas `/users` would list them all.

Here are the corresponding data type declarations:

``` haskell ignore
data QueryParam (sym :: Symbol) a
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol)
```

Examples:

``` haskell
type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
                -- equivalent to 'GET /users?sortby={age, name}'

```

Again, your handlers don't have to deserialize these things (into, for example,
a `SortBy`). **servant** takes care of it.

### `ReqBody`

Each HTTP request can carry some additional data that the server can use in its
*body*, and this data can be encoded in any format -- as long as the server
understands it. This can be used for example for an endpoint for creating new
users: instead of passing each field of the user as a separate query string
parameter or something dirty like that, we can group all the data into a JSON
object. This has the advantage of supporting nested objects.

**servant**'s `ReqBody` combinator takes a list of content types in which the
data encoded in the request body can be represented and the type of that data.
And, as you might have guessed, you don't have to check the content type
header, and do the deserialization yourself. We do it for you. And return `Bad
Request` or `Unsupported Content Type` as appropriate.

Here's the data type declaration for it:

``` haskell ignore
data ReqBody (contentTypes :: [*]) a
```

Examples:

``` haskell
type UserAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
                -- - equivalent to 'POST /users' with a JSON object
                --   describing a User in the request body
                -- - returns a User encoded in JSON

           :<|> "users" :> Capture "userid" Integer
                        :> ReqBody '[JSON] User
                        :> Put '[JSON] User
                -- - equivalent to 'PUT /users/:userid' with a JSON
                --   object describing a User in the request body
                -- - returns a User encoded in JSON
```

### Request `Header`s

Request headers are used for various purposes, from caching to carrying
auth-related data. They consist of a header name and an associated value. An
example would be `Accept: application/json`.

The `Header` combinator in **servant** takes a type-level string for the header
name and the type to which we want to decode the header's value (from some
textual representation), as illustrated below:

``` haskell ignore
data Header (sym :: Symbol) a
```

Here's an example where we declare that an endpoint makes use of the
`User-Agent` header which specifies the name of the software/library used by
the client to send the request.

``` haskell
type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]
```

### Content types

So far, whenever we have used a combinator that carries a list of content
types, we've always specified `'[JSON]`. However, **servant** lets you use several
content types, and also lets you define your own content types.

Four content types are provided out-of-the-box by the core **servant** package:
`JSON`, `PlainText`, `FormUrlEncoded` and `OctetStream`. If for some obscure
reason you wanted one of your endpoints to make your user data available under
those 4 formats, you would write the API type as below:

``` haskell
type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]
```

(There are other packages that provide other content types. For example
**servant-lucid** and **servant-blaze** allow to generate html pages (using
**lucid** and **blaze-html**) and both come with a content type for html.)

We will further explain how these content types and your data types can play
together in the [section about serving an API](Server.html).

### Response `Headers`

Just like an HTTP request, the response generated by a webserver can carry
headers too. **servant** provides a `Headers` combinator that carries a list of
`Header` types and can be used by simply wrapping the "return type" of an endpoint
with it.

``` haskell ignore
data Headers (ls :: [*]) a
```

If you want to describe an endpoint that returns a "User-Count" header in each
response, you could write it as below:

``` haskell
type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])
```

### Basic Authentication

Once you've established the basic routes and semantics of your API, it's time
to consider protecting parts of it. Authentication and authorization are broad
and nuanced topics; as servant began to explore this space we started small
with one of HTTP's earliest authentication schemes: [Basic
Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication).

When protecting endpoints with basic authentication, we need to specify two items:

1. The **realm** of authentication as per the Basic Authentication spec.
2. The datatype returned by the server after authentication is verified. This
    is usually a `User` or `Customer` type datatype.

With those two items in mind, *servant* provides the following combinator:

``` haskell ignore
data BasicAuth (realm :: Symbol) (userData :: *)
```

Which is used like so:

``` haskell
type ProtectedAPI11
     = UserAPI                              -- this is public
 :<|> BasicAuth "my-realm" User :> UserAPI2 -- this is protected by auth
```

### Empty APIs

Sometimes it is useful to be able to generalise an API over the type of some
part of it:

``` haskell
type UserAPI12 innerAPI
     = UserAPI             -- this is the fixed bit of the API
 :<|> "inner" :> innerAPI  -- this lets us put various other APIs under /inner
```

If there is a case where you do not have anything extra to serve, you can use
the `EmptyAPI` combinator to indicate this:

``` haskell
type UserAPI12Alone = UserAPI12 EmptyAPI
```

This also works well as a placeholder for unfinished parts of an API while it
is under development, for when you know that there should be _something_ there
but you don't yet know what. Think of it as similar to the unit type `()`.

### Interoperability with `wai`: `Raw`

Finally, we also include a combinator named `Raw` that provides an escape hatch
to the underlying low-level web library `wai`. It can be used when
you want to plug a [wai `Application`](http://hackage.haskell.org/package/wai)
into your webservice:

``` haskell
type UserAPI13 = "users" :> Get '[JSON] [User]
                 -- a /users endpoint

            :<|> Raw
                 -- requests to anything else than /users
                 -- go here, where the server will try to
                 -- find a file with the right name
                 -- at the right path
```

One example for this is if you want to serve a directory of static files along
with the rest of your API. But you can plug in everything that is an
`Application`, e.g. a whole web application written in any of the web
frameworks that support `wai`.

Be mindful! The `servant-server`'s router works by pattern-matching the
different routes that are composed using `:<|>`. `Raw`, as an escape hatch,
matches any route that hasn't been matched by previous patterns. Therefore,
any subsequent route will be silently ignored.

``` haskell
type UserAPI14 = Raw
            :<|> "users" :> Get '[JSON] [User]
                 -- In this situation, the /users endpoint
                 -- will not be reachable because the Raw
                 -- endpoint matches requests before
```
A simple way to avoid this pitfall is to either use `Raw` as the last
definition, or to always have it under a static path.

``` haskell
type UserAPI15 = "files" :> Raw
                 -- The raw endpoint is under the /files
                 -- static path, so it won't match /users.
            :<|> "users" :> Get '[JSON] [User]

type UserAPI16 = "users" :> Get '[JSON] [User]
            :<|> Raw
                 -- The Raw endpoint is matched last, so
                 -- it won't overlap another endpoint.
```
