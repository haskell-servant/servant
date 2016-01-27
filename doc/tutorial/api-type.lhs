---
title: A web API as a type
toc: true
---

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeOperators #-}
>
> module ApiType where
>
> import Data.Text
> import Servant.API

Consider the following informal specification of an API:

 > The endpoint at `/users` expects a GET request with query string parameter
 > `sortby` whose value can be one of `age` or `name` and returns a
 > list/array of JSON objects describing users, with fields `age`, `name`,
 > `email`, `registration_date`".

You *should* be able to formalize that. And then use the formalized version to
get you much of the way towards writing a web app. And all the way towards
getting some client libraries, and documentation (and in the future, who knows
- tests, HATEOAS, ...).

How would we describe it with servant? As mentioned earlier, an endpoint
description is a good old Haskell **type**:

> type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
>
> data SortBy = Age | Name
>
> data User = User {
>   name :: String,
>   age :: Int
> }

Let's break that down:

- `"users"` says that our endpoint will be accessible under `/users`;
- `QueryParam "sortby" SortBy`, where `SortBy` is defined by `data SortBy = Age
| Name`, says that the endpoint has a query string parameter named `sortby`
whose value will be extracted as a value of type `SortBy`.
- `Get '[JSON] [User]` says that the endpoint will be accessible through HTTP
GET requests, returning a list of users encoded as JSON. You will see
later how you can make use of this to make your data available under different
formats, the choice being made depending on the [Accept
header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) specified in
the client's request.
- the `:>` operator that separates the various "combinators" just lets you
sequence static path fragments, URL captures and other combinators. The
ordering only matters for static path fragments and URL captures. `"users" :>
"list-all" :> Get '[JSON] [User]`, equivalent to `/users/list-all`, is
obviously not the same as `"list-all" :> "users" :> Get '[JSON] [User]`, which
is equivalent to `/list-all/users`. This means that sometimes `:>` is somehow
equivalent to `/`, but sometimes it just lets you chain another combinator.

We can also describe APIs with multiple endpoints by using the `:<|>`
combinators. Here's an example:

> type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
>            :<|> "list-all" :> "users" :> Get '[JSON] [User]

*servant* provides a fair amount of combinators out-of-the-box, but you can
always write your own when you need it. Here's a quick overview of all the
combinators that servant comes with.

Combinators
===========

Static strings
--------------

As you've already seen, you can use type-level strings (enabled with the
`DataKinds` language extension) for static path fragments. Chaining
them amounts to `/`-separating them in a URL.

> type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
>               -- describes an endpoint reachable at:
>               -- /users/list-all/now

`Delete`, `Get`, `Patch`, `Post` and `Put`
------------------------------------------

These 5 combinators are very similar except that they each describe a
different HTTP method. This is how they're declared

``` haskell
data Delete (contentTypes :: [*]) a
data Get (contentTypes :: [*]) a
data Patch (contentTypes :: [*]) a
data Post (contentTypes :: [*]) a
data Put (contentTypes :: [*]) a
```

An endpoint ends with one of the 5 combinators above (unless you write your
own). Examples:

> type UserAPI4 = "users" :> Get '[JSON] [User]
>            :<|> "admins" :> Get '[JSON] [User]

`Capture`
---------

URL captures are parts of the URL that are variable and whose actual value is
captured and passed to the request handlers. In many web frameworks, you'll see
it written as in `/users/:userid`, with that leading `:` denoting that `userid`
is just some kind of variable name or placeholder. For instance, if `userid` is
supposed to range over all integers greater or equal to 1, our endpoint will
match requests made to `/users/1`, `/users/143` and so on.

The `Capture` combinator in servant takes a (type-level) string representing
the "name of the variable" and a type, which indicates the type we want to
decode the "captured value" to.

``` haskell
data Capture (s :: Symbol) a
-- s :: Symbol just says that 's' must be a type-level string.
```

In some web frameworks, you use regexes for captures. We use a
[`FromText`](https://hackage.haskell.org/package/servant/docs/Servant-Common-Text.html#t:FromText)
class, which the captured value must be an instance of.

Examples:

> type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
>                 -- equivalent to 'GET /user/:userid'
>                 -- except that we explicitly say that "userid"
>                 -- must be an integer
>
>            :<|> "user" :> Capture "userid" Integer :> Delete '[] ()
>                 -- equivalent to 'DELETE /user/:userid'

`QueryParam`, `QueryParams`, `QueryFlag`, `MatrixParam`, `MatrixParams` and `MatrixFlag`
----------------------------------------------------------------------------------------

`QueryParam`, `QueryParams` and `QueryFlag` are about query string
parameters, i.e., those parameters that come after the question mark
(`?`) in URLs, like `sortby` in `/users?sortby=age`, whose value is
set to `age`. `QueryParams` lets you specify that the query parameter
is actually a list of values, which can be specified using
`?param[]=value1&param[]=value2`. This represents a list of values
composed of `value1` and `value2`. `QueryFlag` lets you specify a
boolean-like query parameter where a client isn't forced to specify a
value. The absence or presence of the parameter's name in the query
string determines whether the parameter is considered to have the
value `True` or `False`. For instance, `/users?active` would list only
active users whereas `/users` would list them all.

Here are the corresponding data type declarations:

``` haskell
data QueryParam (sym :: Symbol) a
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol)
```

[Matrix parameters](http://www.w3.org/DesignIssues/MatrixURIs.html)
are similar to query string parameters, but they can appear anywhere
in the paths (click the link for more details). A URL with matrix
parameters in it looks like `/users;sortby=age`, as opposed to
`/users?sortby=age` with query string parameters. The big advantage is
that they are not necessarily at the end of the URL. You could have
`/users;active=true;registered_after=2005-01-01/locations` to get
geolocation data about users whom are still active and registered
after *January 1st, 2005*.

Corresponding data type declarations below.

``` haskell
data MatrixParam (sym :: Symbol) a
data MatrixParams (sym :: Symbol) a
data MatrixFlag (sym :: Symbol)
```

Examples:

> type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
>                 -- equivalent to 'GET /users?sortby={age, name}'
>
>            :<|> "users" :> MatrixParam "sortby" SortBy :> Get '[JSON] [User]
>                 -- equivalent to 'GET /users;sortby={age, name}'

Again, your handlers don't have to deserialize these things (into, for example,
a `SortBy`). *servant* takes care of it.

`ReqBody`
---------

Each HTTP request can carry some additional data that the server can use in its
*body*, and this data can be encoded in any format -- as long as the server
understands it. This can be used for example for an endpoint for creating new
users: instead of passing each field of the user as a separate query string
parameter or something dirty like that, we can group all the data into a JSON
object. This has the advantage of supporting nested objects.

*servant*'s `ReqBody` combinator takes a list of content types in which the
data encoded in the request body can be represented and the type of that data.
And, as you might have guessed, you don't have to check the content-type
header, and do the deserialization yourself. We do it for you. And return `Bad
Request` or `Unsupported Content Type` as appropriate.

Here's the data type declaration for it:

``` haskell
data ReqBody (contentTypes :: [*]) a
```

Examples:

> type UserAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
>                 -- - equivalent to 'POST /users' with a JSON object
>                 --   describing a User in the request body
>                 -- - returns a User encoded in JSON
>
>            :<|> "users" :> Capture "userid" Integer
>                         :> ReqBody '[JSON] User
>                         :> Put '[JSON] User
>                 -- - equivalent to 'PUT /users/:userid' with a JSON
>                 --   object describing a User in the request body
>                 -- - returns a User encoded in JSON

Request `Header`s
-----------------

Request headers are used for various purposes, from caching to carrying
auth-related data. They consist of a header name and an associated value. An
example would be `Accept: application/json`.

The `Header` combinator in servant takes a type-level string for the header
name and the type to which we want to decode the header's value (from some
textual representation), as illustrated below:

``` haskell
data Header (sym :: Symbol) a
```

Here's an example where we declare that an endpoint makes use of the
`User-Agent` header which specifies the name of the software/library used by
the client to send the request.

> type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]

Content types
-------------

So far, whenever we have used a combinator that carries a list of content
types, we've always specified `'[JSON]`. However, *servant* lets you use several
content types, and also lets you define your own content types.

Four content-types are provided out-of-the-box by the core *servant* package:
`JSON`, `PlainText`, `FormUrlEncoded` and `OctetStream`. If for some obscure
reason you wanted one of your endpoints to make your user data available under
those 4 formats, you would write the API type as below:

> type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

We also provide an HTML content-type, but since there's no single library
that everyone uses, we decided to release 2 packages, *servant-lucid* and
*servant-blaze*, to provide HTML encoding of your data.

We will further explain how these content types and your data types can play
together in the [section about serving an API](/tutorial/server.html).

Response `Headers`
------------------

Just like an HTTP request, the response generated by a webserver can carry
headers too. *servant* provides a `Headers` combinator that carries a list of
`Header` and can be used by simply wrapping the "return type" of an endpoint
with it.

``` haskell
data Headers (ls :: [*]) a
```

If you want to describe an endpoint that returns a "User-Count" header in each
response, you could write it as below:

> type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])

Interoperability with other WAI `Application`s: `Raw`
-----------------------------------------------------

Finally, we also include a combinator named `Raw` that can be used for two reasons:

- You want to serve static files from a given directory. In that case you can just say:

> type UserAPI11 = "users" :> Get '[JSON] [User]
>                  -- a /users endpoint
>
>             :<|> Raw
>                  -- requests to anything else than /users
>                  -- go here, where the server will try to
>                  -- find a file with the right name
>                  -- at the right path

- You more generally want to plug a [WAI `Application`](http://hackage.haskell.org/package/wai)
into your webservice. Static file serving is a specific example of that. The API type would look the
same as above though. (You can even combine *servant* with other web frameworks
this way!)

<div style="text-align: center;">
  <a href="/tutorial/server.html">Next page: Serving an API</a>
</div>
