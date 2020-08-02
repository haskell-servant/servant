# Serving an API

Enough chit-chat about type-level combinators and representing an API as a
type. Can we have a webservice already?

## A first example

Equipped with some basic knowledge about the way we represent APIs, let's now
write our first webservice.

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
```

**Important**: the `Servant` module comes from the **servant-server** package,
the one that lets us run webservers that implement a particular API type.  It
reexports all the types from the **servant** package that let you declare API
types as well as everything you need to turn your request handlers into a
fully-fledged webserver. This means that in your applications, you can just add
**servant-server** as a dependency, import `Servant` and not worry about anything
else.

We will write a server that will serve the following API.

``` haskell
type UserAPI1 = "users" :> Get '[JSON] [User]
```

Here's what we would like to see when making a GET request to `/users`.

``` javascript
[ {"name": "Isaac Newton", "age": 372, "email": "isaac@newton.co.uk", "registration_date": "1683-03-01"}
, {"name": "Albert Einstein", "age": 136, "email": "ae@mc2.org", "registration_date": "1905-12-01"}
]
```

Now let's define our `User` data type and write some instances for it.

``` haskell
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User
```

Nothing funny going on here. But we now can define our list of two users.

``` haskell
users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]
```

We can now take care of writing the actual webservice that will handle requests
to such an API. This one will be very simple, being reduced to just a single
endpoint. The type of the web application is determined by the API type,
through a *type family* named `Server`. (Type families are just functions that
take types as input and return types.)  The `Server` type family will compute
the right type that a bunch of request handlers should have just from the
corresponding API type.

The first thing to know about the `Server` type family is that behind the
scenes it will drive the routing, letting you focus only on the business
logic. The second thing to know is that for each endpoint, your handlers will
by default run in the `Handler` monad. This is overridable very
easily, as explained near the end of this guide. Third thing, the type of the
value returned in that monad must be the same as the second argument of the
HTTP method combinator used for the corresponding endpoint. In our case, it
means we must provide a handler of type `Handler [User]`. Well,
we have a monad, let's just `return` our list:

``` haskell
server1 :: Server UserAPI1
server1 = return users1
```

That's it. Now we can turn `server` into an actual webserver using
[wai](http://hackage.haskell.org/package/wai) and
[warp](http://hackage.haskell.org/package/warp):

``` haskell
userAPI :: Proxy UserAPI1
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1
```

The `userAPI` bit is, alas, boilerplate (we need it to guide type inference).
But that's about as much boilerplate as you get.

And we're done! Let's run our webservice on the port 8081.

``` haskell
main :: IO ()
main = run 8081 app1
```

You can put this all into a file or just grab [servant's
repo](http://github.com/haskell-servant/servant) and look at the
*doc/tutorial* directory. This code (the source of this web page) is in
*doc/tutorial/Server.lhs*.

If you run it, you can go to `http://localhost:8081/users` in your browser or
query it with curl and you see:

``` bash
$ curl http://localhost:8081/users
[{"email":"isaac@newton.co.uk","registration_date":"1683-03-01","age":372,"name":"Isaac Newton"},{"email":"ae@mc2.org","registration_date":"1905-12-01","age":136,"name":"Albert Einstein"}]
```

## More endpoints

What if we want more than one endpoint? Let's add `/albert` and `/isaac` to
view the corresponding users encoded in JSON.

``` haskell
type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User
```

And let's adapt our code a bit.

``` haskell
isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]
```

Now, just like we separate the various endpoints in `UserAPI` with `:<|>`, we
are going to separate the handlers with `:<|>` too! They must be provided in
the same order as in the API type.

``` haskell
server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac
```

And that's it! You can run this example in the same way that we showed for
`server1` and check out the data available at `/users`, `/albert` and `/isaac`.

## From combinators to handler arguments

Fine, we can write trivial webservices easily, but none of the two above use
any "fancy" combinator from servant. Let's address this and use `QueryParam`,
`Capture` and `ReqBody` right away. You'll see how each occurrence of these
combinators in an endpoint makes the corresponding handler receive an
argument of the appropriate type automatically. You don't have to worry about
manually looking up URL captures or query string parameters, or
decoding/encoding data from/to JSON. Never.

We are going to use the following data types and functions to implement a
server for `API`.

``` haskell
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"
```

We can implement handlers for the three endpoints:

``` haskell
server3 :: Server API
server3 = position
     :<|> hello
     :<|> marketing

  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)
```

Did you see that? The types for your handlers changed to be just what we
needed! In particular:

  - a `Capture "something" a` becomes an argument of type `a` (for `position`);
  - a `QueryParam "something" a` becomes an argument of type `Maybe a` (because
an endpoint can technically be accessed without specifying any query
string parameter, we decided to "force" handlers to be aware that the
parameter might not always be there);

  - a `ReqBody contentTypeList a` becomes an argument of type `a`;

And that's it. Here's the example in action:

``` bash
$ curl http://localhost:8081/position/1/2
{"xCoord":1,"yCoord":2}
$ curl http://localhost:8081/hello
{"msg":"Hello, anonymous coward"}
$ curl http://localhost:8081/hello?name=Alp
{"msg":"Hello, Alp"}
$ curl -X POST -d '{"clientName":"Alp Mestanogullari", "clientEmail" : "alp@foo.com", "clientAge": 25, "clientInterestedIn": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing
{"subject":"Hey Alp Mestanogullari, we miss you!","body":"Hi Alp Mestanogullari,\n\nSince you've recently turned 25, have you checked out our latest haskell, mathematics products? Give us a visit!","to":"alp@foo.com","from":"great@company.com"}
```

For reference, here's a list of some combinators from **servant**:

 > - `Delete`, `Get`, `Patch`, `Post`, `Put`: these do not become arguments. They provide the return type of handlers, which usually is `Handler <something>`.
 > - `Capture "something" a` becomes an argument of type `a`.
 > - `QueryParam "something" a`, `Header "something" a` all become arguments of type `Maybe a`, because there might be no value at all specified by the client for these.
 > - `QueryFlag "something"` gets turned into an argument of type `Bool`.
 > - `QueryParams "something" a` gets turned into an argument of type `[a]`.
 > - `ReqBody contentTypes a` gets turned into an argument of type `a`.

## The `FromHttpApiData`/`ToHttpApiData` classes

Wait... How does **servant** know how to decode the `Int`s from the URL? Or how
to decode a `ClientInfo` value from the request body? The following three sections will
help us answer these questions.

`Capture`s and `QueryParam`s are represented by some textual value in URLs.
`Header`s are similarly represented by a pair of a header name and a
corresponding (textual) value in the request's "metadata". How types are
decoded from headers, captures, and query params is expressed in a class
`FromHttpApiData` (from the package
[**http-api-data**](http://hackage.haskell.org/package/http-api-data)):

``` haskell ignore
class FromHttpApiData a where
  {-# MINIMAL parseUrlPiece | parseQueryParam #-}
  -- | Parse URL path piece.
  parseUrlPiece :: Text -> Either Text a
  parseUrlPiece = parseQueryParam

  -- | Parse HTTP header value.
  parseHeader :: ByteString -> Either Text a
  parseHeader = parseUrlPiece . decodeUtf8

  -- | Parse query param value.
  parseQueryParam :: Text -> Either Text a
  parseQueryParam = parseUrlPiece
```

As you can see, as long as you provide either `parseUrlPiece` (for `Capture`s)
or `parseQueryParam` (for `QueryParam`s), the other methods will be defined in
terms of this.

**http-api-data** provides a decent number of instances, helpers for defining new
ones, and wonderful documentation.

There's not much else to say about these classes. You will need instances for
them when using `Capture`, `QueryParam`, `QueryParams`, and `Header` with your
types. You will need `FromHttpApiData` instances for server-side request
handlers and `ToHttpApiData` instances only when using
**servant-client**, as described in the [section about deriving haskell
functions to query an API](Client.html).

## Using content-types with your data types

The same principle was operating when decoding request bodies from JSON, and
responses *into* JSON. (JSON is just the running example - you can do this with
any content-type.)

This section introduces a couple of typeclasses provided by **servant** that make
all of this work.

### The truth behind `JSON`


What exactly is `JSON` (the type as used in `Get '[JSON] User`)?  Like the 3
other content-types provided out of the box by **servant**, it's a really dumb
data type.

``` haskell ignore
data JSON
data PlainText
data FormUrlEncoded
data OctetStream
```

Obviously, this is not all there is to `JSON`, otherwise it would be quite
pointless. Like most of the data types in **servant**, `JSON` is mostly there as
a special *symbol* that's associated with encoding (resp. decoding) to (resp.
from) the *JSON* format. The way this association is performed can be
decomposed into two steps.

The first step is to provide a proper
`MediaType` (from
[**http-media**](https://hackage.haskell.org/package/http-media-0.6.2/docs/Network-HTTP-Media.html))
representation for `JSON`, or for your own content-types. If you look at the
haddocks from this link, you can see that we just have to specify
`application/json` using the appropriate functions. In our case, we can just
use `(//) :: ByteString -> ByteString -> MediaType`. The precise way to specify
the `MediaType` is to write an instance for the `Accept` class:

``` haskell ignore
-- for reference:
class Accept ctype where
    contentType   :: Proxy ctype -> MediaType

instance Accept JSON where
    contentType _ = "application" // "json"
```

The second step is centered around the `MimeRender` and `MimeUnrender` classes.
These classes just let you specify a way to encode and decode
values into or from your content-type's representation.

``` haskell ignore
class Accept ctype => MimeRender ctype a where
    mimeRender :: Proxy ctype -> a -> ByteString
    -- alternatively readable as:
    mimeRender :: Proxy ctype -> (a -> ByteString)
```

Given a content-type and some user type, `MimeRender` provides a function that
encodes values of type `a` to lazy `ByteString`s.

In the case of `JSON`, this is easily dealt with! For any type `a` with a
`ToJSON` instance, we can render values of that type to JSON using
`Data.Aeson.encode`.

``` haskell ignore
instance ToJSON a => MimeRender JSON a where
  mimeRender _ = encode
```

And now the `MimeUnrender` class, which lets us extract values from lazy
`ByteString`s, alternatively failing with an error string.

``` haskell ignore
class Accept ctype => MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> ByteString -> Either String a
```

We don't have much work to do there either, `Data.Aeson.eitherDecode` is
precisely what we need. However, it only allows arrays and objects as toplevel
JSON values and this has proven to get in our way more than help us so we wrote
our own little function around **aeson** and **attoparsec** that allows any type of
JSON value at the toplevel of a "JSON document". Here's the definition in case
you are curious.

``` haskell
eitherDecodeLenient :: FromJSON a => ByteString -> Either String a
eitherDecodeLenient input = do
    v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
    parseEither parseJSON v
```

This function is exactly what we need for our `MimeUnrender` instance.

``` haskell ignore
instance FromJSON a => MimeUnrender JSON a where
    mimeUnrender _ = eitherDecodeLenient
```

And this is all the code that lets you use `JSON` with `ReqBody`, `Get`,
`Post` and friends. We can check our understanding by implementing support
for an `HTML` content-type, so that users of your webservice can access an
HTML representation of the data they want, ready to be included in any HTML
document, e.g. using [jQuery's `load` function](https://api.jquery.com/load/),
simply by adding `Accept: text/html` to their request headers.

### Case-studies: **servant-blaze** and **servant-lucid**

These days, most of the haskellers who write their HTML UIs directly from
Haskell use either [**blaze-html**](http://hackage.haskell.org/package/blaze-html)
or [**lucid**](http://hackage.haskell.org/package/lucid). The best option for
**servant** is obviously to support both (and hopefully other templating
solutions!). We're first going to look at **lucid**:

``` haskell
data HTMLLucid
```

Once again, the data type is just there as a symbol for the encoding/decoding
functions, except that this time we will only worry about encoding since
**lucid** doesn't provide a way to extract data from HTML.

``` haskell
instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
```

Note that this instance uses the `(/:)` operator from **http-media** which lets
us specify additional information about a content-type, like the charset here.

The rendering instances call similar functions that take
types with an appropriate instance to an "abstract" HTML representation and
then write that to a `ByteString`.

``` haskell
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS
```

For **blaze-html** everything works very similarly:

``` haskell
-- For this tutorial to compile 'HTMLLucid' and 'HTMLBlaze' have to be
-- distinct. Usually you would stick to one html rendering library and then
-- you can go with one 'HTML' type.
data HTMLBlaze

instance Accept HTMLBlaze where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender HTMLBlaze a where
    mimeRender _ = renderHtml . Text.Blaze.Html.toHtml

-- while we're at it, just like for lucid we can
-- provide an instance for rendering blaze's 'Html' type
instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
    mimeRender _ = renderHtml
```

Both [**servant-blaze**](http://hackage.haskell.org/package/servant-blaze) and
[**servant-lucid**](http://hackage.haskell.org/package/servant-lucid) let you use
`HTMLLucid` and `HTMLBlaze` in any content-type list as long as you provide an instance of the
appropriate class (`ToMarkup` for **blaze-html**, `ToHtml` for **lucid**).

We can now write a webservice that uses **servant-lucid** to show the `HTMLLucid`
content-type in action. We will be serving the following API:

``` haskell
type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]
```

where `Person` is defined as follows:

``` haskell
data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic -- for the JSON instance

instance ToJSON Person
```

Now, let's teach **lucid** how to render a `Person` as a row in a table, and then
a list of `Person`s as a table with a row per person.

``` haskell
-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml
```

We create some `Person` values and serve them as a list:

``` haskell
people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = return people

app2 :: Application
app2 = serve personAPI server4
```

And we're good to go:

``` bash
$ curl http://localhost:8081/persons
[{"lastName":"Newton","firstName":"Isaac"},{"lastName":"Einstein","firstName":"Albert"}]
$ curl -H 'Accept: text/html' http://localhost:8081/persons
<table><tr><td>first name</td><td>last name</td></tr><tr><td>Isaac</td><td>Newton</td></tr><tr><td>Albert</td><td>Einstein</td></tr></table>
# or just point your browser to http://localhost:8081/persons
```

## The `Handler` monad

At the heart of the handlers is the monad they run in, namely a newtype `Handler` around `ExceptT ServerError IO`
([haddock documentation for `ExceptT`](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html#t:ExceptT)).
One might wonder: why this monad? The answer is that it is the
simplest monad with the following properties:

- it lets us both return a successful result (using `return`)
or "fail" with a descriptive error (using `throwError`);
- it lets us perform IO, which is absolutely vital since most webservices exist
as interfaces to databases that we interact with in `IO`.

Let's recall some definitions.

``` haskell ignore
-- from the 'mtl' package at
newtype ExceptT e m a = ExceptT (m (Either e a))
```

In short, this means that a handler of type `Handler a` is simply
equivalent to a computation of type `IO (Either ServerError a)`, that is, an IO
action that either returns an error or a result.

The module [`Control.Monad.Except`](https://hackage.haskell.org/package/mtl/docs/Control-Monad-Except.html#t:ExceptT)
from which `ExceptT` comes is worth looking at.
Perhaps most importantly, `ExceptT` and `Handler` are instances of `MonadError`, so
`throwError` can be used to return an error from your handler (whereas `return`
        is enough to return a success).

Most of what you'll be doing in your handlers is running some IO and,
depending on the result, you might sometimes want to throw an error of some
kind and abort early. The next two sections cover how to do just that.

### Performing IO

Other important instances from the list above are `MonadIO m => MonadIO
(ExceptT e m)`, and therefore also `MonadIO Handler` as there is a `MonadIO IO` instance.
[`MonadIO`](http://hackage.haskell.org/package/base/docs/Control-Monad-IO-Class.html#t:MonadIO)
is a class from the **transformers** package defined as:

``` haskell ignore
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
```

So if you want to run any kind of
IO computation in your handlers, just use `liftIO`:

``` haskell
type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
  filecontent <- liftIO (readFile "myfile.txt")
  return (FileContent filecontent)
```

### Failing, through `ServerError`

If you want to explicitly fail at providing the result promised by an endpoint
using the appropriate HTTP status code (not found, unauthorized, etc) and some
error message, all you have to do is use the `throwError` function mentioned above
and provide it with the appropriate value of type `ServerError`, which is
defined as:

``` haskell ignore
data ServerError = ServerError
    { errHTTPCode     :: Int
    , errReasonPhrase :: String
    , errBody         :: ByteString -- lazy bytestring
    , errHeaders      :: [Header]
    }
```

Many standard values are provided out of the box by the `Servant.Server`
module.  If you want to use these values but add a body or some headers, just
use record update syntax:

``` haskell
failingHandler :: Handler ()
failingHandler = throwError myerr

  where myerr :: ServerError
        myerr = err503 { errBody = "Sorry dear user." }
```

Here's an example where we return a customised 404-Not-Found error message in
the response body if "myfile.txt" isn't there:

``` haskell
server6 :: Server IOAPI1
server6 = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then liftIO (readFile "myfile.txt") >>= return . FileContent
    else throwError custom404Err

  where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }
```

Here's how that server looks in action:

``` bash
$ curl --verbose http://localhost:8081/myfile.txt
[snip]
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /myfile.txt HTTP/1.1
> User-Agent: curl/7.30.0
> Host: localhost:8081
> Accept: */*
>
< HTTP/1.1 404 Not Found
[snip]
myfile.txt just isn't there, please leave this server alone.

$ echo Hello > myfile.txt

$ curl --verbose http://localhost:8081/myfile.txt
[snip]
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /myfile.txt HTTP/1.1
> User-Agent: curl/7.30.0
> Host: localhost:8081
> Accept: */*
>
< HTTP/1.1 200 OK
[snip]
< Content-Type: application/json
[snip]
{"content":"Hello\n"}
```

## Response headers

To add headers to your response, use
[addHeader](http://hackage.haskell.org/package/servant/docs/Servant-API-ResponseHeaders.html).
Note that this changes the type of your API, as we can see in the following example:

``` haskell
type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 albert
```

Note that the type of `addHeader header x` is different than the type of `x`!
And if you add more headers, more headers will appear in the header list:

``` haskell
type MyHeadfulHandler = Get '[JSON] (Headers '[Header "X-A-Bool" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = return $ addHeader True $ addHeader 1797 albert
```

But what if your handler only *sometimes* adds a header? If you declare that
your handler adds headers, and you don't add one, the return type of your
handler will be different than expected. To solve this, you have to explicitly
*not* add a header by using `noHeader`:

``` haskell
type MyMaybeHeaderHandler
  = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x = return $ if x then addHeader 1797 albert
                                       else noHeader albert
```

## Serving static files

**servant-server** also provides a way to just serve the content of a directory
under some path in your web API. As mentioned earlier in this document, the
`Raw` combinator can be used in your APIs to mean "plug here any WAI
application". Well, **servant-server** provides a function to get a file and
directory serving WAI application, namely:

``` haskell ignore
-- exported by Servant and Servant.Server
serveDirectoryWebApp :: FilePath -> Server Raw
```

`serveDirectoryWebApp`'s argument must be a path to a valid directory.

Here's an example API that will serve some static files:

``` haskell
type StaticAPI = "static" :> Raw
```

And the server:

``` haskell
staticAPI :: Proxy StaticAPI
staticAPI = Proxy
```

``` haskell
server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

app3 :: Application
app3 = serve staticAPI server7
```

This server will match any request whose path starts with `/static` and will look
for a file at the path described by the rest of the request path, inside the
 *static-files/* directory of the path you run the program from.

In other words: If a client requests `/static/foo.txt`, the server will look for a file at
`./static-files/foo.txt`. If that file exists it'll succeed and serve the file.
If it doesn't exist, the handler will fail with a `404` status code.

`serveDirectoryWebApp` uses some standard settings that fit the use case of
serving static files for most web apps. You can find out about the other
options in the documentation of the `Servant.Server.StaticFiles` module.

## Nested APIs

Let's see how you can define APIs in a modular way, while avoiding repetition.
Consider this simple example:

``` haskell
type UserAPI3 = -- view the user with given userid, in JSON
                Capture "userid" Int :> Get '[JSON] User

           :<|> -- delete the user with given userid. empty response
                Capture "userid" Int :> DeleteNoContent
```

We can instead factor out the `userid`:

``` haskell
type UserAPI4 = Capture "userid" Int :>
  (    Get '[JSON] User
  :<|> DeleteNoContent
  )
```

However, you have to be aware that this has an effect on the type of the
corresponding `Server`:

``` haskell ignore
Server UserAPI3 = (Int -> Handler User)
             :<|> (Int -> Handler NoContent)

Server UserAPI4 = Int -> (    Handler User
                         :<|> Handler NoContent
                         )
```

In the first case, each handler receives the *userid* argument. In the latter,
the whole `Server` takes the *userid* and has handlers that are just
computations in `Handler`, with no arguments. In other words:

``` haskell
server8 :: Server UserAPI3
server8 = getUser :<|> deleteUser

  where getUser :: Int -> Handler User
        getUser _userid = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser _userid = error "..."

-- notice how getUser and deleteUser
-- have a different type! no argument anymore,
-- the argument directly goes to the whole Server
server9 :: Server UserAPI4
server9 userid = getUser userid :<|> deleteUser userid

  where getUser :: Int -> Handler User
        getUser = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser = error "..."
```

Note that there's nothing special about `Capture` that lets you "factor it
out": this can be done with any combinator. Here are a few examples of APIs
with a combinator factored out for which we can write a perfectly valid
`Server`.

``` haskell
-- we just factor out the "users" path fragment
type API1 = "users" :>
  (    Get '[JSON] [User] -- user listing
  :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
  )

-- we factor out the Request Body
type API2 = ReqBody '[JSON] User :>
  (    Get '[JSON] User -- just display the same user back, don't register it
  :<|> PostNoContent -- register the user. empty response
  )

-- we factor out a Header
type API3 = Header "Authorization" Token :>
  (    Get '[JSON] SecretData -- get some secret data, if authorized
  :<|> ReqBody '[JSON] SecretData :> PostNoContent -- add some secret data, if authorized
  )

newtype Token = Token ByteString
newtype SecretData = SecretData ByteString
```

This approach lets you define APIs modularly and assemble them all into one big
API type only at the end.

``` haskell
type UsersAPI =
       Get '[JSON] [User] -- list users
  :<|> ReqBody '[JSON] User :> PostNoContent -- add a user
  :<|> Capture "userid" Int :>
         ( Get '[JSON] User -- view a user
      :<|> ReqBody '[JSON] User :> PutNoContent -- update a user
      :<|> DeleteNoContent -- delete a user
         )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations

  where getUsers :: Handler [User]
        getUsers = error "..."

        newUser :: User -> Handler NoContent
        newUser = error "..."

        userOperations userid =
          viewUser userid :<|> updateUser userid :<|> deleteUser userid

          where
            viewUser :: Int -> Handler User
            viewUser = error "..."

            updateUser :: Int -> User -> Handler NoContent
            updateUser = error "..."

            deleteUser :: Int -> Handler NoContent
            deleteUser = error "..."
```

``` haskell
type ProductsAPI =
       Get '[JSON] [Product] -- list products
  :<|> ReqBody '[JSON] Product :> PostNoContent -- add a product
  :<|> Capture "productid" Int :>
         ( Get '[JSON] Product -- view a product
      :<|> ReqBody '[JSON] Product :> PutNoContent -- update a product
      :<|> DeleteNoContent -- delete a product
         )

data Product = Product { productId :: Int }

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations

  where getProducts :: Handler [Product]
        getProducts = error "..."

        newProduct :: Product -> Handler NoContent
        newProduct = error "..."

        productOperations productid =
          viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid

          where
            viewProduct :: Int -> Handler Product
            viewProduct = error "..."

            updateProduct :: Int -> Product -> Handler NoContent
            updateProduct = error "..."

            deleteProduct :: Int -> Handler NoContent
            deleteProduct = error "..."
```

``` haskell
type CombinedAPI = "users" :> UsersAPI
              :<|> "products" :> ProductsAPI

server10 :: Server CombinedAPI
server10 = usersServer :<|> productsServer
```

Finally, we can realize the user and product APIs are quite similar and
abstract that away:

``` haskell
-- API for values of type 'a'
-- indexed by values of type 'i'
type APIFor a i =
       Get '[JSON] [a] -- list 'a's
  :<|> ReqBody '[JSON] a :> PostNoContent -- add an 'a'
  :<|> Capture "id" i :>
         ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
      :<|> ReqBody '[JSON] a :> PutNoContent -- update an 'a'
      :<|> DeleteNoContent -- delete an 'a'
         )

-- Build the appropriate 'Server'
-- given the handlers of the right type.
serverFor :: Handler [a] -- handler for listing of 'a's
          -> (a -> Handler NoContent) -- handler for adding an 'a'
          -> (i -> Handler a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (i -> a -> Handler NoContent) -- updating an 'a' with given id
          -> (i -> Handler NoContent) -- deleting an 'a' given its id
          -> Server (APIFor a i)
serverFor = error "..."
-- implementation left as an exercise. contact us on IRC
-- or the mailing list if you get stuck!
```

When your API contains the `EmptyAPI` combinator, you'll want to use
`emptyServer` in the corresponding slot for your server, which will simply fail
with 404 whenever a request reaches it:

``` haskell
type CombinedAPI2 = API :<|> "empty" :> EmptyAPI

server11 :: Server CombinedAPI2
server11 = server3 :<|> emptyServer
```

## Using another monad for your handlers

Remember how `Server` turns combinators for HTTP methods into `Handler`? Well, actually, there's more to that. `Server` is actually a
simple type synonym.

``` haskell ignore
type Server api = ServerT api Handler
```

`ServerT` is the actual type family that computes the required types for the
handlers that's part of the `HasServer` class. It's like `Server` except that
it takes another parameter which is the monad you want your handlers to run in,
or more generally the return types of your handlers. This third parameter is
used for specifying the return type of the handler for an endpoint, e.g when
computing `ServerT (Get '[JSON] Person) SomeMonad`. The result would be
`SomeMonad Person`.

The first and main question one might have then is: how do we write handlers
that run in another monad? How can we "bring back" the value from a given monad
into something **servant** can understand?

### Natural transformations

If we have a function that gets us from an `m a` to an `n a`, for any `a`, what
do we have?

``` haskell
type (~>) m n = forall a. m a -> n a
```

For example:

``` haskell
listToMaybe' :: [] ~> Maybe
listToMaybe' = listToMaybe -- from Data.Maybe
```

Note that `servant` doesn't declare the `~>` type-alias, as the unfolded
variant isn't much longer to write, as we'll see shortly.

So if you want to write handlers using another monad/type than `Handler`, say the `Reader String` monad, the first thing you have to
prepare is a function:

``` haskell ignore
readerToHandler :: Reader String a -> Handler a
```

We obviously have to run the `Reader` computation by supplying it with a
`String`, like `"hi"`. We get an `a` out from that and can then just `return`
it into `Handler`.

``` haskell
readerToHandler :: Reader String a -> Handler a
readerToHandler r = return (runReader r "hi")
```

We can write some simple webservice with the handlers running in `Reader String`.

``` haskell
type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b where
    a :: Reader String Int
    a = return 1797

    b :: Double -> Reader String Bool
    b _ = asks (== "hi")
```

We unfortunately can't use `readerServerT` as an argument of `serve`, because
`serve` wants a `Server ReaderAPI`, i.e., with handlers running in `Handler`. But there's a simple solution to this.

### Welcome `hoistServer`

That's right. We have just written `readerToHandler`, which is exactly what we
would need to apply to all handlers to make the handlers have the
right type for `serve`. Being cumbersome to do by hand, we provide a function
`hoistServer` which takes a natural transformation between two parameterized types `m`
and `n` and a `ServerT someapi m`, and returns a `ServerT someapi n`.

In our case, we can wrap up our little webservice by using
`hoistServer readerAPI readerToHandler` on our handlers.

``` haskell
readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI readerToHandler readerServerT

app4 :: Application
app4 = serve readerAPI readerServer
```

This is the webservice in action:

``` bash
$ curl http://localhost:8081/a
1797
$ curl http://localhost:8081/b -X GET -d '42.0' -H 'Content-Type: application/json'
true
```

### An arrow is a reader too.

In previous versions of `servant` we had an `enter` to do what `hoistServer`
does now. `enter` had an ambitious design goals, but was problematic in practice.

One problematic situation was when the source monad was `(->) r`, yet it's
handy in practice, because `(->) r` is isomorphic to `Reader r`.

We can rewrite the previous example without `Reader`:

```haskell
funServerT :: ServerT ReaderAPI ((->) String)
funServerT = a :<|> b where
    a :: String -> Int
    a _ = 1797

    -- unfortunately, we cannot make `String` the first argument.
    b :: Double -> String -> Bool
    b _ s = s == "hi"

funToHandler :: (String -> a) -> Handler a
funToHandler f = return (f "hi")

app5 :: Application
app5 = serve readerAPI (hoistServer readerAPI funToHandler funServerT)
```

## Streaming endpoints

We can create endpoints that don't just give back a single result, but give
back a *stream* of results, served one at a time. Stream endpoints only provide
a single content type, and also specify what framing strategy is used to
delineate the results. To serve these results, we need to give back a stream
producer. Adapters can be written to *Pipes*, *Conduit* and the like, or
written directly as `SourceIO`s. SourceIO builds upon servant's own `SourceT`
stream type (it's simpler than *Pipes* or *Conduit*).
The API of a streaming endpoint needs to explicitly specify which sort of
generator it produces. Note that the generator itself is returned by a
`Handler` action, so that additional IO may be done in the creation of one.

``` haskell
type StreamAPI = "userStream" :> StreamGet NewlineFraming JSON (SourceIO User)
streamAPI :: Proxy StreamAPI
streamAPI = Proxy

streamUsers :: SourceIO User
streamUsers = source [isaac, albert, albert]

app6 :: Application
app6 = serve streamAPI (return streamUsers)
```

This simple application returns a stream of `User` values encoded in JSON
format, with each value separated by a newline. In this case, the stream will
consist of the value of `isaac`, followed by the value of `albert`, then the
value of `albert` a second time. Importantly, the stream is written back as
results are produced, rather than all at once. This means first that results
are delivered when they are available, and second, that if an exception
interrupts production of the full stream, nonetheless partial results have
already been written back.

## Conclusion

You're now equipped to write webservices/web-applications using
**servant**. The rest of this document focuses on **servant-client**,
**servant-js** and **servant-docs**.
