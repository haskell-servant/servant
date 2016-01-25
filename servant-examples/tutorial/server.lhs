---
title: Serving an API
toc: true
---

Enough chit-chat about type-level combinators and representing an API as a
type. Can we have a webservice already?

If you want to follow along with the code and run the examples while you read this guide:

``` bash
cabal get servant-examples
cd servant-examples-<VERSION>
cabal sandbox init
cabal install --dependencies-only
cabal configure && cabal build
```

This will produce a `tutorial` executable in the
`dist/build/tutorial` directory that just runs the example corresponding
to the number specified as a command line argument:

``` bash
$ dist/build/tutorial/tutorial
Usage:   tutorial N
        where N is the number of the example you want to run.
```

A first example
===============

Equipped with some basic knowledge about the way we represent API, let's now write our first webservice.

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
>
> module Server where
>
> import Control.Monad.IO.Class
> import Control.Monad.Reader
> import Control.Monad.Trans.Either
> import Data.Aeson
> import Data.Aeson.Types
> import Data.Attoparsec.ByteString
> import Data.ByteString (ByteString)
> import Data.Int
> import Data.List
> import Data.String.Conversions
> import Data.Time.Calendar
> import GHC.Generics
> import Lucid
> import Network.HTTP.Media ((//), (/:))
> import Network.Wai
> import Network.Wai.Handler.Warp
> import Servant
> import System.Directory
> import Text.Blaze
> import Text.Blaze.Html.Renderer.Utf8
> import qualified Data.Aeson.Parser
> import qualified Text.Blaze.Html

``` haskell
{-# LANGUAGE TypeFamilies #-}
```

**Important**: the `Servant` module comes from the *servant-server* package, the one that lets us run webservers that implement a particular API type. It reexports all the types from the *servant* package that let you declare API types as well as everything you need to turn your request handlers into a fully-fledged webserver. This means that in your applications, you can just add *servant-server* as a dependency, import `Servant` and not worry about anything else.

We will write a server that will serve the following API.

> type UserAPI1 = "users" :> Get '[JSON] [User]

Here's what we would like to see when making a GET request to `/users`.

``` javascript
[ {"name": "Isaac Newton", "age": 372, "email": "isaac@newton.co.uk", "registration_date": "1683-03-01"}
, {"name": "Albert Einstein", "age": 136, "email": "ae@mc2.org", "registration_date": "1905-12-01"}
]
```

Now let's define our `User` data type and write some instances for it.

> data User = User
>   { name :: String
>   , age :: Int
>   , email :: String
>   , registration_date :: Day
>   } deriving (Eq, Show, Generic)
>
> instance ToJSON User

Nothing funny going on here. But we now can define our list of two users.

> users1 :: [User]
> users1 =
>   [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
>   , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
>   ]

Let's also write our API type.

``` haskell
type UserAPI1 = "users" :> Get '[JSON] [User]
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
by default run in the `EitherT ServantErr IO` monad. This is overridable very
easily, as explained near the end of this guide. Third thing, the type of the
value returned in that monad must be the same as the second argument of the
HTTP method combinator used for the corresponding endpoint. In our case, it
means we must provide a handler of type `EitherT ServantErr IO [User]`. Well,
we have a monad, let's just `return` our list:

> server1 :: Server UserAPI1
> server1 = return users1

That's it. Now we can turn `server` into an actual webserver using [wai](http://hackage.haskell.org/package/wai) and [warp](http://hackage.haskell.org/package/warp):

> userAPI :: Proxy UserAPI1
> userAPI = Proxy
>
> -- 'serve' comes from servant and hands you a WAI Application,
> -- which you can think of as an "abstract" web application,
> -- not yet a webserver.
> app1 :: Application
> app1 = serve userAPI server1

The `userAPI` bit is, alas, boilerplate (we need it to guide type inference).
But that's about as much boilerplate as you get.

And we're done! Let's run our webservice on the port 8081.

> main :: IO ()
> main = run 8081 app1

You can put this all into a file or just grab [servant's
repo](http://github.com/haskell-servant/servant) and look at the
*servant-examples* directory. The code we have just explored is in
*tutorial/T1.hs*, runnable with
`dist/build/tutorial/tutorial 1`.

If you run it, you can go to `http://localhost:8081/users` in your browser or
query it with curl and you see:

``` bash
$ curl http://localhost:8081/users
[{"email":"isaac@newton.co.uk","registration_date":"1683-03-01","age":372,"name":"Isaac Newton"},{"email":"ae@mc2.org","registration_date":"1905-12-01","age":136,"name":"Albert Einstein"}]
```

More endpoints
==============

What if we want more than one endpoint? Let's add `/albert` and `/isaac` to view the corresponding users encoded in JSON.

> type UserAPI2 = "users" :> Get '[JSON] [User]
>            :<|> "albert" :> Get '[JSON] User
>            :<|> "isaac" :> Get '[JSON] User

And let's adapt our code a bit.

> isaac :: User
> isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
>
> albert :: User
> albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
>
> users2 :: [User]
> users2 = [isaac, albert]

Now, just like we separate the various endpoints in `UserAPI` with `:<|>`, we
are going to separate the handlers with `:<|>` too! They must be provided in
the same order as the one they appear in in the API type.

> server2 :: Server UserAPI2
> server2 = return users2
>      :<|> return albert
>      :<|> return isaac

And that's it! You can run this example with
`dist/build/tutorial/tutorial 2` and check out the data available
at `/users`, `/albert` and `/isaac`.

From combinators to handler arguments
=====================================

Fine, we can write trivial webservices easily, but none of the two above use
any "fancy" combinator from servant. Let's address this and use `QueryParam`,
`Capture` and `ReqBody` right away. You'll see how each occurence of these
combinators in an endpoint makes the corresponding handler receive an
argument of the appropriate type automatically. You don't have to worry about
manually looking up URL captures or query string parameters, or
decoding/encoding data from/to JSON. Never.

We are going to use the following data types and functions to implement a server for `API`.

> type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
>       :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
>       :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
>
> data Position = Position
>   { x :: Int
>   , y :: Int
>   } deriving Generic
>
> instance ToJSON Position
>
> newtype HelloMessage = HelloMessage { msg :: String }
>   deriving Generic
>
> instance ToJSON HelloMessage
>
> data ClientInfo = ClientInfo
>   { clientName :: String
>   , clientEmail :: String
>   , clientAge :: Int
>   , clientInterestedIn :: [String]
>   } deriving Generic
>
> instance FromJSON ClientInfo
> instance ToJSON ClientInfo
>
> data Email = Email
>   { from :: String
>   , to :: String
>   , subject :: String
>   , body :: String
>   } deriving Generic
>
> instance ToJSON Email
>
> emailForClient :: ClientInfo -> Email
> emailForClient c = Email from' to' subject' body'
>
>   where from'    = "great@company.com"
>         to'      = clientEmail c
>         subject' = "Hey " ++ clientName c ++ ", we miss you!"
>         body'    = "Hi " ++ clientName c ++ ",\n\n"
>                 ++ "Since you've recently turned " ++ show (clientAge c)
>                 ++ ", have you checked out our latest "
>                 ++ intercalate ", " (clientInterestedIn c)
>                 ++ " products? Give us a visit!"

We can implement handlers for the three endpoints:

> server3 :: Server API
> server3 = position
>      :<|> hello
>      :<|> marketing
>
>   where position :: Int -> Int -> EitherT ServantErr IO Position
>         position x y = return (Position x y)
>
>         hello :: Maybe String -> EitherT ServantErr IO HelloMessage
>         hello mname = return . HelloMessage $ case mname of
>           Nothing -> "Hello, anonymous coward"
>           Just n  -> "Hello, " ++ n
>
>         marketing :: ClientInfo -> EitherT ServantErr IO Email
>         marketing clientinfo = return (emailForClient clientinfo)

Did you see that? The types for your handlers changed to be just what we
needed! In particular:

  - a `Capture "something" a` becomes an argument of type `a` (for `position`);
  - a `QueryParam "something" a` becomes an argument of type `Maybe a` (because
an endpoint can technically be accessed without specifying any query
string parameter, we decided to "force" handlers to be aware that the
parameter might not always be there);

  - a `ReqBody contentTypeList a` becomes an argument of type `a`;

And that's it. You can see this example in action by running `dist/build/tutorial/tutorial 3`.

``` bash
$ curl http://localhost:8081/position/1/2
{"x":1,"y":2}
$ curl http://localhost:8081/hello
{"msg":"Hello, anonymous coward"}
$ curl http://localhost:8081/hello?name=Alp
{"msg":"Hello, Alp"}
$ curl -X POST -d '{"name":"Alp Mestanogullari", "email" : "alp@foo.com", "age": 25, "interested_in": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing
{"subject":"Hey Alp Mestanogullari, we miss you!","body":"Hi Alp Mestanogullari,\n\nSince you've recently turned 25, have you checked out our latest haskell, mathematics products? Give us a visit!","to":"alp@foo.com","from":"great@company.com"}
```

For reference, here's a list of some combinators from *servant* and for those
that get turned into arguments to the handlers, the type of the argument.

 > - `Delete`, `Get`, `Patch`, `Post`, `Put`: these do not become arguments. They provide the return type of handlers, which usually is `EitherT ServantErr IO <something>`.
 > - `Capture "something" a` becomes an argument of type `a`.
 > - `QueryParam "something" a`, `MatrixParam "something" a`, `Header "something" a` all become arguments of type `Maybe a`, because there might be no value at all specified by the client for these.
 > - `QueryFlag "something"` and `MatrixFlag "something"` get turned into arguments of type `Bool`.
 > - `QueryParams "something" a` and `MatrixParams "something" a` get turned into arguments of type `[a]`.
 > - `ReqBody contentTypes a` gets turned into an argument of type `a`.

The `FromText`/`ToText` classes
===============================

Wait... How does *servant* know how to decode the `Int`s from the URL? Or how
to decode a `ClientInfo` value from the request body? This is what this and the
following two sections address.

`Capture`s and `QueryParam`s are represented by some textual value in URLs.
`Header`s are similarly represented by a pair of a header name and a
corresponding (textual) value in the request's "metadata". This is why we
decided to provide a pair of typeclasses, `FromText` and `ToText` which just
let you say that you can respectively *extract* or *encode* values of some type
*from*/*to* text. Here are the definitions:

``` haskell
class FromText a where
  fromText :: Text -> Maybe a

class ToText a where
  toText :: a -> Text
```

And as long as the type that a `Capture`/`QueryParam`/`Header`/etc will be
decoded to provides a `FromText` instance, it will Just Work. *servant*
provides a decent number of instances, but here are some examples of defining
your own.

> -- A typical enumeration
> data Direction
>   = Up
>   | Down
>   | Left
>   | Right
>
> instance FromText Direction where
>   -- requires {-# LANGUAGE OverloadedStrings #-}
>   fromText "up"    = Just Up
>   fromText "down"  = Just Down
>   fromText "left"  = Just Server.Left
>   fromText "right" = Just Server.Right
>   fromText       _ = Nothing
>
> instance ToText Direction where
>   toText Up           = "up"
>   toText Down         = "down"
>   toText Server.Left  = "left"
>   toText Server.Right = "right"
>
> newtype UserId = UserId Int64
>   deriving (FromText, ToText)

or writing the instances by hand:

``` haskell
instance FromText UserId where
  fromText = fmap UserId fromText

instance ToText UserId where
  toText (UserId i) = toText i
```

There's not much else to say about these classes. You will need instances for
them when using `Capture`, `QueryParam`, `QueryParams`, `MatrixParam`,
`MatrixParams` and `Header` with your types. You will need `FromText` instances
for server-side request handlers and `ToText` instances only when using
*servant-client*, as described in the [section about deriving haskell
functions to query an API](/tutorial/client.html).

Using content-types with your data types
========================================

The same principle was operating when decoding request bodies from JSON, and
responses *into* JSON. (JSON is just the running example - you can do this with
any content-type.)

This section introduces a couple of typeclasses provided by *servant* that make
all of this work.

The truth behind `JSON`
-----------------------

What exactly is `JSON`? Like the 3 other content types provided out of the box
by *servant*, it's a really dumb data type.

``` haskell
data JSON
data PlainText
data FormUrlEncoded
data OctetStream
```

Obviously, this is not all there is to `JSON`, otherwise it would be quite
pointless. Like most of the data types in *servant*, `JSON` is mostly there as
a special *symbol* that's associated with encoding (resp. decoding) to (resp.
from) the *JSON* format. The way this association is performed can be
decomposed into two steps.

The first step is to provide a proper
[`MediaType`](https://hackage.haskell.org/package/http-media-0.6.2/docs/Network-HTTP-Media.html)
representation for `JSON`, or for your own content types. If you look at the
haddocks from this link, you can see that we just have to specify
`application/json` using the appropriate functions. In our case, we can just
use `(//) :: ByteString -> ByteString -> MediaType`. The precise way to specify
the `MediaType` is to write an instance for the `Accept` class:

``` haskell
-- for reference:
class Accept ctype where
    contentType   :: Proxy ctype -> MediaType

instance Accept JSON where
    contentType _ = "application" // "json"
```

The second step is centered around the `MimeRender` and `MimeUnrender` classes.
These classes just let you specify a way to respectively encode and decode
values respectively into or from your content-type's representation.

``` haskell
class Accept ctype => MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> ByteString
    -- alternatively readable as:
    mimeRender  :: Proxy ctype -> (a -> ByteString)
```

Given a content-type and some user type, `MimeRender` provides a function that
encodes values of type `a` to lazy `ByteString`s.

In the case of `JSON`, this is easily dealt with! For any type `a` with a
`ToJSON` instance, we can render values of that type to JSON using
`Data.Aeson.encode`.

``` haskell
instance ToJSON a => MimeRender JSON a where
  mimeRender _ = encode
```

And now the `MimeUnrender` class, which lets us extract values from lazy
`ByteString`s, alternatively failing with an error string.

``` haskell
class Accept ctype => MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> ByteString -> Either String a
    -- alternatively:
    mimeUnrender :: Proxy ctype -> (ByteString -> Either String a)
```

We don't have much work to do there either, `Data.Aeson.eitherDecode` is
precisely what we need. However, it only allows arrays and objects as toplevel
JSON values and this has proven to get in our way more than help us so we wrote
our own little function around *aeson* and *attoparsec* that allows any type of
JSON value at the toplevel of a "JSON document". Here's the definition in case
you are curious.

> eitherDecodeLenient :: FromJSON a => ByteString -> Either String a
> eitherDecodeLenient input = do
>     v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
>     parseEither parseJSON v

This function is exactly what we need for our `MimeUnrender` instance.

``` haskell
instance FromJSON a => MimeUnrender JSON a where
    mimeUnrender _ = eitherDecodeLenient
```

And this is all the code that lets you use `JSON` for with `ReqBody`, `Get`,
`Post` and friends. We can check our understanding by implementing support
for an `HTML` content type, so that users of your webservice can access an
HTML representation of the data they want, ready to be included in any HTML
document, e.g. using [jQuery's `load` function](https://api.jquery.com/load/), simply by adding `Accept:
text/html` to their request headers.

Case-studies: *servant-blaze* and *servant-lucid*
-------------------------------------------------

These days, most of the haskellers who write their HTML UIs directly from
Haskell use either [blaze-html](http://hackage.haskell.org/package/blaze-html)
or [lucid](http://hackage.haskell.org/package/lucid). The best option for
*servant* is obviously to support both (and hopefully other templating
solutions!).

> data HTMLLucid

Once again, the data type is just there as a symbol for the encoding/decoding
functions, except that this time we will only worry about encoding since
*blaze-html* and *lucid* don't provide a way to extract data from HTML.

Both packages also have the same `Accept` instance for their `HTMLLucid` type.

> instance Accept HTMLLucid where
>     contentType _ = "text" // "html" /: ("charset", "utf-8")

Note that this instance uses the `(/:)` operator from *http-media* which lets
us specify additional information about a content-type, like the charset here.

The rendering instances for both packages both call similar functions that take
types with an appropriate instance to an "abstract" HTML representation and
then write that to a `ByteString`.

For *lucid*:

> instance ToHtml a => MimeRender HTMLLucid a where
>     mimeRender _ = renderBS . toHtml
>
> -- let's also provide an instance for lucid's
> -- 'Html' wrapper.
> instance MimeRender HTMLLucid (Html a) where
>     mimeRender _ = renderBS

For *blaze-html*:

> -- For this tutorial to compile 'HTMLLucid' and 'HTMLBlaze' have to be
> -- distinct. Usually you would stick to one html rendering library and then
> -- you can go with one 'HTML' type.
> data HTMLBlaze
>
> instance Accept HTMLBlaze where
>     contentType _ = "text" // "html" /: ("charset", "utf-8")
>
> instance ToMarkup a => MimeRender HTMLBlaze a where
>     mimeRender _ = renderHtml . Text.Blaze.Html.toHtml
>
> -- while we're at it, just like for lucid we can
> -- provide an instance for rendering blaze's 'Html' type
> instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
>     mimeRender _ = renderHtml

Both [servant-blaze](http://hackage.haskell.org/package/servant-blaze) and
[servant-lucid](http://hackage.haskell.org/package/servant-lucid) let you use
`HTMLLucid` in any content type list as long as you provide an instance of the
appropriate class (`ToMarkup` for *blaze-html*, `ToHtml` for *lucid*).

We can now write webservice that uses *servant-lucid* to show the `HTMLLucid`
content type in action. First off, imports and pragmas as usual.

We will be serving the following API:

> type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

where `Person` is defined as follows:

> data Person = Person
>   { firstName :: String
>   , lastName  :: String
>   } deriving Generic -- for the JSON instance
>
> instance ToJSON Person

Now, let's teach *lucid* how to render a `Person` as a row in a table, and then
a list of `Person`s as a table with a row per person.

> -- HTML serialization of a single person
> instance ToHtml Person where
>   toHtml person =
>     tr_ $ do
>       td_ (toHtml $ firstName person)
>       td_ (toHtml $ lastName person)
>
>   -- do not worry too much about this
>   toHtmlRaw = toHtml
>
> -- HTML serialization of a list of persons
> instance ToHtml [Person] where
>   toHtml persons = table_ $ do
>     tr_ $ do
>       th_ "first name"
>       th_ "last name"
>
>     -- this just calls toHtml on each person of the list
>     -- and concatenates the resulting pieces of HTML together
>     foldMap toHtml persons
>
>   toHtmlRaw = toHtml

We create some `Person` values and serve them as a list:

> persons :: [Person]
> persons =
>   [ Person "Isaac"  "Newton"
>   , Person "Albert" "Einstein"
>   ]
>
> personAPI :: Proxy PersonAPI
> personAPI = Proxy
>
> server4 :: Server PersonAPI
> server4 = return persons
>
> app2 :: Application
> app2 = serve personAPI server4

And we're good to go. You can run this example with `dist/build/tutorial/tutorial 4`.

``` bash
 $ curl http://localhost:8081/persons
 [{"lastName":"Newton","firstName":"Isaac"},{"lastName":"Einstein","firstName":"Albert"}]
 $ curl -H 'Accept: text/html' http://localhost:8081/persons
 <table><tr><td>first name</td><td>last name</td></tr><tr><td>Isaac</td><td>Newton</td></tr><tr><td>Albert</td><td>Einstein</td></tr></table>
 # or just point your browser to http://localhost:8081/persons
```

The `EitherT ServantErr IO` monad
=================================

At the heart of the handlers is the monad they run in, namely `EitherT
ServantErr IO`. One might wonder: why this monad? The answer is that it is the
simplest monad with the following properties:

- it lets us both return a successful result (with the `Right` branch of
`Either`) or "fail" with a descriptive error (with the `Left` branch of
`Either`);
- it lets us perform IO, which is absolutely vital since most webservices exist
as interfaces to databases that we interact with in `IO`;

Let's recall some definitions.

``` haskell
-- from the Prelude
data Either e a = Left e | Right a

-- from the 'either' package at
-- http://hackage.haskell.org/package/either-4.3.3.2/docs/Control-Monad-Trans-Either.html
newtype EitherT e m a
  = EitherT { runEitherT :: m (Either e a) }
```

In short, this means that a handler of type `EitherT ServantErr IO a` is simply
equivalent to a computation of type `IO (Either ServantErr a)`, that is, an IO
action that either returns an error or a result.

The aforementioned `either` package is worth taking a look at. Perhaps most
importantly:

``` haskell
left :: Monad m => e -> EitherT e m a
```
Allows you to return an error from your handler (whereas `return` is enough to
return a success).

Most of what you'll be doing in your handlers is running some IO and,
depending on the result, you might sometimes want to throw an error of some
kind and abort early. The next two sections cover how to do just that.

Performing IO
-------------

Another important instance from the list above is `MonadIO m => MonadIO (EitherT e m)`. [`MonadIO`](http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-IO-Class.html) is a class from the *transformers* package defined as:

``` haskell
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
```

Obviously, the `IO` monad provides a `MonadIO` instance. Hence for any type `e`, `EitherT e IO` has a `MonadIO` instance. So if you want to run any kind of IO computation in your handlers, just use `liftIO`:

> type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent
>
> newtype FileContent = FileContent
>   { content :: String }
>   deriving Generic
>
> instance ToJSON FileContent
>
> server5 :: Server IOAPI1
> server5 = do
>   filecontent <- liftIO (readFile "myfile.txt")
>   return (FileContent filecontent)

Failing, through `ServantErr`
-----------------------------

If you want to explicitly fail at providing the result promised by an endpoint
using the appropriate HTTP status code (not found, unauthorized, etc) and some
error message, all you have to do is use the `left` function mentioned above
and provide it with the appropriate value of type `ServantErr`, which is
defined as:

``` haskell
data ServantErr = ServantErr
    { errHTTPCode     :: Int
    , errReasonPhrase :: String
    , errBody         :: ByteString -- lazy bytestring
    , errHeaders      :: [Header]
    }
```

Many standard values are provided out of the box by the `Servant.Server`
module.  If you want to use these values but add a body or some headers, just
use record update syntax:

> failingHandler :: EitherT ServantErr IO ()
> failingHandler = left myerr
>
>   where myerr :: ServantErr
>         myerr = err503 { errBody = "Sorry dear user." }

Here's an example where we return a customised 404-Not-Found error message in
the response body if "myfile.txt" isn't there:

> server6 :: Server IOAPI1
> server6 = do
>   exists <- liftIO (doesFileExist "myfile.txt")
>   if exists
>     then liftIO (readFile "myfile.txt") >>= return . FileContent
>     else left custom404Err
>
>   where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }

Let's run this server (`dist/build/tutorial/tutorial 5`) and
query it, first without the file and then with the file.

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
 myfile.txt just isnt there, please leave this server alone.

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

Response headers
================

To add headers to your response, use [addHeader](http://hackage.haskell.org/package/servant-0.4.4/docs/Servant-API-ResponseHeaders.html).
Note that this changes the type of your API, as we can see in the following example:

> type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)
>
> myHandler :: Server MyHandler
> myHandler = return $ addHeader 1797 albert


Serving static files
====================

*servant-server* also provides a way to just serve the content of a directory
under some path in your web API. As mentioned earlier in this document, the
`Raw` combinator can be used in your APIs to mean "plug here any WAI
application". Well, servant-server provides a function to get a file and
directory serving WAI application, namely:

``` haskell
-- exported by Servant and Servant.Server
serveDirectory :: FilePath -> Server Raw
```

`serveDirectory`'s argument must be a path to a valid directory. You can see an
example below, runnable with `dist/build/tutorial/tutorial 6`
(you **must** run it from within the *servant-examples/* directory!), which is
a webserver that serves the various bits of code covered in this
getting-started.

The API type will be the following.

> type CodeAPI = "code" :> Raw

And the server:

> codeAPI :: Proxy CodeAPI
> codeAPI = Proxy

> server7 :: Server CodeAPI
> server7 = serveDirectory "tutorial"
>
> app3 :: Application
> app3 = serve codeAPI server7

This server will match any request whose path starts with `/code` and will look for a file at the path described by the rest of the request path, inside the *tutorial/* directory of the path you run the program from.

In other words:

- If a client requests `/code/foo.txt`, the server will look for a file at `./tutorial/foo.txt` (and fail)
- If a client requests `/code/T1.hs`, the server will look for a file at `./tutorial/T1.hs` (and succeed)
- If a client requests `/code/foo/bar/baz/movie.mp4`, the server will look for a file at `./tutorial/foo/bar/baz/movie.mp4` (and fail)

Here is our little server in action.

``` haskell
$ curl http://localhost:8081/code/T1.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module T1 where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Servant

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

-- orphan ToJSON instance for Day. necessary to derive one for User
instance ToJSON Day where
  -- display a day in YYYY-mm-dd format
  toJSON d = toJSON (showGregorian d)

instance ToJSON User

type UserAPI = "users" :> Get '[JSON] [User]

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = return users

app :: Application
app = serve userAPI server
$ curl http://localhost:8081/code/tutorial.hs
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import qualified T1
import qualified T2
import qualified T3
import qualified T4
import qualified T5
import qualified T6
import qualified T7
import qualified T9
import qualified T10

app :: String -> (Application -> IO ()) -> IO ()
app n f = case n of
  "1" -> f T1.app
  "2" -> f T2.app
  "3" -> f T3.app
  "4" -> f T4.app
  "5" -> f T5.app
  "6" -> f T6.app
  "7" -> f T7.app
  "8" -> f T3.app
  "9" -> T9.writeJSFiles >> f T9.app
  "10" -> f T10.app
  _   -> usage

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> app n (run 8081)
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:\t tutorial N"
  putStrLn "\t\twhere N is the number of the example you want to run."

$ curl http://localhost:8081/foo
not found
```

Nested APIs
===========

Let's see how you can define APIs in a modular way, while avoiding repetition. Consider this simple example:

> type UserAPI3 = -- view the user with given userid, in JSON
>                 Capture "userid" Int :> Get '[JSON] User
>
>            :<|> -- delete the user with given userid. empty response
>                 Capture "userid" Int :> Delete '[] ()

We can instead factor out the `userid`:

> type UserAPI4 = Capture "userid" Int :>
>   (    Get '[JSON] User
>   :<|> Delete '[] ()
>   )

However, you have to be aware that this has an effect on the type of the corresponding `Server`:

``` haskell
Server UserAPI3 = (Int -> EitherT ServantErr IO User)
             :<|> (Int -> EitherT ServantErr IO ())

Server UserAPI4 = Int -> (    EitherT ServantErr IO User
                         :<|> EitherT ServantErr IO ()
                         )
```

In the first case, each handler receives the *userid* argument. In the latter,
the whole `Server` takes the *userid* and has handlers that are just computations in `EitherT`, with no arguments. In other words:

> server8 :: Server UserAPI3
> server8 = getUser :<|> deleteUser
>
>   where getUser :: Int -> EitherT ServantErr IO User
>         getUser _userid = error "..."
>
>         deleteUser :: Int -> EitherT ServantErr IO ()
>         deleteUser _userid = error "..."
>
> -- notice how getUser and deleteUser
> -- have a different type! no argument anymore,
> -- the argument directly goes to the whole Server
> server9 :: Server UserAPI4
> server9 userid = getUser userid :<|> deleteUser userid
>
>   where getUser :: Int -> EitherT ServantErr IO User
>         getUser = error "..."
>
>         deleteUser :: Int -> EitherT ServantErr IO ()
>         deleteUser = error "..."

Note that there's nothing special about `Capture` that lets you "factor it out": this can be done with any combinator. Here are a few examples of APIs with a combinator factored out for which we can write a perfectly valid `Server`.

> -- we just factor out the "users" path fragment
> type API1 = "users" :>
>   (    Get '[JSON] [User] -- user listing
>   :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
>   )
>
> -- we factor out the Request Body
> type API2 = ReqBody '[JSON] User :>
>   (    Get '[JSON] User -- just display the same user back, don't register it
>   :<|> Post '[JSON] ()  -- register the user. empty response
>   )
>
> -- we factor out a Header
> type API3 = Header "Authorization" Token :>
>   (    Get '[JSON] SecretData -- get some secret data, if authorized
>   :<|> ReqBody '[JSON] SecretData :> Post '[] () -- add some secret data, if authorized
>   )
>
> newtype Token = Token ByteString
> newtype SecretData = SecretData ByteString

This approach lets you define APIs modularly and assemble them all into one big API type only at the end.

> type UsersAPI =
>        Get '[JSON] [User] -- list users
>   :<|> ReqBody '[JSON] User :> Post '[] () -- add a user
>   :<|> Capture "userid" Int :>
>          ( Get '[JSON] User -- view a user
>       :<|> ReqBody '[JSON] User :> Put '[] () -- update a user
>       :<|> Delete '[] () -- delete a user
>          )
>
> usersServer :: Server UsersAPI
> usersServer = getUsers :<|> newUser :<|> userOperations
>
>   where getUsers :: EitherT ServantErr IO [User]
>         getUsers = error "..."
>
>         newUser :: User -> EitherT ServantErr IO ()
>         newUser = error "..."
>
>         userOperations userid =
>           viewUser userid :<|> updateUser userid :<|> deleteUser userid
>
>           where
>             viewUser :: Int -> EitherT ServantErr IO User
>             viewUser = error "..."
>
>             updateUser :: Int -> User -> EitherT ServantErr IO ()
>             updateUser = error "..."
>
>             deleteUser :: Int -> EitherT ServantErr IO ()
>             deleteUser = error "..."

> type ProductsAPI =
>        Get '[JSON] [Product] -- list products
>   :<|> ReqBody '[JSON] Product :> Post '[] () -- add a product
>   :<|> Capture "productid" Int :>
>          ( Get '[JSON] Product -- view a product
>       :<|> ReqBody '[JSON] Product :> Put '[] () -- update a product
>       :<|> Delete '[] () -- delete a product
>          )
>
> data Product = Product { productId :: Int }
>
> productsServer :: Server ProductsAPI
> productsServer = getProducts :<|> newProduct :<|> productOperations
>
>   where getProducts :: EitherT ServantErr IO [Product]
>         getProducts = error "..."
>
>         newProduct :: Product -> EitherT ServantErr IO ()
>         newProduct = error "..."
>
>         productOperations productid =
>           viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
>
>           where
>             viewProduct :: Int -> EitherT ServantErr IO Product
>             viewProduct = error "..."
>
>             updateProduct :: Int -> Product -> EitherT ServantErr IO ()
>             updateProduct = error "..."
>
>             deleteProduct :: Int -> EitherT ServantErr IO ()
>             deleteProduct = error "..."

> type CombinedAPI = "users" :> UsersAPI
>               :<|> "products" :> ProductsAPI
>
> server10 :: Server CombinedAPI
> server10 = usersServer :<|> productsServer

Finally, we can realize the user and product APIs are quite similar and abstract that away:

> -- API for values of type 'a'
> -- indexed by values of type 'i'
> type APIFor a i =
>        Get '[JSON] [a] -- list 'a's
>   :<|> ReqBody '[JSON] a :> Post '[] () -- add an 'a'
>   :<|> Capture "id" i :>
>          ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
>       :<|> ReqBody '[JSON] a :> Put '[] () -- update an 'a'
>       :<|> Delete '[] () -- delete an 'a'
>          )
>
> -- Build the appropriate 'Server'
> -- given the handlers of the right type.
> serverFor :: EitherT ServantErr IO [a] -- handler for listing of 'a's
>           -> (a -> EitherT ServantErr IO ()) -- handler for adding an 'a'
>           -> (i -> EitherT ServantErr IO a) -- handler for viewing an 'a' given its identifier of type 'i'
>           -> (i -> a -> EitherT ServantErr IO ()) -- updating an 'a' with given id
>           -> (i -> EitherT ServantErr IO ()) -- deleting an 'a' given its id
>           -> Server (APIFor a i)
> serverFor = error "..."
> -- implementation left as an exercise. contact us on IRC
> -- or the mailing list if you get stuck!

Using another monad for your handlers
=====================================

Remember how `Server` turns combinators for HTTP methods into `EitherT ServantErr IO`? Well, actually, there's more to that. `Server` is actually a simple type synonym.

``` haskell
type Server api = ServerT api (EitherT ServantErr IO)
```

`ServerT` is the actual type family that computes the required types for the handlers that's part of the `HasServer` class. It's like `Server` except that it takes a third parameter which is the monad you want your handlers to run in, or more generally the return types of your handlers. This third parameter is used for specifying the return type of the handler for an endpoint, e.g when computing `ServerT (Get '[JSON] Person) SomeMonad`. The result would be `SomeMonad Person`.

The first and main question one might have then is: how do we write handlers that run in another monad? How can we "bring back" the value from a given monad into something *servant* can understand?

Natural transformations
-----------------------

If we have a function that gets us from an `m a` to an `n a`, for any `a`, what
do we have?

``` haskell
newtype m :~> n = Nat { unNat :: forall a. m a -> n a}

-- For example
-- listToMaybeNat ::`[] :~> Maybe`
-- listToMaybeNat = Nat listToMaybe  -- from Data.Maybe
```
(`Nat` comes from "natural transformation", in case you're wondering.)

So if you want to write handlers using another monad/type than `EitherT
ServantErr IO`, say the `Reader String` monad, the first thing you have to
prepare is a function:

``` haskell
readerToEither :: Reader String :~> EitherT ServantErr IO
```

Let's start with `readerToEither'`. We obviously have to run the `Reader`
computation by supplying it with a `String`, like `"hi"`. We get an `a` out
from that and can then just `return` it into `EitherT`. We can then just wrap
that function with the `Nat` constructor to make it have the fancier type.

> readerToEither' :: forall a. Reader String a -> EitherT ServantErr IO a
> readerToEither' r = return (runReader r "hi")
>
> readerToEither :: Reader String :~> EitherT ServantErr IO
> readerToEither = Nat readerToEither'

We can write some simple webservice with the handlers running in `Reader String`.

> type ReaderAPI = "a" :> Get '[JSON] Int
>             :<|> "b" :> Get '[JSON] String
>
> readerAPI :: Proxy ReaderAPI
> readerAPI = Proxy
>
> readerServerT :: ServerT ReaderAPI (Reader String)
> readerServerT = a :<|> b
>
>   where a :: Reader String Int
>         a = return 1797
>
>         b :: Reader String String
>         b = ask

We unfortunately can't use `readerServerT` as an argument of `serve`, because
`serve` wants a `Server ReaderAPI`, i.e., with handlers running in `EitherT
ServantErr IO`. But there's a simple solution to this.

Enter `enter`
-------------

That's right. We have just written `readerToEither`, which is exactly what we
would need to apply to the results of all handlers to make the handlers have the
right type for `serve`. Being cumbersome to do by hand, we provide a function
`enter` which takes a natural transformation between two parametrized types `m`
and `n` and a `ServerT someapi m`, and returns a `ServerT someapi n`.

In our case, we can wrap up our little webservice by using `enter readerToEither` on our handlers.

> readerServer :: Server ReaderAPI
> readerServer = enter readerToEither readerServerT
>
> app4 :: Application
> app4 = serve readerAPI readerServer

And we can indeed see this webservice in action by running `dist/build/tutorial/tutorial 7`.

``` bash
$ curl http://localhost:8081/a
1797
$ curl http://localhost:8081/b
"hi"
```

Conclusion
==========

You're now equipped to write any kind of webservice/web-application using *servant*. One thing not covered here is how to incorporate your own combinators and will be the topic of a page on the website. The rest of this document focuses on *servant-client*, *servant-jquery* and *servant-docs*.

<div style="text-align: center;">
  <p><a href="/tutorial/api-type.html">Previous page: A web API as a type</a></p>
  <p><a href="/tutorial/client.html">Next page: Deriving Haskell functions to query an API</a></p>
</div>
