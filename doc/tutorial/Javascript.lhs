# Generating Javascript functions to query an API

We will now see how **servant** lets you turn an API type into javascript
functions that you can call to query a webservice.

For this, we will consider a simple page divided in two parts. At the top, we
will have a search box that lets us search in a list of Haskell books by
author/title with a list of results that gets updated every time we enter or
remove a character, while at the bottom we will be able to see the classical
[probabilistic method to approximate
pi](http://en.wikipedia.org/wiki/Approximations_of_%CF%80#Summing_a_circle.27s_area),
using a webservice to get random points. Finally, we will serve an HTML file
along with a couple of Javascript files, among which one that's automatically
generated from the API type and which will provide ready-to-use functions to
query your API.

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Javascript where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import GHC.Generics
import Language.Javascript.JQuery
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Servant
import Servant.JS
import System.Random
```

Now let's have the API type(s) and the accompanying datatypes.

``` haskell
type API = "point" :> Get '[JSON] Point
      :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)

type API' = API :<|> Raw

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point

data Search a = Search
  { query   :: Text
  , results :: [a]
  } deriving Generic

mkSearch :: Text -> [a] -> Search a
mkSearch = Search

instance ToJSON a => ToJSON (Search a)

data Book = Book
  { author :: Text
  , title  :: Text
  , year   :: Int
  } deriving Generic

instance ToJSON Book

book :: Text -> Text -> Int -> Book
book = Book
```

We need a "book database". For the purpose of this guide, let's restrict
ourselves to the following books.

``` haskell
books :: [Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
  , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
  , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
  , book "Graham Hutton" "Programming in Haskell" 2007
  , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
  , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]
```

Now, given an optional search string `q`, we want to perform a case insensitive
search in that list of books. We're obviously not going to try and implement
the best possible algorithm, this is out of scope for this tutorial. The
following simple linear scan will do, given how small our list is.

``` haskell
searchBook :: Monad m => Maybe Text -> m (Search Book)
searchBook Nothing  = pure (mkSearch "" books)
searchBook (Just q) = pure (mkSearch q books')

  where books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
                            || q' `T.isInfixOf` T.toLower (title b)
                        )
                        books
        q' = T.toLower q
```

We also need an endpoint that generates random points `(x, y)` with `-1 <= x,y
<= 1`. The code below uses
[random](http://hackage.haskell.org/package/random)'s `System.Random`.

``` haskell
randomPoint :: MonadIO m => m Point
randomPoint = liftIO . getStdRandom $ \g ->
  let (rx, g')  = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
  in (Point rx ry, g'')
```

If we add static file serving, our server is now complete.

``` haskell
api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: Server API
server = randomPoint
    :<|> searchBook

server' :: Server API'
server' = server
     :<|> serveDirectoryFileServer "static"

app :: Application
app = serve api' server'

main :: IO ()
main = run 8000 app
```

Why two different API types, proxies and servers though? Simply because we
don't want to generate javascript functions for the `Raw` part of our API type,
so we need a `Proxy` for our API type `API'` without its `Raw` endpoint.

The `EmptyAPI` combinator needs no special treatment as it generates no
Javascript functions: an empty API has no endpoints to access.

Very similarly to how one can derive haskell functions, we can derive the
javascript with just a simple function call to `jsForAPI` from
`Servant.JS`.

``` haskell
apiJS1 :: Text
apiJS1 = jsForAPI api jquery
```

This `Text` contains 2 Javascript functions, 'getPoint' and 'getBooks':

``` javascript

var getPoint = function(onSuccess, onError)
{
  $.ajax(
    { url: '/point'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getBooks = function(q, onSuccess, onError)
{
  $.ajax(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
```

We created a directory `static` that contains two static files: `index.html`,
which is the entrypoint to our little web application; and `ui.js`, which
contains some hand-written javascript. This javascript code assumes the two
generated functions `getPoint` and `getBooks` in scope. Therefore we need to
write the generated javascript into a file:

``` haskell
writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiJS1
  jq <- T.readFile =<< Language.Javascript.JQuery.file
  T.writeFile "static/jq.js" jq
```

(We're also writing the jquery library into a file, as it's also used by
`ui.js`.) `static/api.js` will be included in `index.html` and the two
generated functions will therefore be available in `ui.js`.

And we're good to go. You can start the `main` function of this file and go to
`http://localhost:8000/`. Start typing in the name of one of the authors in our
database or part of a book title, and check out how long it takes to
approximate pi using the method mentioned above.

## Customizations

Instead of calling `jquery`, you can call its variant `jqueryWith`.
Here are the type definitions

```haskell ignore
jquery :: JavaScriptGenerator
jqueryWith :: CommonGeneratorOptions -> JavaScriptGenerator
```

The `CommonGeneratorOptions` will let you define different behaviors to
change how functions are generated. Here is the definition of currently
available options:

```haskell ignore
data CommonGeneratorOptions = CommonGeneratorOptions
  {
    -- | function generating function names
    functionNameBuilder :: FunctionName -> Text
    -- | name used when a user wants to send the request body (to let you redefine it)
  , requestBody :: Text
    -- | name of the callback parameter when the request was successful
  , successCallback :: Text
    -- | name of the callback parameter when the request reported an error
  , errorCallback :: Text
    -- | namespace on which we define the js function (empty means local var)
  , moduleName :: Text
    -- | a prefix that should be prepended to the URL in the generated JS
  , urlPrefix :: Text
  }
```

This pattern is available with all supported backends, and default values are provided.

## Vanilla support

If you don't use JQuery for your application, you can reduce your
dependencies to simply use the `XMLHttpRequest` object from the standard API.

Use the same code as before but simply replace the previous `apiJS` with
the following one:

``` haskell
apiJS2 :: Text
apiJS2 = jsForAPI api vanillaJS
```

The rest is *completely* unchanged.

The output file is a bit different, but it has the same parameters,

``` javascript


var getPoint = function(onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/point', true);
  xhr.setRequestHeader(\"Accept\",\"application/json\");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var getBooks = function(q, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/books' + '?q=' + encodeURIComponent(q), true);
  xhr.setRequestHeader(\"Accept\",\"application/json\");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}


```

And that's all, your web service can of course be accessible from those
two clients at the same time!

## Axios support

### Simple usage

If you use Axios library for your application, we support that too!

Use the same code as before but simply replace the previous `apiJS` with
the following one:

``` haskell
apiJS3 :: Text
apiJS3 = jsForAPI api $ axios defAxiosOptions
```

The rest is *completely* unchanged.

The output file is a bit different,

``` javascript


var getPoint = function()
{
  return axios({ url: '/point'
    , method: 'get'
    });
}



var getBooks = function(q)
{
  return axios({ url: '/books' + '?q=' + encodeURIComponent(q)
    , method: 'get'
    });
}

```

**Caution:** In order to support the promise style of the API, there are no onSuccess
nor onError callback functions.

### Defining Axios configuration

Axios lets you define a 'configuration' to determine the behavior of the
program when the AJAX request is sent.

We mapped this into a configuration

``` haskell
data AxiosOptions = AxiosOptions
  { -- | indicates whether or not cross-site Access-Control requests
    -- should be made using credentials
    withCredentials :: !Bool
    -- | the name of the cookie to use as a value for xsrf token
  , xsrfCookieName :: !(Maybe Text)
    -- | the name of the header to use as a value for xsrf token
  , xsrfHeaderName :: !(Maybe Text)
  }
```

## Angular support

### Simple usage

You can apply the same procedure as with `vanillaJS` and `jquery`, and
generate top level functions.

The difference is that `angular` Generator always takes an argument.

``` haskell
apiJS4 :: Text
apiJS4 = jsForAPI api $ angular defAngularOptions
```

The generated code will be a bit different than previous generators. An extra
argument `$http` will be added to let Angular magical Dependency Injector
operate.

**Caution:** In order to support the promise style of the API, there are no onSuccess
nor onError callback functions.

``` javascript


var getPoint = function($http)
{
  return $http(
    { url: '/point'
    , method: 'GET'
    });
}



var getBooks = function($http, q)
{
  return $http(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , method: 'GET'
    });
}

```

You can then build your controllers easily

``` javascript

app.controller("MyController", function($http) {
  this.getPoint = getPoint($http)
    .success(/* Do something */)
    .error(/* Report error */);

  this.getPoint = getBooks($http, q)
    .success(/* Do something */)
    .error(/* Report error */);
});
```

### Service generator

You can also generate automatically a service to wrap the whole API as
a single Angular service:

``` javascript
app.service('MyService', function($http) {
  return ({
  postCounter: function()
  {
   return $http(
     { url: '/counter'
     , method: 'POST'
      });
  },
  getCounter: function()
  {
   return $http(
     { url: '/books' + '?q=' + encodeURIComponent(q), true);
     , method: 'GET'
      });
  }
  });
});
```

To do so, you just have to use an alternate generator.

``` haskell
apiJS5 :: Text
apiJS5 = jsForAPI api $ angularService defAngularOptions
```

Again, it is possible to customize some portions with the options.

``` haskell
data AngularOptions = AngularOptions
  { -- | When generating code with wrapInService, name of the service to generate, default is 'app'
    serviceName :: Text
  , -- | beginning of the service definition
    prologue :: Text -> Text -> Text
  , -- | end of the service definition
    epilogue :: Text
  }
```

## Custom function name builder

Servant comes with three name builders included:

- camelCase (the default)
- concatCase
- snakeCase

Keeping the JQuery as an example, let's see the impact:

``` haskell
apiJS6 :: Text
apiJS6 = jsForAPI api $ jqueryWith defCommonGeneratorOptions { functionNameBuilder= snakeCase }
```

This `Text` contains 2 Javascript functions:

``` javascript


var get_point = function(onSuccess, onError)
{
  $.ajax(
    { url: '/point'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var get_books = function(q, onSuccess, onError)
{
  $.ajax(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

```
