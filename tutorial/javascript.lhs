---
title: Deriving Javascript functions to query an API
toc: true
---

We will now see how *servant* lets you turn an API type into javascript
functions that you can call to query a webservice. The derived code assumes you
use *jQuery* but you could very easily adapt the code to generate ajax requests
based on vanilla javascript or another library than *jQuery*.

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

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TypeOperators #-}
>
> module Javascript where
>
> import Control.Monad.IO.Class
> import Data.Aeson
> import Data.Proxy
> import Data.Text (Text)
> import qualified Data.Text as T
> import GHC.Generics
> import Language.Javascript.JQuery
> import Network.Wai
> import Servant
> import Servant.JQuery
> import System.Random

Now let's have the API type(s) and the accompanying datatypes.

> type API = "point" :> Get '[JSON] Point
>       :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)
>
> type API' = API :<|> Raw
>
> data Point = Point
>   { x :: Double
>   , y :: Double
>   } deriving Generic
>
> instance ToJSON Point
>
> data Search a = Search
>   { query   :: Text
>   , results :: [a]
>   } deriving Generic
>
> mkSearch :: Text -> [a] -> Search a
> mkSearch = Search
>
> instance ToJSON a => ToJSON (Search a)
>
> data Book = Book
>   { author :: Text
>   , title  :: Text
>   , year   :: Int
>   } deriving Generic
>
> instance ToJSON Book
>
> book :: Text -> Text -> Int -> Book
> book = Book

We need a "book database". For the purpose of this guide, let's restrict ourselves to the following books.

> books :: [Book]
> books =
>   [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
>   , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
>   , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
>   , book "Graham Hutton" "Programming in Haskell" 2007
>   , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
>   , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
>   ]

Now, given an optional search string `q`, we want to perform a case insensitive search in that list of books. We're obviously not going to try and implement the best possible algorithm, this is out of scope for this tutorial. The following simple linear scan will do, given how small our list is.

> searchBook :: Monad m => Maybe Text -> m (Search Book)
> searchBook Nothing  = return (mkSearch "" books)
> searchBook (Just q) = return (mkSearch q books')
>
>   where books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
>                             || q' `T.isInfixOf` T.toLower (title b)
>                         )
>                         books
>         q' = T.toLower q

We also need an endpoint that generates random points `(x, y)` with `-1 <= x,y <= 1`. The code below uses [random](http://hackage.haskell.org/package/random)'s `System.Random`.

> randomPoint :: MonadIO m => m Point
> randomPoint = liftIO . getStdRandom $ \g ->
>   let (rx, g')  = randomR (-1, 1) g
>       (ry, g'') = randomR (-1, 1) g'
>   in (Point rx ry, g'')

If we add static file serving, our server is now complete.

> api :: Proxy API
> api = Proxy
>
> api' :: Proxy API'
> api' = Proxy
>
> server :: Server API
> server = randomPoint
>     :<|> searchBook
>
> server' :: Server API'
> server' = server
>      :<|> serveDirectory "tutorial/t9"
>
> app :: Application
> app = serve api' server'

Why two different API types, proxies and servers though? Simply because we don't want to generate javascript functions for the `Raw` part of our API type, so we need a `Proxy` for our API type `API'` without its `Raw` endpoint.

Very similarly to how one can derive haskell functions, we can derive the javascript with just a simple function call to `jsForAPI` from `Servant.JQuery`.

> apiJS :: String
> apiJS = jsForAPI api

This `String` contains 2 Javascript functions:

``` javascript

function getpoint(onSuccess, onError)
{
  $.ajax(
    { url: '/point'
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
}

function getbooks(q, onSuccess, onError)
{
  $.ajax(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
}
```

Right before starting up our server, we will need to write this `String` to a file, say `api.js`, along with a copy of the *jQuery* library, as provided by the [js-jquery](http://hackage.haskell.org/package/js-jquery) package.

> writeJSFiles :: IO ()
> writeJSFiles = do
>   writeFile "getting-started/gs9/api.js" apiJS
>   jq <- readFile =<< Language.Javascript.JQuery.file
>   writeFile "getting-started/gs9/jq.js" jq

And we're good to go. Start the server with `dist/build/tutorial/tutorial 9` and go to `http://localhost:8081/`. Start typing in the name of one of the authors in our database or part of a book title, and check out how long it takes to approximate &pi; using the method mentioned above.

<div style="text-align: center;">
  <p><a href="/tutorial/client.html">Previous page: Deriving Haskell functions to query an API</a></p>
  <p><a href="/tutorial/docs.html">Next page: Generating documentation for APIs</a></p>
</div>
