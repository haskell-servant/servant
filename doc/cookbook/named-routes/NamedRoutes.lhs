# Record-based APIs: the nested case

*Available in Servant 0.19 or higher*

Servant offers a very natural way of constructing APIs with nested records, called `NamedRoutes`.

This cookbook explains how to implement such nested-record-based-APIs using
`NamedRoutes` through the example of a Movie Catalog.
If you don't need the nested aspect of the record-based API, you might want to look at [Record-based
APIs: the simple case](../generic/Generic.html) cookbook
which covers a simpler implementation in which every endpoint is on the same
level.

First, we start by constructing the domain types of our Movie Catalog.
After, we show you how to implement the API type with the NamedRoutes records.
Lastly, we make a Server and a Client out of the API type.

However, it should be understood that this cookbook does _not_ dwell on the
built-in servant combinators as the [Structuring APIs](<../structuring-apis/StructuringApis.html>) 
cookbook already covers that angle.


## Boilerplate time!

First, let’s get rid of the extensions and imports boilerplate in order to focus on our new technique:


```haskell
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

import GHC.Generics               ( Generic )
import Data.Aeson                 ( FromJSON, ToJSON )
import Data.Proxy                 ( Proxy(..) )
import Network.Wai.Handler.Warp   ( run )

import Servant                    ( NamedRoutes
                                  , Handler, serve )
import Servant.API                (Capture, Delete, Get, Put, QueryParam, ReqBody
                                  , JSON, NoContent (..)
                                  , FromHttpApiData (..),ToHttpApiData(..)
                                  , (:>) )
import Servant.API.Generic        ( (:-) )

import Servant.Client             ( AsClientT, ClientM, client
                                  , (//), (/:) )
import Servant.Client.Generic     ()

import Servant.Server             ( Application )
import Servant.Server.Generic     ( AsServer )

```

## Domain context

Now that we’ve handled the boilerplate, we can dive into our Movie Catalog domain.

Consider a `Movie` constructed from a `Title` and a `Year` of publication.

```haskell
data Movie = Movie
    { movieId :: MovieId
    , title :: Title
    , year :: Year
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

type MovieId = String
type Title = String
type Year = Int

```


Let’s forget about the deriving stuff for now and think about the API that we want to make.

```
  "version" -> Get Version
  /
api          "list" -> Get [Movie] ?sortBy= Title | Year       (sort by the Title or the Year)
  \        /
  "movies"                Get Movie
           \                /
            Capture MovieId - Put Movie
                            \
                              Delete MovieId
```

In this example, we create a very simple endpoint for the Version,
and several complex endpoints that use nested records for the CRUD part of the movie.

So, the URLs would look like

- GET …/version
- GET …/movies/list?sortby=Title
- GET …/movies/<MovieId>/
- PUT …/movies/<MovieId>/
- DELETE …/movies/<MovieId>

### API Type

Now that we have a very clear idea of the API we want to make, we need to transform it into usable Haskell code:

```haskell

data API mode = API
    { version :: mode :- "version" :> Get '[JSON] Version
    , movies :: mode :- "movies" :> NamedRoutes MoviesAPI
    } deriving stock Generic

type Version = String -- This will do for the sake of example.

```
Here, we see the first node of our tree. It contains the two branches “version” and “movies” respectively:

The “version” branch is very simple and self-explanatory.
The “movies” branch will contain another node, represented by another record (see above). That is why we need the `NameRoutes` helper.

Note:

The `mode` type parameter indicates into which implementation the record’s `Generic` representation will be transformed—as a client or as a server. We will discuss that later.

Let's jump into the "movies" subtree node:


```haskell

data MoviesAPI mode = MoviesAPI
    { list :: mode :- "list" :> QueryParam "SortBy" SortBy :> Get '[JSON] [Movie]
    , movie :: mode :- Capture "movieId" MovieId :> NamedRoutes MovieAPI
    } deriving stock Generic

data SortBy = Year | Title

instance ToHttpApiData SortBy where
  toQueryParam Year = "year"
  toQueryParam Title = "title"

instance FromHttpApiData SortBy where
  parseQueryParam "year" = Right Year
  parseQueryParam "title" = Right Title
  parseQueryParam param = Left $ param <> " is not a valid value"

```
So, remember, this type represents the `MoviesAPI` node that we’ve connected earlier to the main `API` tree.

In this subtree, we illustrated both an endpoint with a **query param** and also, a **capture** with a subtree underneath it.

So, let's go deeper into our API tree.

```haskell
data MovieAPI mode = MovieAPI
  { get :: mode :- Get '[JSON] (Maybe Movie)
  , update :: mode :- ReqBody '[JSON] Movie :> Put '[JSON] NoContent
  , delete :: mode :- Delete '[JSON] NoContent
  } deriving stock Generic
```

As you can see, we end up implementing the deepest routes of our API.

Small detail: as our main API tree is also a record, we need the `NamedRoutes` helper.
To improve readability, we suggest you create a type alias:

```haskell
type MovieCatalogAPI = NamedRoutes API
```

That's it, we have our `MovieCatalogAPI` type!

Let's make a server and a client out of it!

## The Server

As you know, we can’t talk about a server, without addressing the handlers.

First, we take our handlers…

```haskell
versionHandler :: Handler Version
versionHandler = pure "0.0.1"

movieListHandler :: Maybe SortBy -> Handler [Movie]
movieListHandler _ = pure moviesDB

moviesDB :: [Movie]
moviesDB =
  [ Movie "1" "Se7en" 1995
  , Movie "2" "Minority Report" 2002
  , Movie "3" "The Godfather" 1972
  ]

getMovieHandler :: MovieId -> Handler (Maybe Movie)
getMovieHandler requestMovieId = go moviesDB
  where
    go [] = pure Nothing
    go (m : _ms) | movieId m == requestMovieId = pure $ Just m
    go (_m : ms) = go ms

updateMovieHandler :: MovieId -> Movie -> Handler NoContent
updateMovieHandler requestedMovieId newMovie =
   -- update the movie list in the database...
   pure NoContent

deleteMovieHandler :: MovieId -> Handler NoContent
deleteMovieHandler _ =
   -- delete the movie from the database...
   pure NoContent
```

And assemble them together with the record structure, which is the glue here.

```haskell
server ::  API AsServer
server =
  API
    { version = versionHandler
    , movies = moviesHandler
    }

moviesHandler :: MoviesAPI AsServer
moviesHandler =
  MoviesAPI
    { list = movieListHandler
    , movie = movieHandler
    }

movieHandler :: MovieId -> MovieAPI AsServer
movieHandler mId = MovieAPI
    { get = getMovieHandler mId
    , update = updateMovieHandler mId
    , delete = deleteMovieHandler mId
    }
```
As you might have noticed, we build our handlers out of the same record types we used to define our API: `MoviesAPI` and `MovieAPI`. What kind of magic is this ?

Finally, we can run the server and connect the API routes to the handlers as usual:

```haskell
api :: Proxy MovieCatalogAPI
api = Proxy

main :: IO ()
main = run 8081 app

app :: Application
app = serve api server
```
Yay! That’s done and we’ve got our server!

## The Client

The client, so to speak, is very easy to implement:

```haskell
movieCatalogClient :: API (AsClientT ClientM)
movieCatalogClient = client api -- remember: api :: Proxy MovieCatalogAPI
```

We’ve also introduced some operators that help navigate through the nested records.

`(//)` is used to jump from one record to another.
`(/:)` is used to provide a parameter, whether it be a query param or a capture.

Let’s use those nice helpers for our movie catalog:

```haskell
listMovies :: Maybe SortBy -> ClientM [Movie]
listMovies sortBy = movieCatalogClient // movies // list /: sortBy

getMovie :: MovieId -> ClientM (Maybe Movie)
getMovie mId = movieCatalogClient // movies // movie /: mId // get

updateMovie :: MovieId -> Movie -> ClientM NoContent
updateMovie mId newMovie = movieCatalogClient // movies // movie /: mId // update /: newMovie

deleteMovie :: MovieId -> ClientM NoContent
deleteMovie mId = movieCatalogClient // movies // movie /: mId // delete
```

Done! We’ve got our client!

## Conclusion

We hope that you found this cookbook helpful, and that you now feel more confident 
using the record-based APIs, nested or not.

If you are interested in further understanding the built-in Servant combinators, see
[Structuring APIs](../structuring-apis/StructuringApis.html).

Since `NamedRoutes` is based on the Generic mechanism, you might want to have a look at 
[Sandy Maguire’s _Thinking with Types_ book](https://thinkingwithtypes.com/).