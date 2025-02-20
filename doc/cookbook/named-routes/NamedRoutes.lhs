# Record-based APIs

*Available in Servant 0.19 or higher*

Servant offers a very natural way of constructing APIs with records and nested records.

This cookbook explains how to implement APIs using records.  

First, we start by constructing the domain types of our Movie Catalog.
After, we show you how to implement the API type with the NamedRoutes records.
Lastly, we make a Server and a Client out of the API type.

However, it should be understood that this cookbook does _not_ dwell on the
built-in servant combinators as the [Structuring APIs](<../structuring-apis/StructuringApis.html>) 
cookbook already covers that angle.

## Motivation: Why would I want to use records over the `:<|>` operator?

With a record-based API, we don’t need to care about the declaration order of the endpoints.
For example, with the `:<|>` operator there’s room for error when the order of the API type

```haskell,ignore
type API1  =   "version" :> Get '[JSON] Version
                :<|>  "movies" :> Get '[JSON] [Movie]
```
does not follow the `Handler` implementation order
```haskell,ignore
apiHandler :: ServerT API1 Handler
apiHandler =   getMovies
                 :<|> getVersion
```
GHC can and will scold you with a very tedious message such as :
```console
    • Couldn't match type 'Handler NoContent'
                     with 'Movie -> Handler NoContent'
      Expected type: ServerT MovieCatalogAPI Handler
        Actual type: Handler Version
                     :<|> ((Maybe SortBy -> Handler [Movie])
                           :<|> ((MovieId -> Handler (Maybe Movie))
                                 :<|> ((MovieId -> Movie -> Handler NoContent)
                                       :<|> (MovieId -> Handler NoContent))))
    • In the expression:
        versionHandler
          :<|>
            movieListHandler
              :<|>
                getMovieHandler :<|> updateMovieHandler :<|> deleteMovieHandler
      In an equation for 'server':
          server
            = versionHandler
                :<|>
                  movieListHandler
                    :<|>
                      getMovieHandler :<|> updateMovieHandler :<|> deleteMovieHandler
    |
226 | server = versionHandler
    |
```
On the contrary, with the record-based technique, we refer to the routes by their name:
```haskell,ignore
data API mode = API
    { list   :: "list" :> ...
    , delete ::  "delete" :> ...
    }
```
and GHC follows the lead:
```console
    • Couldn't match type 'NoContent' with 'Movie'
      Expected type: AsServerT Handler :- Delete '[JSON] Movie
        Actual type: Handler NoContent
    • In the 'delete' field of a record
      In the expression:
        MovieAPI
          {get = getMovieHandler movieId,
           update = updateMovieHandler movieId,
           delete = deleteMovieHandler movieId}
      In an equation for 'movieHandler':
          movieHandler movieId
            = MovieAPI
                {get = getMovieHandler movieId,
                 update = updateMovieHandler movieId,
                 delete = deleteMovieHandler movieId}
    |
252 |     , delete = deleteMovieHandler movieId
    |
```

So, records are more readable for a human, and GHC gives you more accurate error messages, so 
why ever look back? Let's get started! 


<details> 
  <summary>The boilerplate required for both the nested and flat case</summary>

```haskell
{-# LANGUAGE GHC2021                  #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE OverloadedRecordDot      #-}

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Network.Wai.Handler.Warp   (run)
import Data.Aeson                 (FromJSON (..), ToJSON (..))
import GHC.Generics               (Generic)
import Data.Text                  (Text)
import Data.Foldable              (find)

import Servant 
import Servant.Client 
import Servant.Client.Generic 
import Servant.Server.Generic
```

</details>

## Domain context

Consider a `Movie` constructed from a `Title` and a `Year` of publication.

```haskell
data Movie = Movie
    { movieId :: MovieId
    , title :: Title
    , year :: Year
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

type MovieId = Text
type Title = Text
type Year = Int
```

To proceed, let us think about the API we want to build: 

```
"version" ───► Get Version                                         
                                                                  
"movies" ──┬─► "list" ────────────► Get [Movie] ?sortBy=(Title|Year)
           │                                                      
           └─► Capture MovieId ─┬─► Get Movie                       
                                │                                  
                                ├─► Put Movie                       
                                │                                  
                                └─► Delete Movie
```

In this example, we create a very simple endpoint for the Version,
and several complex endpoints that use nested records for the CRUD part of the movie.

So, flattening this out, the URLs a client may call are: 

- `GET /version`
- `GET /movies/list?sortby=<title>`
- `GET /movies/:MovieId`
- `PUT /movies/:MovieId`
- `DELETE /movies/:MovieId`

### API Type

Now that we have a clear idea of the API we want to make, we need to transform it into usable Haskell code, for that, let us first 
create a `Generic` record type that will hold our Api:

```haskell
data API mode = API
    { version :: mode :- "version" :> Get '[JSON] Version
    , movies :: mode :- "movies" :> NamedRoutes MoviesAPI
    } deriving stock Generic

type Version = Text -- this will do for the sake of example.
```

Here, we see the first node of our tree. It contains the two branches “version” and “movies” respectively:

The “version” branch reads as follows: "instantiated ad a `mode`, the record field with the name `version` holds a route that needs
to match the prefix `"version"` and returns a `Version`, serialized as a `JSON` upon issing a `Get` request.

The “movies” branch will contain another node, represented by another record (see above). That is why we need the `NameRoutes` helper, 
`:>` would normally expect another "standard" `API` tree (the ones with the `:<|>` operator) and `NamedRoutes` gets us one of these 
when passing a record.

Note:

The `mode` type parameter indicates into which implementation the record’s `Generic` representation will be transformed—as a client or as a server. We will discuss that later.

Let's jump into the "movies" subtree node:


```haskell
data MoviesAPI mode = MoviesAPI
    { list :: mode :- "list" :> QueryParam "SortBy" SortBy :> Get '[JSON] [Movie]
    , movie :: mode :- Capture "movieId" MovieId :> NamedRoutes MovieAPI
    } deriving stock Generic

data SortBy = Year | Title
  deriving stock (Eq, Ord, Show)

-- ... and the boilerplate to allow for usage as a query param: 
instance ToHttpApiData SortBy where
  toQueryParam Year = "year"
  toQueryParam Title = "title"

instance FromHttpApiData SortBy where
  parseQueryParam "year" = Right Year
  parseQueryParam "title" = Right Title
  parseQueryParam param = Left $ param <> " is not a valid SortBy"
```

So, remember, this type represents the `MoviesAPI` node that we’ve connected earlier to the main `API` tree.

In this subtree, we illustrated both an endpoint with a **query param** and also, a **capture** with a subtree underneath it.

The first branch is done, now, let's also implement the second one as follows: 

```haskell
data MovieAPI mode = MovieAPI
  { get :: mode :- Get '[JSON] (Maybe Movie)
  , update :: mode :- ReqBody '[JSON] Movie :> Put '[JSON] NoContent
  , delete :: mode :- Delete '[JSON] NoContent
  } deriving stock Generic
```

Small detail: as our main API tree is also a record, we need the `NamedRoutes` helper (to obtain the API proper)
To improve readability, we suggest you create a type alias:

```haskell
type MovieCatalogAPI = NamedRoutes API
```

That's it, we have our `MovieCatalogAPI` type!

Let's make a server and a client out of it!

## The Server

As you know, we can’t talk about a server, without addressing the handlers.

First, we build our handlers (mind that we're cheating a bit, obviously, the `moviesDB` in reality 
would have to be part of some state, such that we can modify it, additionally, making it a list is not 
very wise in terms of performance)

```haskell
versionHandler :: Handler Version
versionHandler = pure "0.0.1"

movieListHandler :: Maybe SortBy -> Handler [Movie]
movieListHandler _ = 
  -- depending on sortBy, do a different sorting
  pure moviesDB

moviesDB :: [Movie]
moviesDB =
  [ Movie "1" "Se7en" 1995
  , Movie "2" "Minority Report" 2002
  , Movie "3" "The Godfather" 1972
  ]

getMovieHandler :: MovieId -> Handler (Maybe Movie)
getMovieHandler requestMovieId = pure $ find (\movie -> movie.movieId == requestMovieId) moviesDB

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

Finally, we can run the server and connect the API routes to the handlers, using the convenient `genericServe` function

```haskell
main :: IO ()
main = run 8081 app

app :: Application
app = genericServe server
```

Yay! That's done and we've got our server!

## Clients and Links

The clear advantage of record-based generics approach, is that
we can get safe links and clients very conveniently. We don't need to define endpoint types,
as field accessors work as proxies - let's demonstrate that with a client: 

```haskell
movieCatalogClient :: API (AsClientT ClientM)
movieCatalogClient = genericClient -- this also works with other Monads than ClientM by using `genericClientHoist`
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

Done! We’ve got our client, now let's turn to the links: 

```haskell
-- a single link: 
versionLink :: Link 
versionLink = fieldLink version

-- links for the entire route: 
routesLinks :: API (AsLink Link)
routesLinks = allFieldLinks
```

## Using record-based APIs together with a custom monad

If your app uses a custom monad, here's how you can combine it with
generics.

```haskell
-- for some custom Environment
data HandlerEnv = MkHandlerEnv

type AppM = ReaderT HandlerEnv Handler

-- we need to provide a natural transformation
appToHandler :: HandlerEnv -> (forall a. AppM a -> Handler a)
appToHandler env act = runReaderT act env

-- which we can then use in `genericServeT`
appMyMonad :: HandlerEnv -> Application
appMyMonad env = genericServeT (appToHandler env) appMapi
  where 
    appMapi :: ServerT (NamedRoutes API) m
    appMapi = undefined
```

There is also a combinator for serving with a `Context`, `genericServeWithContextT`.

## Conclusion

We hope that you found this cookbook helpful, and that you now feel more confident 
using the record-based APIs, nested or not.

If you are interested in further understanding the built-in Servant combinators, see
[Structuring APIs](../structuring-apis/StructuringApis.html).

Since `NamedRoutes` is based on the Generic mechanism, you might want to have a look at 
[Sandy Maguire's _Thinking with Types_ book](https://thinkingwithtypes.com/).
