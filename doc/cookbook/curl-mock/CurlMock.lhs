# Generating mock curl calls

In this example we will generate curl requests with mock post data from a servant API.
This may be useful for testing and development purposes.
Especially post requests with a request body are tedious to send manually.

Also, we will learn how to use the servant-foreign library to generate stuff from servant APIs.


Language extensions and imports:
``` haskell
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

import           Control.Lens                      ((^.))
import           Data.Aeson
import           Data.Aeson.Text
import           Data.Proxy                        (Proxy (Proxy))
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8)
import qualified Data.Text.IO                      as T.IO
import qualified Data.Text.Lazy                    as LazyT
import           GHC.Generics
import           Servant                           ((:<|>), (:>), Get, JSON,
                                                    Post, ReqBody)
import           Servant.Foreign                   (Foreign, GenerateList,
                                                    HasForeign, HasForeignType, Req,
                                                    Segment, SegmentType (Cap, Static),
                                                    argName, listFromAPI, path,
                                                    reqBody, reqMethod, reqUrl, typeFor,
                                                    unPathSegment, unSegment,)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Gen               (generate)
import qualified Data.Text                         as T

```


Let's define our API:

``` haskell
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "new" :> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()

data User = User
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show, Generic)

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink
instance ToJSON User
instance FromJSON User
```

Notice the `Arbitrary User` instance which we will later need to create mock data.

Also, the obligatory servant boilerplate:

``` haskell
api :: Proxy UserAPI
api = Proxy
```


## servant-foreign and the HasForeignType Class

Servant-foreign allows us to look into the API we designed.
The entry point is `listFromAPI` which takes three types and returns a list of endpoints:

``` haskell ignore
listFromAPI :: (HasForeign lang ftype api, GenerateList ftype (Foreign ftype api)) => Proxy lang -> Proxy ftype -> Proxy api -> [Req ftype]
```

This looks a bit confusing...
[Here](https://hackage.haskell.org/package/servant-foreign/docs/Servant-Foreign.html#t:HasForeign) is the documentation for the `HasForeign` typeclass.
We will not go into details here, but this allows us to create a value of type `ftype` for any type `a` in our API.

In our case we want to create a mock of every type `a`.

We create a new datatype that holds our mocked value. Well, not the mocked value itself. To mock it we need IO (random). So the promise of a mocked value after some IO is performed:

``` haskell
data NoLang

data Mocked = Mocked (IO Text)
```

Now, we create an instance of `HasForeignType` for `NoLang` and `Mocked` for every `a` that implements `ToJSON` and `Arbitrary`:
``` haskell
instance (ToJSON a, Arbitrary a) => HasForeignType NoLang Mocked a where
  typeFor _ _ _ =
    Mocked (genText (Proxy :: Proxy a))
```

What does `genText` do? It generates an arbitrary value of type `a` and encodes it as text. (And does some lazy to non-lazy text transformation we do not care about):

``` haskell
genText :: (ToJSON a, Arbitrary a) => Proxy a -> IO Text
genText p =
  fmap (\v -> LazyT.toStrict $ encodeToLazyText v) (genArb p)

genArb :: Arbitrary a => Proxy a -> IO a
genArb _ =
  generate arbitrary
```

### Generating curl calls for every endpoint

Everything is prepared now and we can start generating some curl calls.

``` haskell
generateCurl :: (GenerateList Mocked (Foreign Mocked api), HasForeign NoLang Mocked api)
  => Proxy api
  -> Text
  -> IO Text
generateCurl p host =
  fmap T.unlines body
  where
  body = mapM (generateEndpoint host)
    $ listFromAPI (Proxy :: Proxy NoLang) (Proxy :: Proxy Mocked) p
```

First, `listFromAPI` gives us a list of `Req Mocked`. Each `Req` describes one endpoint from the API type.
We generate a curl call for each of them using the following helper.

``` haskell
generateEndpoint :: Text -> Req Mocked -> IO Text
generateEndpoint host req =
  case maybeBody of
    Just body ->
      body >>= \b -> return $ T.intercalate " " [ "curl", "-X", method, "-d", "'" <> b <> "'"
                                                , "-H 'Content-Type: application/json'", host <> "/" <> url ]
    Nothing ->
      return $ T.intercalate " " [ "curl", "-X", method, host <> "/" <> url ]
  where
    method = decodeUtf8 $ req ^. reqMethod

    url = T.intercalate "/" $ map segment (req ^. reqUrl . path)

    maybeBody = fmap (\(Mocked io) -> io) (req ^. reqBody)

```
`servant-foreign` offers a multitude of lenses to be used with `Req`-values.

`reqMethod` gives us a straigthforward `Network.HTTP.Types.Method`, `reqUrl` the url part and so on.
Just take a look at [the docs](https://hackage.haskell.org/package/servant-foreign/docs/Servant-Foreign.html).

But how do we get our mocked json string? This seems to be a bit to short to be true:

``` haskell ignore
maybeBody = fmap (\(Mocked io) -> io) (req ^. reqBody)
```

But it is that simple!
The docs say `reqBody` gives us a `Maybe f`. What is `f`, you ask? As defined in `generateCurl`, `f` is `Mocked` and contains a `IO Text`. How is this `Mocked` value created? The `HasForeignType::typeFor` does it!

Of course only if the endpoint has a request body.


Some (incomplete) code for url segments:
``` haskell
segment :: Segment Mocked -> Text
segment seg =
  case unSegment seg of
    Static p ->
      unPathSegment p

    Cap arg ->
      -- Left as exercise for the reader: Mock args in the url
      unPathSegment $ arg ^. argName
```

And now, lets hook it all up in our main function:

``` haskell
main :: IO ()
main =
  generateCurl api "localhost:8081" >>= T.IO.putStrLn
```

Done:

``` curl
curl -X GET localhost:8081/users
curl -X POST -d '{"email":"wV򝣀_b򆎘:z񁊞򲙲!(3DM V","age":10,"name":"=|W"}' -H 'Content-Type: application/json' localhost:8081/new/user

```

This is of course no complete curl call mock generator, many things including path arguments are missing.
But it correctly generates mock calls for simple POST requests.

Also, we now know how to use `HasForeignType` and `listFromAPI` to generate anything we want.
