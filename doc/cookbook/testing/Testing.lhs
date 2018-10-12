# How To Test Servant Applications

Even with a nicely structured API that passes Haskell's strict type checker,
it's a good idea to write some tests for your application. 

In this recipe we'll work through some common testing strategies and provide
examples of utlizing these testing strategies in order to test Servant
applications.

This recipe uses the following ingredients:

```haskell
{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}
import qualified Control.Concurrent               as C
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant
import           Servant.Client
import           Servant.Server

import           Test.Hspec
import           Test.Hspec.Wai                                                    
import           Test.Hspec.Wai.Matcher
```

We're going to produce different `Spec`s that represent different
aspects of our application, and we'll ask `hspec` to run all of our different
`Spec`s. This is a common organizational method for testing modules:

```haskell
spec :: Spec
spec =
  businessLogicSpec
  thirdPartyResourcesSpec
  servantQuickcheckSpec
```

Often, codebases will use `hspec`'s 
[autodiscover pragma](http://hspec.github.io/hspec-discover.html)
to find all testing modules and `Spec`s inside, but we're going to
explicitly make a `main` function to run our tests because we have only one
`spec` defined above:

```haskell
main :: IO ()
main = hspec spec
```

## Testing Your Business Logic

Let's say we have an API that looks something like this:

```haskell
data User = User { 
  name :: Text 
  , user_id :: Integer 
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

type UserApi = 
  -- One endpoint: create a user
  "user" :> Capture "userId" Integer :> Post '[JSON] User
```

A real server would likely use a database to store, retrieve, and validate
users, but we're going to do something really simple merely to have something
to test. With that said, here's a sample handler for the endpoint described
above:

```haskell
userServer :: Server UserApi Handler
userServer = createUser
  where creatUser userId
  | userId > 5000 = pure $ User { name = "some user", user_id = userId }
  | otherwise = throwError $ err400 { errBody = "userId is too small" }
```

### Strategy 1: Spin Up a Server, Create a Client, Make Some Requests

One of the benefits of Servant's type-level DSL for describing APIs is that
once you have provided a type-level description of your API, you can create
clients, documentation, or other tools for it somewhat magically.

In this case, we'd like to *test* our server, so we can use `servant-client`
to create a client, after which we'll run our server, and then make requests
of it and see how it responds.

Let's write some tests:

```haskell
businessLogicSpec :: Spec
businessLogicSpec = do
  -- create a test client function
  createUser = client (Proxy :: Proxy UserApi)
  -- create a servant-client ClientEnv
  baseUrl <- parseBaseUrl "http://localhost:8888"
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager baseUrl

  -- Run the server in another thread.
  liftIO $ C.forkIO $ Warp.run 8888 (server userServer)

  -- testing scenarios start here
  describe "POST /user" $ do
    it "should create a user with a high enough ID" $
      result <- runClientM clientEnv (createUser 50001)
      result `shouldEqual` Right $ User { name = "some_user", user_id = "5001 "}
    it "should fail with a too-small ID" $
      result <- runClientM clientEnv (createUser 4999)
      result `shouldEqual` Right $ User { name = "some_user", user_id = "5001 "}
```

### Running These Tests

Let's run our tests and see what happens:

```
$ cabal new-test all
```

Great: we passed! Servers obviously get more complex, though, and we may not
wish to create a whole suite of clients for our server every time. In our next
scenario we're going to mock out a 3rd-party resource that our server talks to
and we're going to be using `hspec-wai` to run our `Application` instance and
to make requests.

## *Mocking* 3rd Party Resources

Often our web applications will need to make their own web
requests to other 3rd-party applications. These requests provide a lot
of opportunity for failure and so we'd like to test that the right
messages and failure values (in addition to success values) are returned
from our application.

### Define the 3rd-Party Resource

With Servant's type-level API definitions, assuming you've already defined the
API you want to mock, it's relatively trivial to create a simple server for
the purposes of running tests. For instance, consider an API server that needs
to get data out of Elasticsearch. Let's first define the Elasticsearch server
and client using Servant API descriptions:

```haskell
type SearchAPI = 
  -- We're using Aeson's Generic JSON `Value` to make things easier on
  -- ourselves. We're also representing only one Elasticsearch endpoint:
  -- get item by id
  "myIndex" :> "myDocType" :> Capture "docId" Integer :> Get '[JSON] Value

-- Here's our Servant Client function
getDocument = client (Proxy :: Proxy SearchAPI)

-- We can use these helpers when we want to make requests 
-- using our client function
clientEnv :: Text -> Text -> IO ClientEnv
clientEnv esHost esPort = do
  baseUrl <- parseBaseUrl $ T.unpack $ esHost <> ":" <> esPort
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager baseUrl

runSearchClient :: Text -> Text -> ClientM a -> IO (Either ServantError a)
runSearchClient esHost esPort = (clientEnv esHost esPort >>=) . runClientM
```

### Servant Server Example Using this 3rd-Party Resource

So we've got an Elasticsearch server and a client to talk to it with one
function that retrieves a document by its id. Let's now build a simple app 
server that uses this client to retrieve documents. This is somewhat
contrived, but hopefully it illustrates the typical three-tier application
architecture.

One note: we're also going to take advantage of `aeson-lens` here, which may
look a bit foreign. The gist of it is that we're going to traverse a JSON 
`Value` from Elasticsearch and try to extract some  kind of document to
return.

```haskell
type DocApi = 
  "docs" :> Capture "docId" Integer :> Get '[JSON] Value

docServer :: Text -> Text -> Server DocApi Handler
docServer esHost esPort = getDocById esHost esPort

-- Our Handler tries to get a doc from Elasticsearch and then tries to parse
-- it. Unfortunately, there's a lot of opportunity for failure in these
-- actions
getDocById :: Text -> Text -> Integer -> Handler Value
getDocById esHost esPort docId = do
  -- Our Servant Client function returns Either ServantError Value here:
  docRes <- liftIO $ runSearchClient esHost esPort (getDocument docId)
  case docRes of
    Left err -> throwError $ err404 { errBody = "Failed looking up content" }
    Right value -> do
      -- we'll either fail to parse our document or we'll return it
      case value ^? _Object . ix "_source" of
        Nothing -> throwError $ err400 { errBody = "Failed parsing content" }
        Just obj -> pure obj
```

### Testing Our Backend

So the above represents our application. How shall we test this application?
Ideally, we'd like it to make requests of a real Elasticsearch server, but we
certainly don't want our tests to trigger requests to a live, production
database. In addition, we don't want to depend on our real Elasticsearch
server having specific, consistent results for us to test against, because
that would make our tests flaky (and flaky tests are sometimes described as
worse than not having tests at all).

One solution to this is to create a trivial Elasticsearch server as part of
our testing code. We can do this relatively easily because we already have
an API definition for it above. With a *real* server, we can then let our own
application make requests of it and we'll simulate different scenarios in
order to make sure our application responds the way we expect it to.

Let's start with some helpers which will allow us to run a testing version
of our Elasticsearch server in another thread:

```haskell
-- | We'll run the Elasticsearch server so we can test behaviors
withElasticsearch :: IO () -> IO ()
withElasticsearch action =
  bracket (liftIO $ C.forkIO $ Warp.run 9999 esTestApp)
    C.killThread
    (const action)

esTestApp :: Application
esTestApp = serve (Proxy :: Proxy SearchAPI) esTestServer

esTestServer :: Server SearchAPI Handler
esTestServer = getESDocument

getESDocument :: Integer -> Handler Value
getESDocument docId
  -- arbitrary things we can trigger in our tests to check for failure
  -- We want to try to trigger different code paths
  | docId > 1000 = pure . Left $ ConnectionError "Bad connection!"
  | docId > 500 = pure . Object $ HM.fromList [("bad", String "data")]
  | otherwise = pure $ Object $ HM.fromList [("_source", defaultDocument)] 
```

Now, we should be ready to write some tests. As mentioned above we're going
to use `hspec-wai` in this example to make test requests and to run our own
application. This should simplify our testing code somewhat:

```haskell
thirdPartyResourcesSpec :: Spec
thirdPartyResourcesSpec = around_ withElasticsearch $ do
  -- we call `with` and pass  our servant-server `Application`
  with (pure $ serve $ docServer "localhost" "9999") $ do
    describe "GET /docs" $ do
      it "should be able to get a document" $
        -- `get` is a function from hspec-wai`. 
        get "/docs/1" `shouldRespondWith` 200
      it "should be able to handle connection failures" $
        -- We can also make custom HTTP requests with the  `request` function
        get "/docs/1001" `shouldRespondWith` 404
      it "should be able to handle parsing failures" $
        get "/docs/501" `shouldRespondWith` 400
      it "should be able to handle odd HTTP requests" $
        -- we can also make all kinds of arbitrary custom requests to see how
        -- our server responds using the `request` function:
        -- request :: Method -> ByteString -> [Header] 
        --   -> LB.ByteString -> WaiSession SResponse
        request methodPost "/docs/501" [] "{" `shouldRespondWith` 415
```


## Servant Quickcheck

[`servant-quickcheck`](https://github.com/haskell-servant/servant-quickcheck)
is a project that allows users to write tests for whole Servant APIs using
quickcheck-style property-checking mechanisms.

`servant-quickcheck` is great for asserting whole-API rules, such as "no
endpoint throws a 500" or "all 301 status codes also come with a Location
header". The project even comes with a number of predicates that reference
the [RFCs they originate from](https://github.com/haskell-servant/servant-quickcheck/blob/master/src/Servant/QuickCheck/Internal/Predicates.hs).


### Quickcheckable API

Let's make an API and a server to demonstrate how to use `servant-quickcheck`:

```haskell
type API = ReqBody '[JSON] String :> Post '[JSON] String
      :<|> Get '[JSON] Int
      :<|> BasicAuth "some-realm" () :> Get '[JSON] ()

api :: Proxy API
api = Proxy

server :: IO (Server API)
server = do
    mvar <- newMVar ""
    return $ (\x -> liftIO $ swapMVar mvar x)
        :<|> (liftIO $ readMVar mvar >>= return . length)
        :<|> (const $ return ())
```

### Using `servant-quickcheck`

`servant-quickcheck` also has a cool mechanism where you can compare two API
servers to demonstrate that they respond identically to requests. This may be
useful if you are planning to rewrite one API in another language or with
another web framework. You have to specify whether you're looking for
`jsonEquality` vs regular `ByteString` equality, though:

```haskell
servantQuickcheckSpec :: Spec
servantQuickcheckSpec = describe "" $ do
  it "API demonstrates best practices" $
    withServantServer api server $ \burl ->
      serverSatisfies api burl args (unauthorizedContainsWWWAuthenticate
                                 <%> not500
                                 <%> onlyJsonObjects
                                 <%> mempty)

  it "API doesn't have these things implemented yet" $
    withServantServer api server $ \burl -> do
      serverDoesntSatisfy api burl args (getsHaveCacheControlHeader
                                    <%> notAllowedContainsAllowHeader
                                    <%> mempty)
```