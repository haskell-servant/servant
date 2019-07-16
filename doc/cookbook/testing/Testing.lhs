# How To Test Servant Applications

Even with a nicely structured API that passes Haskell's strict type checker,
it's a good idea to write some tests for your application.

In this recipe we'll work through some common testing strategies and provide
examples of utlizing these testing strategies in order to test Servant
applications.

## Testing strategies

There are many testing strategies you may wish to employ when testing your
Servant application, but included below are three common testing patterns:

- We'll use `servant-client` to derive client functions and then send valid
requests to our API, running in another thread. This is great for testing
that our **business logic** is correctly implemented with only valid HTTP
requests.

- We'll also use `hspec-wai` to make **arbitrary HTTP requests**, in order to
test how our application may respond to invalid or otherwise unexpected
requests.

- Finally, we can also use `servant-quickcheck` for **whole-API tests**, in order
to assert that our entire application conforms to **best practices**.

## Useful Libraries

The following libraries will often come in handy when we decide to test our
Servant applications:

- [hspec](https://hspec.github.io/)
- [hspec-wai](http://hackage.haskell.org/package/hspec-wai)
- [QuickCheck](http://hackage.haskell.org/package/QuickCheck)
- [servant-quickcheck](https://hackage.haskell.org/package/servant-quickcheck)

## Imports and Our Testing Module

This recipe starts with the following ingredients:

```haskell
{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}
import           Prelude ()
import           Prelude.Compat

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
import           Control.Lens              hiding (Context)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal (serverDoesntSatisfy)

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
```

We're going to produce different `Spec`s that represent different
aspects of our application, and we'll ask `hspec` to run all of our different
`Spec`s. This is a common organizational method for testing modules:

```haskell
spec :: Spec
spec = do
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
to test. With that said, here's a sample handler, server, and `Application`
for the endpoint described above:

```haskell
userApp :: Application
userApp = serve (Proxy :: Proxy UserApi) userServer

userServer :: Server UserApi
userServer = createUser

createUser :: Integer -> Handler User
createUser userId = do
  if userId > 5000
    then pure $ User { name = "some user", user_id = userId }
    else throwError $ err400 { errBody = "userId is too small" }
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
withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure userApp) action


businessLogicSpec :: Spec
businessLogicSpec =
  -- `around` will start our Server before the tests and turn it off after
  around withUserApp $ do
    -- create a test client function
    let createUser = client (Proxy :: Proxy UserApi)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "POST /user" $ do
      it "should create a user with a high enough ID" $ \port -> do
        result <- runClientM (createUser 50001) (clientEnv port)
        result `shouldBe` (Right $ User { name = "some user", user_id = 50001})
      it "will it fail with a too-small ID?" $ \port -> do
        result <- runClientM (createUser 4999) (clientEnv port)
        result `shouldBe` (Right $ User { name = "some user", user_id = 50001})
```

### Running These Tests

Let's run our tests and see what happens:

```
$ cabal new-test all
POST /user
  should create a user with a high enough ID
  should fail with a too-small ID FAILED [1]

  Failures:

  Testing.lhs:129:7:
  1) POST /user should fail with a too-small ID
       expected: Right (User {name = "some user", user_id = 50001})
        but got: Left (FailureResponse (Response {responseStatusCode = Status {statusCode = 400, statusMessage = "Bad Request"}, responseHeaders = fromList [("Transfer-Encoding","chunked"),("Date","Fri, 12 Oct 2018 04:36:22 GMT"),("Server","Warp/3.2.25")], responseHttpVersion = HTTP/1.1, responseBody = "userId is too small"}))

  To rerun use: --match "/POST /user/should fail with a too-small ID/"
```

Hmm. One passed and one failed! It looks like I *was* expecting a success
response in the second test, but I actually got a failure. We should fix that,
but first I'd like to introduce `hspec-wai`, which will give us different
mechanisms for making requests of our application and validating the responses
we get. We're also going to spin up a fake Elasticsearch server, so that our
server can think it's talking to a real database.


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
  baseUrl <- parseBaseUrl $ unpack $ esHost <> ":" <> esPort
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager baseUrl

runSearchClient :: Text -> Text -> ClientM a -> IO (Either ClientError a)
runSearchClient esHost esPort = (clientEnv esHost esPort >>=) . runClientM
```

### Servant Server Example Using this 3rd-Party Resource

So we've got an Elasticsearch server and a client to talk to it. Let's now
build a simple app server that uses this client to retrieve documents. This
is somewhat contrived, but hopefully it illustrates the typical three-tier
application architecture.

One note: we're also going to take advantage of `lens-aeson` here, which may
look a bit foreign. The gist of it is that we're going to traverse a JSON
`Value` from Elasticsearch and try to extract some  kind of document to
return.

Imagine, then, that this is our real server implementation:

```haskell
type DocApi =
  "docs" :> Capture "docId" Integer :> Get '[JSON] Value

docsApp :: Text -> Text -> Application
docsApp esHost esPort = serve (Proxy :: Proxy DocApi) $ docServer esHost esPort

docServer :: Text -> Text -> Server DocApi
docServer esHost esPort = getDocById esHost esPort

-- Our Handler tries to get a doc from Elasticsearch and then tries to parse
-- it. Unfortunately, there's a lot of opportunity for failure in these
-- actions
getDocById :: Text -> Text -> Integer -> Handler Value
getDocById esHost esPort docId = do
  -- Our Servant Client function returns Either ClientError Value here:
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

So the above represents our application and is close to a server we may
actually deploy. How then shall we test this application?

Ideally, we'd like it to make requests of a *real* Elasticsearch server, but
we certainly don't want our tests to trigger requests to a live, production
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

esTestServer :: Server SearchAPI
esTestServer = getESDocument

-- This is the *mock* handler we're going to use. We create it
-- here specifically to trigger different behavior in our tests.
getESDocument :: Integer -> Handler Value
getESDocument docId
  -- arbitrary things we can use in our tests to simulate failure:
  -- we want to trigger different code paths.
  | docId > 1000 = throwError err500
  | docId > 500 = pure . Object $ HM.fromList [("bad", String "data")]
  | otherwise = pure $ Object $ HM.fromList [("_source", Object $ HM.fromList [("a", String "b")])]
```

Now, we should be ready to write some tests.

In this case, we're going to use `hspec-wai`, which will give us a simple way
to run our application, make requests, and make assertions against the
responses we receive.

Hopefully, this will simplify our testing code:

```haskell
thirdPartyResourcesSpec :: Spec
thirdPartyResourcesSpec = around_ withElasticsearch $ do
  -- we call `with` from `hspec-wai` and pass *real* `Application`
  with (pure $ docsApp "localhost" "9999") $ do
    describe "GET /docs" $ do
      it "should be able to get a document" $
        -- `get` is a function from hspec-wai`.
        get "/docs/1" `shouldRespondWith` 200
      it "should be able to handle connection failures" $
        get "/docs/1001" `shouldRespondWith` 404
      it "should be able to handle parsing failures" $
        get "/docs/501" `shouldRespondWith` 400
      it "should be able to handle odd HTTP requests" $
        -- we can also make all kinds of arbitrary custom requests to see how
        -- our server responds using the `request` function:
        -- request :: Method -> ByteString -> [Header]
        --   -> LB.ByteString -> WaiSession SResponse
        request methodPost "/docs/501" [] "{" `shouldRespondWith` 405
      it "we can also do more with the Response using hspec-wai's matchers" $
        -- see also `MatchHeader` and JSON-matching tools as well...
        get "/docs/1" `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }

bodyMatcher :: [Network.HTTP.Types.Header] -> Body -> Maybe String
bodyMatcher _ body = case (decode body :: Maybe Value) of
  -- success in this case means we return `Nothing`
  Just val | val == (Object $ HM.fromList [("a", String "b")]) -> Nothing
  _ -> Just "This is how we represent failure: this message will be printed"
```

Out of the box, `hspec-wai` provides a lot of useful tools for us to run tests
against our application. What happens when we run these tests?

```
$ cabal new-test all
...

GET /docs
  should be able to get a document
  should be able to handle connection failures
  should be able to handle parsing failures
  should be able to handle odd HTTP requests
  we can also do more with the Response using hspec-wai's matchers
```

Fortunately, they all passed! Let's move to another strategy: whole-API
testing.

## Servant Quickcheck

[`servant-quickcheck`](https://github.com/haskell-servant/servant-quickcheck)
is a project that allows users to write tests for whole Servant APIs using
quickcheck-style property-checking mechanisms.

`servant-quickcheck` is great for asserting API-wide rules, such as "no
endpoint throws a 500" or "all 301 status codes also come with a Location
header". The project even comes with a number of predicates that reference
the [RFCs they originate from](https://github.com/haskell-servant/servant-quickcheck/blob/master/src/Servant/QuickCheck/Internal/Predicates.hs).

In other words, it's one way to assert that your APIs conform to specs and
best practices.

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

Let's build some tests for our API using `servant-quickcheck`.

Similar to the above examples, we're going to create `Spec`s, but in this
case, we'll rely on a number of predicates available from `servant-quickcheck`
to see if our API server conforms to best practices:

```haskell
-- Let's set some QuickCheck values
args :: Args
args = defaultArgs { maxSuccess = 500 }

-- Here's a Servant Context object we'll use
ctx :: Context '[BasicAuthCheck ()]
ctx = BasicAuthCheck (const . return $ NoSuchUser) :. EmptyContext


servantQuickcheckSpec :: Spec
servantQuickcheckSpec = describe "" $ do
  it "API demonstrates best practices" $
    -- `withServerServer` and `withServantServerAndContext` come from `servant-quickcheck`
    withServantServerAndContext api ctx server $ \burl ->
      -- `serverSatisfies` and the predicates also come from `servant-quickcheck`
      serverSatisfies api burl args (unauthorizedContainsWWWAuthenticate
                                 <%> not500
                                 <%> onlyJsonObjects  -- this one isn't true!
                                 <%> mempty)

  it "API doesn't have these things implemented yet" $
    withServantServerAndContext api ctx server $ \burl -> do
      serverDoesntSatisfy api burl args (getsHaveCacheControlHeader
                                    <%> notAllowedContainsAllowHeader
                                    <%> mempty)
```

Let's see what happens when we run these tests:


```
  API demonstrates best practices FAILED [2]
+++ OK, passed 500 tests.
  API doesn't have these things implemented yet

  src/Servant/QuickCheck/Internal/QuickCheck.hs:143:11:
  2) Main[339:25] API demonstrates best practices
       Failed:
       Just Predicate failed
            Predicate: onlyJsonObjects

            Response:
                 Status code: 200
                 Headers:  "Transfer-Encoding": "chunked"
                           "Date": "Fri, 12 Oct 2018 04:36:22 GMT"
                           "Server": "Warp/3.2.25"
                           "Content-Type": "application/json;charset=utf-8"
                 Body: ""

  To rerun use: --match "/Main[339:25]/API demonstrates best practices/"

Randomized with seed 1046277487

Finished in 0.4306 seconds
```

Hmm. It looks like we *thought* our API only returned JSON objects, which is a
best practice, but in fact, we *did* have an endpoint that returned an empty
body, which you can see in the printed response above: `Body: ""`. We should
consider revising our API to only return top-level JSON Objects in the future!

### Other Cool Things

`servant-quickcheck` also has a cool mechanism where you can compare two API
servers to demonstrate that they respond identically to requests. This may be
useful if you are planning to rewrite one API in another language or with
another web framework. You have to specify whether you're looking for
`jsonEquality` vs regular `ByteString` equality, though.

## Conclusion

There are lots of techniques for testing and we only covered a few here.

Useful libraries such as `hspec-wai` have ways of running Wai `Application`s
and sending requests to them, while Servant's type-level DSL for defining APIs
allows us to more easily mock out servers and to derive clients, which will
only craft valid requests.

Lastly, if you want a broad overview of where your application fits in with
regard to best practices, consider using `servant-quickcheck`.

This program is available as a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/testing).
