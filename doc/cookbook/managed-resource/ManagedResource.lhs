# Request-lifetime Managed Resources

Let's see how we can write a handle that uses a resource managed by Servant. The resource is created automatically by Servant when the server recieves a request, and the resource is automatically destroyed when the server is finished handling a request.

As usual, we start with a little bit of throat clearing.


``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.IO
```

Here we define an API type that uses the `WithResource` combinator. The server handler for an endpoint with a `WithResource res` component will receive a value of that type as an argument.

``` haskell
type API = WithResource Handle :> ReqBody '[PlainText] String :> Post '[JSON] NoContent

api :: Proxy API
api = Proxy
```

But this resource value has to come from somewhere. Servant obtains the value using an Acquire provided in the context. The Acquire knows how to both create and destroy resources of a particular type.

``` haskell
appContext :: Context '[Acquire Handle]
appContext = acquireHandle :. EmptyContext

acquireHandle :: Acquire Handle
acquireHandle = mkAcquire newHandle closeHandle

newHandle :: IO Handle
newHandle = do
  putStrLn "opening file"
  h <- openFile "test.txt" AppendMode
  putStrLn "opened file"
  pure h

closeHandle :: Handle -> IO ()
closeHandle h = do
  putStrLn "closing file"
  hClose h
  putStrLn "closed file"
```

Now we create the handler which will use this resource. This handler will write the request message to the System.IO.Handle which was provided to us. In some situations the handler will succeed, but in some in will fail. In either case, Servant will clean up the resource for us.

``` haskell
server :: Server API
server = writeToFile

  where writeToFile :: (ReleaseKey, Handle) -> String -> Handler NoContent
        writeToFile (_, h) msg = case msg of
          "illegal" -> error "wait, that's illegal!"
          legalMsg -> liftIO $ do
            putStrLn "writing file"
            hPutStrLn h legalMsg
            putStrLn "wrote file"
            pure NoContent
```

Finally we run the server in the background while we post messages to it.

``` haskell
runApp :: IO ()
runApp = run 8080 (serveWithContext api appContext $ server)

postMsg :: String -> ClientM NoContent
postMsg = client api

main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp) killThread $ \_ -> do
    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      liftIO $ putStrLn "sending hello message"
      _ <- postMsg "hello"
      liftIO $ putStrLn "sending illegal message"
      _ <- postMsg "illegal"
      liftIO $ putStrLn "done"
    print ms
```

This program prints

```
sending hello message
opening file
opened file
writing file
wrote file
closing file
closed file
sending illegal message
opening file
opened file
closing file
closed file
wait, that's illegal!
CallStack (from HasCallStack):
  error, called at ManagedResource.lhs:63:24 in main:Main
Left (FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Http, baseUrlHost = "localhost", baseUrlPort = 8080, baseUrlPath = ""},""), requestQueryString = fromList [], requestBody = Just ((),text/plain;charset=utf-8), requestAccept = fromList [], requestHeaders = fromList [], requestHttpVersion = HTTP/1.1, requestMethod = "POST"}) (Response {responseStatusCode = Status {statusCode = 500, statusMessage = "Internal Server Error"}, responseHeaders = fromList [("Transfer-Encoding","chunked"),("Date","Thu, 24 Nov 2022 21:04:47 GMT"),("Server","Warp/3.3.23"),("Content-Type","text/plain; charset=utf-8")], responseHttpVersion = HTTP/1.1, responseBody = "Something went wrong"}))
```

and appends to a file called `test.txt`. We can see from the output that when a legal message is sent, the file is opened, written to, and closed. We can also see that when an illegal message is sent, the file is opened but not written to. Crucially, it is still closed even though the handler threw an exception.
