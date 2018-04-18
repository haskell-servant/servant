# servant-client

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

This library lets you automatically derive Haskell functions that let you query each endpoint of a *servant* webservice.

## Example

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Proxy
import Data.Text
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client


type Book = Text

type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
        :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books

myApi :: Proxy MyApi
myApi = Proxy

-- 'client' allows you to produce operations to query an API from a client.
postNewBook :: Book -> ClientM Book
getAllBooks :: ClientM [Book]
(getAllBooks :<|> postNewBook) = client myApi


main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM getAllBooks (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right books -> print books
```
