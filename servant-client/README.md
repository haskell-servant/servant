# servant-client

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

This library lets you automatically derive Haskell functions that let you query each endpoint of a *servant* webservice.

## Example

``` haskell
type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
        :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books

myApi :: Proxy MyApi
myApi = Proxy

getAllBooks :: EitherT String IO [Book]
postNewBook :: Book -> EitherT String IO Book
-- 'client' allows you to produce operations to query an API from a client.
(getAllBooks :<|> postNewBook) = client myApi host
  where host = BaseUrl Http "localhost" 8080
```
