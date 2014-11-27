{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Alternative where

import Data.Monoid
import Data.Proxy
import Servant.Server

-- | Union of two APIs, first takes precedence in case of overlap.
--
-- Example:
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
data a :<|> b = a :<|> b
infixr 8 :<|>

-- | A server for @a ':<|>' b@ first tries to match the request again the route
--   represented by @a@ and if it fails tries @b@. You must provide a request
--   handler for each route.
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (a :<|> b) request respond =
    route pa a request $ \ mResponse ->
      if isMismatch mResponse
        then route pb b request $ \mResponse' -> respond (mResponse <> mResponse')
        else respond mResponse

    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
