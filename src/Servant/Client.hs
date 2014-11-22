{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client
  ( client
  , HasClient(..)
  , module Servant.Common.BaseUrl
  ) where

import Data.Proxy

import Servant.Common.BaseUrl
import Servant.Common.Req

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: BaseUrl -> EitherT String IO [Book]
-- > postNewBook :: Book -> BaseUrl -> EitherT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi
client :: HasClient layout => Proxy layout -> Client layout
client p = clientWithRoute p defReq

-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. Use 'client'
-- directly, this class implements the client-side
-- behavior of each combinator but you don't have to worry about it.
class HasClient layout where
  type Client layout :: *
  clientWithRoute :: Proxy layout -> Req -> Client layout
