{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.ReqBody where

import Control.Applicative
import Data.Aeson
import Data.Proxy
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Common.Req
import Servant.Docs
import Servant.Server

-- | Extract the request body as a value of type @a@.
--
-- Example:
--
-- >            -- POST /books
-- > type MyApi = "books" :> ReqBody Book :> Post Book
data ReqBody a

-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody Book :> Post Book
-- >
-- > server :: Server MyApi
-- > server = postBook
-- >   where postBook :: Book -> EitherT (Int, String) IO Book
-- >         postBook book = ...insert into your db...
instance (FromJSON a, HasServer sublayout)
      => HasServer (ReqBody a :> sublayout) where

  type Server (ReqBody a :> sublayout) =
    a -> Server sublayout

  route Proxy subserver request respond = do
    mrqbody <- decode' <$> lazyRequestBody request
    case mrqbody of
      Nothing -> respond $ failWith InvalidBody
      Just v  -> route (Proxy :: Proxy sublayout) (subserver v) request respond

-- | If you use a 'ReqBody' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'ReqBody'.
-- That function will take care of encoding this argument as JSON and
-- of using it as the request body.
--
-- All you need is for your type to have a 'ToJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody Book :> Post Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > addBook :: Book -> BaseUrl -> EitherT String IO Book
-- > addBook = client myApi
-- > -- then you can just use "addBook" to query that endpoint
instance (ToJSON a, HasClient sublayout)
      => HasClient (ReqBody a :> sublayout) where

  type Client (ReqBody a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req body =
    clientWithRoute (Proxy :: Proxy sublayout) $
      setRQBody (encode body) req

instance (ToSample a, HasDocs sublayout)
      => HasDocs (ReqBody a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout

          action' = action & rqbody .~ sampleByteString p
          p = Proxy :: Proxy a
