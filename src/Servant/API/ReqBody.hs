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
