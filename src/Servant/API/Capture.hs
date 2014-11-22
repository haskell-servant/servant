{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Capture (Capture) where

import Data.Proxy
import Data.Text
import GHC.TypeLits
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Common.Req
import Servant.Common.Text
import Servant.Docs
import Servant.Server

-- | Capture a value from the request path under a certain type @a@.
--
-- Example:
--
-- >            -- GET /books/:isbn
-- > type MyApi = "books" :> Capture "isbn" Text :> Get Book
data Capture sym a

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = fromText

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> EitherT (Int, String) IO Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type Server (Capture capture a :> sublayout) =
     a -> Server sublayout

  route Proxy subserver request respond = case pathInfo request of
    (first : rest)
      -> case captured captureProxy first of
           Nothing  -> respond $ failWith NotFound
           Just v   -> route (Proxy :: Proxy sublayout) (subserver v) request{
                         pathInfo = rest
                       } respond
    _ -> respond $ failWith NotFound

    where captureProxy = Proxy :: Proxy (Capture capture a)

-- | If you use a 'Capture' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'Capture'.
-- That function will take care of inserting a textual representation
-- of this value at the right place in the request path.
--
-- You can control how values for this type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBook :: Text -> BaseUrl -> EitherT String IO Book
-- > getBook = client myApi
-- > -- then you can just use "getBook" to query that endpoint
instance (KnownSymbol capture, ToText a, HasClient sublayout)
      => HasClient (Capture capture a :> sublayout) where

  type Client (Capture capture a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req val =
    clientWithRoute (Proxy :: Proxy sublayout) $
      appendToPath p req

    where p = unpack (toText val)

-- | @"books" :> 'Capture' "isbn" Text@ will appear as
-- @/books/:isbn@ in the docs.
instance (KnownSymbol sym, ToCapture (Capture sym a), HasDocs sublayout)
      => HasDocs (Capture sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action')

    where sublayoutP = Proxy :: Proxy sublayout
          captureP = Proxy :: Proxy (Capture sym a)

          action' = over captures (|> toCapture captureP) action
          endpoint' = over path (\p -> p++"/:"++symbolVal symP) endpoint
          symP = Proxy :: Proxy sym
