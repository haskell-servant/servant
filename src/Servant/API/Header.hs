{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Header where

import Data.Proxy
import Data.String
import Data.Text.Encoding (decodeUtf8)
import GHC.TypeLits
import Network.Wai
import Servant.API.Sub
import Servant.Common.Text
import Servant.Server

-- | Extract the given header's value as a value of type @a@.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "from" Referer :> Get Referer
data Header sym a

-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromText' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> EitherT (Int, String) IO referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (Header sym a :> sublayout) where

  type Server (Header sym a :> sublayout) =
    Maybe a -> Server sublayout

  route Proxy subserver request respond = do
    let mheader = fromText . decodeUtf8 =<< lookup str (requestHeaders request)
    route (Proxy :: Proxy sublayout) (subserver mheader) request respond

      where str = fromString $ symbolVal (Proxy :: Proxy sym)
