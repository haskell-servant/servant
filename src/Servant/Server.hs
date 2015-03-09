{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module lets you implement 'Server's for defined APIs. You'll
-- most likely just need 'serve'.
module Servant.Server
  ( -- * Run a wai application from an API
    serve

  , -- * Construct a wai Application from an API
    toApplication

  , -- * Handlers for all standard combinators
    HasServer(..)
  , Server
  ) where

import Data.Proxy (Proxy)
import Network.Wai (Application)
import Servant.API (Canonicalize, canonicalize)
import Servant.Server.Internal


-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
--
-- Example:
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > app :: Application
-- > app = serve myApi server
-- >
-- > main :: IO ()
-- > main = Network.Wai.Handler.Warp.run 8080 app
serve :: HasServer (Canonicalize layout) => Proxy layout -> Server layout -> Application
serve p server = toApplication (route (canonicalize p) server)

type Server layout = Server' (Canonicalize layout)
