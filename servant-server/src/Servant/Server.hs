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
  , ServerT

    -- * Enter
    -- Applying functions to all handlers
  , Enter(..)

    -- * Default error type
  , ServantErr(..)
    -- ** 3XX
  , err300
  , err301
  , err302
  , err303
  , err304
  , err305
  , err307
    -- ** 4XX
  , err400
  , err401
  , err402
  , err403
  , err404
  , err405
  , err406
  , err407
  , err409
  , err410
  , err411
  , err412
  , err413
  , err414
  , err415
  , err416
  , err417
   -- * 5XX
  , err500
  , err501
  , err502
  , err503
  , err504
  , err505

  ) where

import Data.Proxy (Proxy)
import Network.Wai (Application)
import Servant.API (Canonicalize, canonicalize)
import Servant.Server.Internal
import Servant.Server.Internal.ServantErr
import Servant.Server.Internal.Enter


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
