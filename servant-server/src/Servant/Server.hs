{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module lets you implement 'Server's for defined APIs. You'll
-- most likely just need 'serve'.
module Servant.Server
  ( -- * Run a wai application from an API
    serve
  , serveWithContext

  , -- * Construct a wai Application from an API
    toApplication

  , -- * Handlers for all standard combinators
    HasServer(..)
  , Server

    -- * Enter
    -- $enterDoc

    -- ** Basic functions and datatypes
  , enter
  , (:~>)(..)
    -- ** `Nat` utilities
  , liftNat
  , runReaderTNat
  , evalStateTLNat
  , evalStateTSNat
  , logWriterTLNat
  , logWriterTSNat
  -- ** Functions based on <https://hackage.haskell.org/package/mmorph mmorph>
  , hoistNat
  , embedNat
  , squashNat
  , generalizeNat
  , tweakResponse

  -- * Context
  , Context(..)
  , HasContextEntry(getContextEntry)
  -- ** NamedContext
  , NamedContext(..)
  , descendIntoNamedContext

  -- * Basic Authentication
  , BasicAuthCheck(BasicAuthCheck, unBasicAuthCheck)
  , BasicAuthResult(..)

  -- * General Authentication
  -- , AuthHandler(unAuthHandler)
  -- , AuthServerData
  -- , mkAuthHandler

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
   -- ** 5XX
  , err500
  , err501
  , err502
  , err503
  , err504
  , err505

  ) where

import           Data.Proxy                    (Proxy)
import           Network.Wai                   (Application)
import           Servant.Server.Internal
import           Servant.Server.Internal.Enter


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
--
serve :: (HasServer layout '[]) => Proxy layout -> Server layout -> Application
serve p = serveWithContext p EmptyContext

serveWithContext :: (HasServer layout context)
    => Proxy layout -> Context context -> Server layout -> Application
serveWithContext p context server = toApplication (runRouter (route p context d))
  where
    d = Delayed r r r r (\ _ _ _ -> Route server)
    r = return (Route ())


-- Documentation

-- $enterDoc
-- Sometimes our cherished `ExceptT` monad isn't quite the type you'd like for
-- your handlers. Maybe you want to thread some configuration in a @Reader@
-- monad. Or have your types ensure that your handlers don't do any IO. Enter
-- `enter`.
--
-- With `enter`, you can provide a function, wrapped in the `(:~>)` / `Nat`
-- newtype, to convert any number of endpoints from one type constructor to
-- another. For example
--
-- >>> import Control.Monad.Reader
-- >>> import qualified Control.Category as C
-- >>> type ReaderAPI = "ep1" :> Get '[JSON] Int :<|> "ep2" :> Get '[JSON] String
-- >>> let readerServer = return 1797 :<|> ask :: ServerT ReaderAPI (Reader String)
-- >>> let mainServer = enter (generalizeNat C.. (runReaderTNat "hi")) readerServer :: Server ReaderAPI
--

-- $setup
-- >>> import Servant.API
-- >>> import Servant.Server
