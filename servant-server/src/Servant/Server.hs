{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

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
  , EmptyServer
  , emptyServer
  , Handler (..)
  , runHandler

    -- * Debugging the server layout
  , layout
  , layoutWithContext

    -- * Enter / hoisting server
  , hoistServer

  -- ** Functions based on <https://hackage.haskell.org/package/mmorph mmorph>
  , tweakResponse

  -- * Context
  , Context(..)
  , HasContextEntry(getContextEntry)
  , type (.++)
  , (.++)
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
  , ServerError(..)
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
  , err418
  , err422
   -- ** 5XX
  , err500
  , err501
  , err502
  , err503
  , err504
  , err505

  -- * Formatting of errors from combinators
  --
  -- | You can configure how Servant will render errors that occur while parsing the request.

  , ErrorFormatter
  , NotFoundErrorFormatter
  , ErrorFormatters

  , bodyParserErrorFormatter
  , urlParseErrorFormatter
  , headerParseErrorFormatter
  , notFoundErrorFormatter

  , DefaultErrorFormatters
  , defaultErrorFormatters

  , getAcceptHeader

  -- * Re-exports
  , Application
  , Tagged (..)
  , module Servant.Server.UVerb

  ) where

import           Data.Proxy
                 (Proxy (..))
import           Data.Tagged
                 (Tagged (..))
import           Data.Text
                 (Text)
import           Network.Wai
                 (Application)
import           Servant.Server.Internal
import           Servant.Server.UVerb


-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
--
-- Example:
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books
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
serve :: (HasServer api '[]) => Proxy api -> Server api -> Application
serve p = serveWithContext p EmptyContext

-- | Like 'serve', but allows you to pass custom context.
--
-- 'defaultErrorFormatters' will always be appended to the end of the passed context,
-- but if you pass your own formatter, it will override the default one.
serveWithContext :: ( HasServer api context
                    , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters )
    => Proxy api -> Context context -> Server api -> Application
serveWithContext p context server =
  toApplication (runRouter format404 (route p context (emptyDelayed (Route server))))
  where
    format404 = notFoundErrorFormatter . getContextEntry . mkContextWithErrorFormatter $ context

-- | Hoist server implementation.
--
-- Sometimes our cherished `Handler` monad isn't quite the type you'd like for
-- your handlers. Maybe you want to thread some configuration in a @Reader@
-- monad. Or have your types ensure that your handlers don't do any IO. Use
-- `hoistServer` (a successor of now deprecated @enter@).
--
-- With `hoistServer`, you can provide a function,
-- to convert any number of endpoints from one type constructor to
-- another. For example
--
-- /Note:/ 'Server' 'Raw' can also be entered. It will be retagged.
--
-- >>> import Control.Monad.Reader
-- >>> type ReaderAPI = "ep1" :> Get '[JSON] Int :<|> "ep2" :> Get '[JSON] String :<|> Raw :<|> EmptyAPI
-- >>> let readerApi = Proxy :: Proxy ReaderAPI
-- >>> let readerServer = return 1797 :<|> ask :<|> Tagged (error "raw server") :<|> emptyServer :: ServerT ReaderAPI (Reader String)
-- >>> let nt x = return (runReader x "hi")
-- >>> let mainServer = hoistServer readerApi nt readerServer :: Server ReaderAPI
--
hoistServer :: (HasServer api '[]) => Proxy api
            -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n
hoistServer p = hoistServerWithContext p (Proxy :: Proxy '[])

-- | The function 'layout' produces a textual description of the internal
-- router layout for debugging purposes. Note that the router layout is
-- determined just by the API, not by the handlers.
--
-- Example:
--
-- For the following API
--
-- > type API =
-- >        "a" :> "d" :> Get '[JSON] NoContent
-- >   :<|> "b" :> Capture "x" Int :> Get '[JSON] Bool
-- >   :<|> "c" :> Put '[JSON] Bool
-- >   :<|> "a" :> "e" :> Get '[JSON] Int
-- >   :<|> "b" :> Capture "x" Int :> Put '[JSON] Bool
-- >   :<|> Raw
--
-- we get the following output:
--
-- > /
-- > ├─ a/
-- > │  ├─ d/
-- > │  │  └─•
-- > │  └─ e/
-- > │     └─•
-- > ├─ b/
-- > │  └─ <capture>/
-- > │     ├─•
-- > │     ┆
-- > │     └─•
-- > ├─ c/
-- > │  └─•
-- > ┆
-- > └─ <raw>
--
-- Explanation of symbols:
--
-- [@├@] Normal lines reflect static branching via a table.
--
-- [@a/@] Nodes reflect static path components.
--
-- [@─•@] Leaves reflect endpoints.
--
-- [@\<capture\>/@] This is a delayed capture of a path component.
--
-- [@\<raw\>@] This is a part of the API we do not know anything about.
--
-- [@┆@] Dashed lines suggest a dynamic choice between the part above
-- and below. If there is a success for fatal failure in the first part,
-- that one takes precedence. If both parts fail, the \"better\" error
-- code will be returned.
--
layout :: (HasServer api '[]) => Proxy api -> Text
layout p = layoutWithContext p EmptyContext

-- | Variant of 'layout' that takes an additional 'Context'.
layoutWithContext :: (HasServer api context)
    => Proxy api -> Context context -> Text
layoutWithContext p context =
  routerLayout (route p context (emptyDelayed (FailFatal err501)))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import Servant.API
-- >>> import Servant.Server
