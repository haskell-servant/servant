-- | This module defines sever-side handlers that let you serve static files
--   and your API's docs.
--
-- - 'serveDirectory' lets you serve anything that lives under a particular
--   directory on your filesystem.
-- - 'serveDocumentation' lets you serve the markdown-version of the docs for
--   your API.
module Servant.Utils.StaticFiles (
  serveDirectory,
  serveDocumentation,
 ) where

import Data.Proxy
import Data.String.Conversions
import Filesystem.Path.CurrentOS (decodeString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static

import Servant.API.Raw
import Servant.Docs
import Servant.Server

-- | Serve anything under the specified directory as a 'Raw' endpoint.
--
-- @
-- type MyApi = "static" :> Raw
--
-- server :: Server MyApi
-- server = serveDirectory "\/var\/www"
-- @
--
-- would capture any request to @\/static\/\<something>@ and look for
-- @\<something>@ under @\/var\/www@.
--
-- It will do its best to guess the MIME type for that file, based on the extension,
-- and send an appropriate /Content-Type/ header if possible.
--
-- If your goal is to serve HTML, CSS and Javascript files that use the rest of the API
-- as a webapp backend, you will most likely not want the static files to be hidden
-- behind a /\/static\// prefix. In that case, remember to put the 'serveDirectory'
-- handler in the last position, because /servant/ will try to match the handlers
-- in order.
serveDirectory :: FilePath -> Server Raw
serveDirectory documentRoot =
  staticApp (defaultFileServerSettings (decodeString (documentRoot ++ "/")))

-- | Serve your API's docs as markdown embedded in an html \<pre> tag.
--
-- > type MyApi = "users" :> Get [User]
-- >         :<|> "docs   :> Raw
-- >
-- > apiProxy :: Proxy MyApi
-- > apiProxy = Proxy
-- >
-- > server :: Server MyApi
-- > server = listUsers
-- >     :<|> serveDocumentation apiProxy
serveDocumentation :: HasDocs api => Proxy api -> Server Raw
serveDocumentation proxy _request respond =
  respond $ responseLBS ok200 [] $ cs $ toHtml $ markdown $ docs proxy

toHtml :: String -> String
toHtml md =
  "<html>" ++
  "<body>" ++
  "<pre>" ++
  md ++
  "</pre>" ++
  "</body>" ++
  "</html>"
