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

serveDirectory :: FilePath -> Server Raw
serveDirectory documentRoot =
  staticApp (defaultFileServerSettings (decodeString (documentRoot ++ "/")))

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
