module Servant.Utils.StaticFiles where

import Filesystem.Path.CurrentOS (decodeString)
import Network.Wai.Application.Static

import Servant.API
import Servant.Server

serveDirectory :: FilePath -> Server Raw
serveDirectory documentRoot =
  staticApp (defaultFileServerSettings (decodeString (documentRoot ++ "/")))
