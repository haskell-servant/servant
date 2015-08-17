{-# LANGUAGE CPP #-}
-- | This module defines a sever-side handler that lets you serve static files.
--
-- - 'serveDirectory' lets you serve anything that lives under a particular
--   directory on your filesystem.
module Servant.Utils.StaticFiles (
  serveDirectory,
 ) where

import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 staticApp)
import           Servant.API.Raw                (Raw)
import           Servant.Server                 (Server)
import           System.FilePath                (addTrailingPathSeparator)
#if !MIN_VERSION_wai_app_static(3,1,0)
import           Filesystem.Path.CurrentOS      (decodeString)
#endif

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
serveDirectory =
#if MIN_VERSION_wai_app_static(3,1,0)
    staticApp . defaultFileServerSettings . addTrailingPathSeparator
#else
    staticApp . defaultFileServerSettings . decodeString . addTrailingPathSeparator
#endif
