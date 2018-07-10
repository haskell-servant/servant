{-# LANGUAGE CPP #-}
-- | This module defines server-side handlers that lets you serve static files.
--
-- The most common needs for a web application are covered by
-- 'serveDirectoryWebApp`, but the other variants allow you to use
-- different `StaticSettings` and 'serveDirectoryWith' even allows you
-- to specify arbitrary 'StaticSettings' to be used for serving static files.
module Servant.Server.StaticFiles
  ( serveDirectoryWebApp
  , serveDirectoryWebAppLookup
  , serveDirectoryFileServer
  , serveDirectoryEmbedded
  , serveDirectoryWith
  , -- * Deprecated
    serveDirectory
  ) where

import           Data.ByteString
                 (ByteString)
import           Network.Wai.Application.Static
import           Servant.API.Raw
                 (Raw)
import           Servant.Server
                 (ServerT, Tagged (..))
import           System.FilePath
                 (addTrailingPathSeparator)
import           WaiAppStatic.Storage.Filesystem
                 (ETagLookup)

-- | Serve anything under the specified directory as a 'Raw' endpoint.
--
-- @
-- type MyApi = "static" :> Raw
--
-- server :: Server MyApi
-- server = serveDirectoryWebApp "\/var\/www"
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
-- behind a /\/static\// prefix. In that case, remember to put the 'serveDirectoryWebApp'
-- handler in the last position, because /servant/ will try to match the handlers
-- in order.
--
-- Corresponds to the `defaultWebAppSettings` `StaticSettings` value.
serveDirectoryWebApp :: FilePath -> ServerT Raw m
serveDirectoryWebApp = serveDirectoryWith . defaultWebAppSettings . fixPath

-- | Same as 'serveDirectoryWebApp', but uses `defaultFileServerSettings`.
serveDirectoryFileServer :: FilePath -> ServerT Raw m
serveDirectoryFileServer = serveDirectoryWith . defaultFileServerSettings . fixPath

-- | Same as 'serveDirectoryWebApp', but uses 'webAppSettingsWithLookup'.
serveDirectoryWebAppLookup :: ETagLookup -> FilePath -> ServerT Raw m
serveDirectoryWebAppLookup etag =
  serveDirectoryWith . flip webAppSettingsWithLookup etag . fixPath

-- | Uses 'embeddedSettings'.
serveDirectoryEmbedded :: [(FilePath, ByteString)] -> ServerT Raw m
serveDirectoryEmbedded files = serveDirectoryWith (embeddedSettings files)

-- | Alias for 'staticApp'. Lets you serve a directory
--   with arbitrary 'StaticSettings'. Useful when you want
--   particular settings not covered by the four other
--   variants. This is the most flexible method.
serveDirectoryWith :: StaticSettings -> ServerT Raw m
serveDirectoryWith = Tagged . staticApp

-- | Same as 'serveDirectoryFileServer'. It used to be the only
--   file serving function in servant pre-0.10 and will be kept
--   around for a few versions, but is deprecated.
serveDirectory :: FilePath -> ServerT Raw m
serveDirectory = serveDirectoryFileServer
{-# DEPRECATED serveDirectory "Use serveDirectoryFileServer instead" #-}

fixPath :: FilePath -> FilePath
fixPath = addTrailingPathSeparator
