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

    -- * Deprecated
  , serveDirectory
  )
where

import Data.ByteString (ByteString)
import Network.Wai.Application.Static
import Servant.API.Raw (Raw)
import System.FilePath (addTrailingPathSeparator)
import WaiAppStatic.Storage.Filesystem (ETagLookup)

import Servant.Server (ServerT, Tagged (..))

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
--
--   == Preprocessing files before serving them
--
--   Since 'StaticSettings' is a plain record, you can start from one of
--   the default settings (e.g. 'defaultFileServerSettings') and wrap its
--   'ssLookupFile' field to change how some files are served, while
--   keeping everything else (index files, MIME types, directory listing,
--   ...) as is. For instance, to render @text\/*@ files through a template
--   engine before sending them:
--
--   > import qualified Data.ByteString as BS
--   > import qualified Data.ByteString.Lazy as LBS
--   > import qualified Data.Text as T
--   > import Network.Wai (responseLBS)
--   > import Network.Wai.Application.Static
--   > import System.FilePath ((</>))
--   > import WaiAppStatic.Types
--   >
--   > -- 'render' is your template engine; 'isTemplate' decides which files
--   > -- should go through it, e.g. based on 'ssGetMimeType'.
--   > templatedSettings
--   >   :: (FilePath -> Bool)
--   >   -> (BS.ByteString -> IO LBS.ByteString)
--   >   -> FilePath
--   >   -> StaticSettings
--   > templatedSettings isTemplate render root =
--   >   settings {ssLookupFile = lookupFile}
--   >   where
--   >     settings = defaultFileServerSettings root
--   >     lookupFile pieces = do
--   >       result <- ssLookupFile settings pieces
--   >       case result of
--   >         LRFile file
--   >           | isTemplate (T.unpack (fromPiece (fileName file))) -> do
--   >               let path = foldl (</>) root (map (T.unpack . fromPiece) pieces)
--   >               rendered <- render =<< BS.readFile path
--   >               pure $ LRFile file
--   >                 { fileGetSize = fromIntegral (LBS.length rendered)
--   >                 , fileToResponse = \status headers -> responseLBS status headers rendered
--   >                 , fileGetHash = pure Nothing
--   >                 , fileGetModified = Nothing
--   >                 }
--   >         other -> pure other
--   >
--   > server :: Server ("static" :> Raw)
--   > server = serveDirectoryWith (templatedSettings isTemplate render "/var/www")
--
--   Note that index files (e.g. @index.html@ for a directory) are also
--   looked up through 'ssLookupFile', so they get preprocessed as well.
--   The hash and modification time are dropped so that clients do not
--   cache a rendered page based on the metadata of its template.
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
