-- | This module contains 'Generators' which embed and potentially process
-- files.
module Servant.Server.Embedded.Files (
    embedFile
  , embedFileWith
  , concatFiles
  , concatFilesWith

  -- * Compression tools
  , compressTool
  , uglifyJs
  , yuiJavascript
  , yuiCSS
  , closureJs
) where

import Control.Concurrent.Async (Concurrently (..))
import Control.Monad (when)
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceHandle)
import Data.Maybe (isNothing)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (TExp(..))
import Network.Mime (defaultMimeLookup, MimeType)
import Servant.Server.Embedded.TH
import System.Directory (findExecutable, doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List as C
import qualified System.Process as Proc
import qualified Data.Text as T

-- | Embed a file.  When compiling for production, the file content is embedded
-- into the executable.  When compiling for development, the file will be reloaded
-- from disk on every request.  The 'FilePath' must be specified relative to the
-- directory which contains the @.cabal@ file.
embedFile :: EntryVarName -> FilePath -> Generator
embedFile n fp = return
    EmbeddableEntry
      { ebeName = n
      , ebeMimeType = defaultMimeLookup $ T.pack fp
      , ebeProduction = etagAsHash <$> BL.readFile fp
      , ebeDevelReload = [|| BL.readFile $$(TExp <$> litE (stringL fp)) ||]
      }

-- | Embed a file and execute a processing function at compile time.
--
-- The processing function is only run when compiling for production, and the processing function is
-- executed at compile time.  During development, on every request the file is reloaded
-- and served as a single resource at the given location without being processed.
embedFileWith :: (BL.ByteString -> IO BL.ByteString) -> EntryVarName -> FilePath -> Generator
embedFileWith process n fp = return
    EmbeddableEntry
        { ebeName = n
        , ebeMimeType = defaultMimeLookup $ T.pack fp
        , ebeProduction = etagAsHash <$> (BL.readFile fp >>= process)
        , ebeDevelReload = [|| BL.readFile $$(TExp <$> litE (stringL fp)) ||]
        }

-- | Concat a list of files into a single bytestring and serve the resuling content.
-- The 'FilePath's must be given relative to the directory containing the @.cabal@ file.
concatFiles :: EntryVarName -> MimeType -> [FilePath] -> Generator
concatFiles n mime files = do
    let load = BL.concat <$> mapM BL.readFile files
    let filesExp = TExp <$> listE (map (litE . stringL) files)
    return EmbeddableEntry
      { ebeName = n
      , ebeMimeType = mime
      , ebeProduction = etagAsHash <$> load
      , ebeDevelReload = [|| BL.concat <$> mapM BL.readFile $$filesExp ||]
      }

-- | Concat a list of files into a single bytestring.  When compiling for production, pass the
-- resulting bytestring through the processing function.  The 'FilePath's must be given relative
-- to the directory containing the @.cabal@ file.
concatFilesWith :: (BL.ByteString -> IO BL.ByteString) -> EntryVarName -> MimeType -> [FilePath] -> Generator
concatFilesWith process n mime files = do
    let load = (BL.concat <$> mapM BL.readFile files) >>= process
    let filesExp = TExp <$> listE (map (litE . stringL) files)
    return EmbeddableEntry
      { ebeName = n
      , ebeMimeType = mime
      , ebeProduction = etagAsHash <$> load
      , ebeDevelReload = [|| BL.concat <$> mapM BL.readFile $$filesExp ||]
      }

-- | Helper to convert a process into a compression function.  The process
-- should be set up to take input from standard input and write to standard output.
compressTool :: FilePath -- ^ program
             -> [String] -- ^ options
             -> BL.ByteString -> IO BL.ByteString
compressTool f opts ct = do
    fExists <- doesFileExist f
    mpath <- if fExists
                then return $ Just f
                else findExecutable f
    when (isNothing mpath) $
        fail $ "Unable to find " ++ f
    let p = (Proc.proc f opts)
                { Proc.std_in = Proc.CreatePipe
                , Proc.std_out = Proc.CreatePipe
                }
    (Just hin, Just hout, _, ph) <- Proc.createProcess p
    (compressed, (), code) <- runConcurrently $ (,,)
        <$> Concurrently (sourceHandle hout $$ C.consume)
        <*> Concurrently (BL.hPut hin ct >> hClose hin)
        <*> Concurrently (Proc.waitForProcess ph)
    if code == ExitSuccess
        then do
            putStrLn $ "Compressed successfully with " ++ f
            return $ BL.fromChunks compressed
        else error $ "compressTool: compression failed with " ++ f

-- | Use <https://github.com/mishoo/UglifyJS2 UglifyJS2> to compress javascript.
-- Assumes @node_modules\/uglifyjs\/bin\/uglifyjs@ exists so just run @npm install uglifyjs@.  It
-- uses options @[\"-m\", \"-c\"]@ to both mangle and compress.
uglifyJs :: BL.ByteString -> IO BL.ByteString
uglifyJs = compressTool "./node_modules/uglifyjs/bin/uglifyjs" ["-m", "-c"]

-- | Use <http://yui.github.io/yuicompressor/ YUI Compressor> to compress javascript.
-- Assumes a script @yuicompressor@ is located in the path.  If not, you can still
-- use something like
--
-- > compressTool "java" ["-jar", "/path/to/yuicompressor.jar", "--type", "js"]
yuiJavascript :: BL.ByteString -> IO BL.ByteString
yuiJavascript = compressTool "yuicompressor" ["--type", "js"]

-- | Use <http://yui.github.io/yuicompressor/ YUI Compressor> to compress CSS.
-- Assumes a script @yuicompressor@ is located in the path.
yuiCSS :: BL.ByteString -> IO BL.ByteString
yuiCSS = compressTool "yuicompressor" ["--type", "css"]

-- | Use <https://developers.google.com/closure/compiler/ Closure> to compress
-- javascript using the default options.  Assumes a script @closure@ is located in
-- the path. If not, you can still run using
--
-- > compressTool "java" ["-jar", "/path/to/compiler.jar"]
closureJs :: BL.ByteString -> IO BL.ByteString
closureJs = compressTool "closure" []
