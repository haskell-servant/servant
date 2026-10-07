module Servant.Server.Embedded.Ghcjs (
    embedGhcjsFromStackBuild
) where

import Data.List (intercalate)
import Data.Monoid ((<>))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (TExp(..))
import Servant.Server.Embedded.Files (compressTool)
import Servant.Server.Embedded.TH
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.Process (readCreateProcess, proc, CreateProcess(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

embedGhcjsFromStackBuild :: EntryVarName -> FilePath -> String -> [String] -> Generator
embedGhcjsFromStackBuild name dir projName varsToProtect = do
    curDir <- runIO getCurrentDirectory
    let dir' = curDir </> dir
    installDir <- runIO $ readCreateProcess (proc "stack" ["path", "--local-install-root"]) { cwd = Just dir' } ""
    let fp = (filter (/='\n') installDir) </> "bin" </> (projName++".jsexe") </> "all.js"
    return EmbeddableEntry
        { ebeName = name
        , ebeMimeType = "application/javascript"
        , ebeProduction = etagAsHash <$> (BL.readFile fp >>= compressGhcjs varsToProtect)
        , ebeDevelReload = [|| BL.readFile $$(TExp <$> litE (stringL fp)) ||]
        }

compressGhcjs :: [String] -> BL.ByteString -> IO BL.ByteString
compressGhcjs varsToProtect allJs = removeBadJs >>= closureCompile
    where
        vars = TL.encodeUtf8 $ TL.pack $ intercalate "," varsToProtect
        addWindow x = "window['" ++ x ++ "']"
        windowVars = TL.encodeUtf8 $ TL.pack $ intercalate "," $ map addWindow varsToProtect
        input = BL.concat [ "(function(global"
                          , if null varsToProtect then "" else "," <> vars
                          , ") {"
                          , allJs
                          , "})(window"
                          , if null varsToProtect then "" else "," <> windowVars
                          , ");"
                          ]

        removeBadJs = compressTool "sed" ["s/goog.provide.*//;s/goog.require.*//;s/final\\([^a-z]\\)/final0\\1/"] input
        closureCompile = compressTool "closure" ["--compilation_level=ADVANCED_OPTIMIZATIONS"]
