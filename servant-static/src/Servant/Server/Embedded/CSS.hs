-- | This module contains 'Generators' for processing and embedding CSS.
module Servant.Server.Embedded.CSS (
    embedWithLess
  , embedWithPostCSS
) where

import Data.List (intersperse)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (TExp(..), liftString, lift)
import Servant.Server.Embedded.Files (compressTool)
import Servant.Server.Embedded.TH
import qualified Data.ByteString.Lazy as BL

-- | Execute <http://lesscss.org/ lesscss> and serve the resulting CSS.  It assumes that
-- @lessc@ and @cleancss@ are installed in @node_modules@, so run @npm install less clean-css@.
-- During development, @lessc@ will be executed on every request so a browser refresh is enough to
-- reload any changes to the CSS files.  During production, @lessc@ is executed at compile time and
-- the resulting CSS is passed through @cleancss@.  The 'FilePath' is relative to the directory
-- containing the @.cabal@ file.
embedWithLess :: EntryVarName -> FilePath -> Generator
embedWithLess n f = do
    let less = compressTool "sh" ["-c", "node_modules/less/bin/lessc - | node_modules/clean-css/bin/cleancss"]
    return EmbeddableEntry
        { ebeName = n
        , ebeMimeType = "text/css"
        , ebeProduction = etagAsHash <$> (BL.readFile f >>= less)
        , ebeDevelReload = [|| BL.readFile $$(TExp <$> litE (stringL f)) >>= 
                               compressTool "node_modules/less/bin/lessc" ["-"]
                           ||]
        }

-- | Compile a file using postcss.
compilePostCSS :: EntryVarName -> FilePath -> Bool -> [String] -> IO BL.ByteString
compilePostCSS n fp sourceMaps plugins = compressTool "node" ["-e", script] ""
    where
        mapArg = if sourceMaps then ", map:true" else ""
        addRequire plugin = "require('" ++ plugin ++ "')"
        requirePlugins = concat $ intersperse "," $ map addRequire plugins
        script = unlines
          [ "require('fs').readFile('" ++ fp ++ "', function(err, css) {"
          ,      "if (err) console.log('Error: ' + err.toString());"
          ,      "require('postcss')([" ++ requirePlugins ++ "])"
          ,          ".process(css, { from: '" ++ fp ++ "', to: '" ++ show n ++ ".css'" ++ mapArg ++ "})"
          ,          ".then(function(result) {"
          ,               "console.log(result.css);"
          ,          "})"
          ,          ".catch(function(err) {"
          ,               "console.log('Error:' + err.toString());"
          ,          "});"
          ,  "});"
          ]

-- | Use <https://github.com/postcss/postcss postcss> to compile and embed CSS.
-- It assumes that the postcss plugins and @cssnano@ are installed in @node_modules@.
-- During development, @postcss@ will be executed on every request so a browser refresh is enough to
-- reload any changes to the CSS files.  In addition, during development sourceMaps will be created.
-- During production, @postcss@ is executed at compile time in addition to the @cssnano@ plugin.
-- The 'FilePath' is relative to the directory containing the @.cabal@ file.
embedWithPostCSS :: EntryVarName -- ^ The variable name to create.
                 -> FilePath -- ^ Path to CSS file to compile.
                 -> [String] -- ^ List of postcss plugins.  When compiling for production,
                             --   @cssnano@ is added to the end of this list.  Each plugin
                             --   in this list must be installed into @node_modules@ so that
                             --   when @node@ executes @require(plugin)@ the plugin is loaded.
                 -> Generator
embedWithPostCSS n fp plugins = return
    EmbeddableEntry
        { ebeName = n
        , ebeMimeType = "text/css"
        , ebeProduction = etagAsHash <$> compilePostCSS (show n) fp False (plugins ++ ["cssnano"])
        , ebeDevelReload = [|| compilePostCSS $$(TExp <$> liftString (show n))
                                              $$(TExp <$> liftString fp)
                                              True
                                              $$(TExp <$> lift plugins)
                           ||] 
        }
