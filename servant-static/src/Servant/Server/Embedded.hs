-- | A module to embed static content such as javascript and CSS into the executable at compile time
-- so that it does not need to be distributed along with the server.  In addition, this module
-- supports processing of these resources before they are embedded, such as javascript or CSS
-- minification.  Finally, there is a development mode which will recompute each resource on every
-- request, so allow a simple browser refresh to reload potentially changed javascript or CSS.
--
-- To use this module, use 'EmbeddedContent' in your servant API definition.  For example,
--
-- >type MyAPI = "static" :> "js" :> "bootstrap.js" :> EmbeddedContent "application/javascript"
-- >        :<|> "static" :> "css" :> "bootstrap.css" :> EmbeddedContent "text/css"
-- >        :<|> "static" :> "css" :> "mysite.css" :> EmbeddedContent "text/css"
--
-- Then, decide on a generator for each 'EmbeddedContent'.  There are several generators which embed
-- files directly, minifiy files, and use 3rd party tools like less and postcss.  You can also
-- easily create your own generators.  Each generator is passed to 'embed' and will produce a
-- haskell variable of type 'EmbeddedEntry'.  For example,
--
-- >#if DEVELOPMENT
-- >#define DEV_BOOL True
-- >#else
-- >#define DEV_BOOL False
-- >#endif
-- >
-- >embed DEV_BOOL
-- >    [ embedFileWith uglifyJs "bootJs" "node_modules/bootstrap/dist/js/bootstrap.js"
-- >    , embedFile "bootCss" "node_modules/bootstrap/dist/css/bootstrap.min.css"
-- >    , embedWithPostCSS "mysiteCss" "css/mysite.css"
-- >    ]
--
-- The above template haskell splice will produce the following three variables automatically (you
-- do not need to enter anything extra):
-- 
-- >bootJs :: EmbeddedEntry "application/javascript"
-- >bootCss :: EmbeddedEntry "text/css"
-- >mysiteCss :: EmbeddedEntry "text/css"
--
-- These 'EmbeddedEntry's are used to create the server for the 'EmbeddedContent' endpoints.
--
-- >staticServer :: Server MyAPI
-- >staticServer = bootJs :<|> bootCss :<|> mysiteCss
--
-- If the DEVELOPMENT define is true (I suggest you use a cabal flag), on each
-- request the server will recompute the resource.  This means that the file will be reloaded from
-- disk or postcss will be re-executed on each request.  Thus when the DEVELOPMENT flag is true, a
-- browser refresh will reload and recompute the resources from disk.
--
-- When the DEVELOPMENT define is false, instead at compile time the resource will be loaded, the
-- processing will occur, it will be compressed with gzip, and finally the resulting bytes will be
-- embedded directly into the executable.  The server will then return this embedded content on each
-- request without computing anything or loading anything from disk.
--
-- In addition, when DEVELOPMENT is false, the server will use etags, 304 not modified responses,
-- and potentially Cache-Control headers to reduce the need for the client to re-request these
-- resources.  The server will always use etags to return 304 not modified responses.  By default,
-- these etags are the md5 hash of the content.  So for example if bootstrap is updated to a new
-- version, the file @node_modules\/bootstrap\/dist\/js\/bootstrap.js@ will change and so the etag
-- will be different.  Thus when the client re-requests the resource, the etag the client sends will
-- differ from the server and so the server will return the new content.
--
-- Using just etags still requires the client to send a request for each resource and for the
-- server to respond with 304 not modified the vast majority of the time.  To mitigate that, a
-- Cache-Control header can be configured to be used to tell the client to not re-request the
-- resource.  In this module, such a Cache-Control header is controlled by an etag query parameter
-- on the URL.  If the client requests the resource via a URL @\/static\/js\/bootstrap.js@, no
-- Cache-Control header is sent because when a new version of the server is released the client
-- might need to re-download the bootstrap.js.  If instead the client requested the
-- resource via the URL @\/static\/js\/bootstrap.js?etag=123456789@ and the etag is correct, a
-- Cache-Control header is set to tell the client to cache the resource for one year.  When a new
-- version of the server is released with an updated bootstrap version, the etag will change and as
-- long as the new server uses an HTML script tag referring to a URL with the new etag, the client
-- will download the new bootstrap version because the URL has changed.
--
-- The calculated etag is stored inside the 'EmbeddedEntry' created by template haskell and can be
-- passed to 'safeLink' in order to create a link which includes the correct etag.  The function
-- 'embeddedLink' is a simple wrapper around 'safeLink' which extracts the etag from the
-- 'EmbeddedEntry'.
--
-- >bootstrapJsLink :: URI
-- >bootstrapJsLink =
-- >  embeddedLink (Proxy :: Proxy MyAPI)
-- >               (Proxy :: Proxy ("static" :> "js" :> "bootstrap.js" :> EmbeddedContent "application/javascript"))
-- >               bootJs
module Servant.Server.Embedded(
    EmbeddedContent(..)
  , EntryVarName
  , Generator
  , EmbeddableEntry
  , EmbeddedEntry
  , embed
  , Etag(..)
  , embeddedLink

  -- * Generators
  , module Servant.Server.Embedded.Files
  , module Servant.Server.Embedded.CSS
  , module Servant.Server.Embedded.Ghcjs
) where

import Control.Monad (forM)
import Language.Haskell.TH
import Servant
import Servant.Server.Embedded.CSS
import Servant.Server.Embedded.Files
import Servant.Server.Embedded.Ghcjs
import Servant.Server.Embedded.TH
import Servant.Server.Embedded.Types

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | For each 'Generator', embed the result of the generator into the executable to produce
-- an 'EmbeddedEntry'.  Each resource can be embedded in two ways, controlled by the boolean passed
-- to 'embed'.  In development mode, the resource will be recomputed on each request allowing a
-- single browser refresh to reload the content.  In production mode, the resource is loaded and
-- embedded into the executable at compile time.
--
-- After creating the 'EmbeddedEntry', 'embed' will create a haskell variable to hold the
-- 'EmbeddedEntry'.  The name of the haskell variable is the 'EntryVarName' passed to the function
-- which creates the generator.
embed :: Bool -- ^ development mode?
      -> [Generator]
      -> Q [Dec]
embed dev gens = concat <$> do
    entries <- sequence gens
    forM entries $ \e -> do
        let n = mkName (ebeName e)

        let emb = if dev then embedDevel e else embedProduction e
        def <- valD (varP n) (normalB (unType <$> emb)) []

        let mime = T.unpack $ T.decodeUtf8 $ ebeMimeType e
        sig <- sigD n (conT ''EmbeddedEntry `appT` litT (strTyLit mime))

        return [sig, def]

-- | The 'HasLink' instance of 'EmbeddedContent' requires an 'Etag' be passed to create the link.
-- This etag is stored inside the 'EmbeddedEntry' on the server, and so 'embeddedLink' is a simple
-- wrapper around 'safeLink' which extracts the 'Etag' from the 'EmbeddedEntry' and then passes it
-- to 'safeLink'.
embeddedLink :: (IsElem endpoint api, HasLink endpoint, MkLink endpoint ~ (Maybe Etag -> URI))
             => Proxy api -> Proxy endpoint -> EmbeddedEntry mime -> URI
embeddedLink p1 p2 x = safeLink p1 p2 (eeEtag x)
