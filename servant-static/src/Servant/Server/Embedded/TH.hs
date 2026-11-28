-- | This module which contains the template haskell code which embeds the content.  Normally, you
-- do not need to use anything from this module.  You only need this module if you are creating your
-- own custom content generators, or you want more control over specifically how the resulting
-- 'EmbeddedEntry' structs are created and stored.
module Servant.Server.Embedded.TH (
    EntryVarName
  , EmbeddableEntry(..)
  , Generator
  , base64md5
  , etagAsHash
  , embedDevel
  , embedProduction
) where

import Blaze.ByteString.Builder.ByteString (insertByteString)
import Codec.Compression.GZip (compress)
import Crypto.Hash (MD5, Digest, hashlazy)
import Data.ByteString.Unsafe (unsafePackAddressLen)
import Data.Byteable (toBytes)
import Data.Monoid ((<>))
import GHC.Exts (Int(..))
import Language.Haskell.TH
import Language.Haskell.TH.Lib (TExpQ)
import Language.Haskell.TH.Syntax (TExp(..), lift)
import Network.HTTP.Types (status200, status304)
import Network.Mime (MimeType)
import Network.Wai
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as Base64

import Servant.Server.Embedded.Types

-- | For each entry, the template haskell code will produce a variable of type @'EmbeddedEntry'
-- mime@.  The variable name is specified by a value of type 'EntryVarName', so the string must
-- be a valid haskell identifier (start with a lower case letter, no spaces, etc.).
type EntryVarName = String

-- | A structure which only exists at compile time and specifies how some content should
-- be served.  The same content is described in two possible ways, either an IO action
-- which is executed at compile time to embed the content in the executable, or as an
-- action which will be executed on every request.
data EmbeddableEntry = EmbeddableEntry {
    ebeName :: EntryVarName
  , ebeMimeType :: MimeType
  , ebeProduction :: IO (Etag, BL.ByteString)
      -- ^ An action executed at compile time to load the content to embed.
  , ebeDevelReload :: TExpQ (IO BL.ByteString)
      -- ^ A template haskell expression that produces an @IO BL.ByteString@.  This
      -- @IO BL.ByteString@ will be executed on every request to load the content.
      -- The etag will be the hash of the content.
}

-- | A generator is an action which describes how to embed some content.
type Generator = Q EmbeddableEntry

-- | Hash and base64 encode a bytestring.
base64md5 :: BL.ByteString -> B.ByteString
base64md5 lbs = Base64.encode $ toBytes d
    where
        d :: Digest MD5
        d = hashlazy lbs

-- | Helper function to create the Etag by calling 'base64md5' on the content.
etagAsHash :: BL.ByteString -> (Etag, BL.ByteString)
etagAsHash b = (Etag $ base64md5 b, b)

-----------------------------------------------------------------------

-- | This is executed at runtime to recreate and serve the content on each request.
serveDevel :: MimeType -> IO BL.ByteString -> Application
serveDevel mime buildCt request sendResp = do
    ct <- buildCt
    let etag = "\"" <> base64md5 ct <> "\""
        h = [ ("Content-Type", mime)
            , ("ETag", etag)
            ]
    case lookup "if-none-match" $ requestHeaders request of
        Just m | m == etag -> sendResp $ responseLBS status304 [] ""
        _ -> sendResp $ responseLBS status200 h ct

-- | Embed the 'EmbeddableEntry' into the executable using 'ebeDevelReload', so that the
-- content will be recomputed on each request.  The action 'ebeProduction' is ignored.
embedDevel ::EmbeddableEntry -> TExpQ (EmbeddedEntry mime)
embedDevel e = addTypeDecl (ebeMimeType e)
    [| EmbeddedEntry
        { eeEtag = Nothing
        , eeApp = serveDevel $(bytestringE $ ebeMimeType e) $(unType <$> ebeDevelReload e)
        }
    |]


-----------------------------------------------------------------------------------

-- | This is executed at runtime to serve the previously embedded content.
serveProd :: MimeType -> Bool -> Etag -> B.ByteString -> Application
serveProd mime isCompressed (Etag etag) ct request sendResp = do
    let etag' = "\"" <> etag <> "\""
        h = [ ("Content-Type", mime)
            , ("ETag", etag')
            ] ++
            [ ("Content-Encoding", "gzip") | isCompressed ]
        cacheControl = [ ("Expires", "Thu, 31 Dec 2037 23:55:55 GMT")
                       , ("Cache-Control", "public, max-age=31536000") -- one year = 60*60*24*365
                       ]


    let ifMatch = lookup "if-none-match" $ requestHeaders request
        etagQuery = lookup "etag" $ queryString request
    sendResp $ case (ifMatch, etagQuery) of

        -- if both header and query, check both match
        (Just m, Just (Just t)) | m == etag' && t == etag -> responseLBS status304 [] ""

        -- if just the header, check that it matches
        (Just m, Nothing) | m == etag' -> responseLBS status304 [] ""

        -- if the etag argument was given correctly, respond with the content and
        -- specify that it should be cached forever.
        (_, Just (Just t)) | t == etag ->
            responseBuilder status200 (h++cacheControl) $ insertByteString $ ct

        -- if no etag argument was given or it was incorrect, then we cannot specify
        -- the cache control headers
        _ -> responseBuilder status200 h $ insertByteString $ ct

-- | Embed the 'EmbeddableEntry' into the executable by calling 'ebeProduction'.  The resulting
-- content will be compressed using gzip (depends on mime-type) and embedded into the executable.
-- At runtime, the content will be served.  In addition, if the incomming request URL contains an
-- @?etag=..@ query parameter, a @Cache-Control@ header will be returned to the client to indicate
-- that the content should be cached by the client forever.  Links with this etag are created by
-- the 'HasLink' instance, so if you always use links from 'safeLink' to refer to this
-- resource, the client will only request the resource when the etag changes.
embedProduction :: EmbeddableEntry -> TExpQ (EmbeddedEntry mime)
embedProduction e = do
    runIO $ putStrLn $ "Embedding resouce for " ++ ebeName e
    (Etag etag, ct) <- runIO $ ebeProduction e
    let (isCompressed, embeddedCt) = tryCompress (ebeMimeType e) ct
    etagVar <- newName "etag"
    addTypeDecl (ebeMimeType e)
        $ letE [ valD (varP etagVar) (normalB $ bytestringE etag) [] ]
               [| EmbeddedEntry
                    { eeEtag = Just (Etag $(varE etagVar))
                    , eeApp = serveProd $(bytestringE $ ebeMimeType e)
                                        $(lift isCompressed)
                                        (Etag $(varE etagVar))
                                        $(bytestringLazyE embeddedCt)
                                       
                    }
               |]


-- | Turn an expression producing an 'EmbeddedEntry' into one which has a type
-- declaration for 'EmbeddedEntry' with the given mime type.
addTypeDecl :: MimeType -> ExpQ -> TExpQ (EmbeddedEntry mime)
addTypeDecl mime entryExp = do
    eVar <- newName "entry"
    let mimeStr = T.unpack $ T.decodeUtf8 mime
    TExp <$> letE
          -- let eVar :: EmbeddedEntry mime = entryExp
        [ valD (sigP (varP eVar) (conT ''EmbeddedEntry `appT` litT (strTyLit mimeStr)))
               (normalB entryExp)
               []
        ]
        -- in eVar
        (varE eVar)

-----------------------------------------------------------------------------------
-- The following code was copied from wai-app-static (with a few small changes)
-----------------------------------------------------------------------------------

-- The use of unsafePackAddressLen is safe here because the length
-- is correct and we will only be reading from the bytestring, never
-- modifying it.
--
-- The only IO within unsafePackAddressLen is within newForeignPtr_ where
-- a new IORef is created as newIORef (NoFinalizers, []) to hold the finalizer
-- for the pointer.  Since the pointer for the content will never have a finalizer
-- added, we do not care if this finalizer IORef gets created more than once since
-- the IORef will always be holding (NoFinalizers, []).  Therefore
-- unsafeDupablePerformIO is safe.
bytestringE :: B.ByteString -> ExpQ
bytestringE b = [| unsafeDupablePerformIO (unsafePackAddressLen (I# $lenE) $ctE) |]
    where
        lenE = litE $ intPrimL $ toInteger $ B.length b
        ctE = litE $ stringPrimL $ B.unpack b

bytestringLazyE :: BL.ByteString -> ExpQ
bytestringLazyE b = [| unsafeDupablePerformIO (unsafePackAddressLen (I# $lenE) $ctE) |]
    where
        lenE = litE $ intPrimL $ toInteger $ BL.length b
        ctE = litE $ stringPrimL $ BL.unpack b

shouldCompress :: MimeType -> Bool
shouldCompress m = "text/" `B.isPrefixOf` m || m `elem` extra
    where
        extra = [ "application/json"
                , "application/javascript"
                , "application/ecmascript"
                ]

-- | Only compress if the mime type is correct and the compressed text is actually shorter.
tryCompress :: MimeType -> BL.ByteString -> (Bool, BL.ByteString)
tryCompress mime ct
        | shouldCompress mime = (c, ct')
        | otherwise = (False, ct)
    where
        compressed = compress ct
        c = BL.length compressed < BL.length ct
        ct' = if c then compressed else ct
