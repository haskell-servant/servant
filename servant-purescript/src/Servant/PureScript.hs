{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Servant.PureScript (
  generatePSModule,
  generatePSUtilModule,
  generatePS,
  PSSettings(..),
  baseURL,
  defaultSettings
) where

import           Control.Arrow ((&&&))
import           Control.Lens (makeLenses, (^.), (^..), view)
import           Data.Char (toUpper, toLower)
import           Data.Proxy (Proxy(Proxy))
import           Data.String.Conversions (ConvertibleStrings, ST, cs, (<>))
import qualified Data.Text as T
import qualified Servant.JS as JS
import qualified Servant.JS.Internal as JS
import qualified Servant.Foreign as F


-- | PureScript rendering settings
data PSSettings = PSSettings {
    _baseURL :: String, -- ^ Base URL for AJAX requests
    _utilModuleName :: ST  -- ^ module that all generated ajax modules depend on
}

makeLenses ''PSSettings

-- | (may be obsoleted by https://github.com/purescript/purescript-globals/pull/7 at some point.)
generatePSUtilModule :: PSSettings -> (ST, ST)
generatePSUtilModule settings = (purs, js)
  where
    purs = T.unlines
        [ "module " <> (settings ^. utilModuleName) <> " where"
        , "foreign import encodeURIComponent :: String -> String"
        ]
    js = T.unlines
        [ "\"use strict\";"
        , "// module " <> (settings ^. utilModuleName)
        , "exports.encodeURIComponent = encodeURIComponent;"
        ]

-- | Given a servant api, generate a PureScript module containing a list of functions for AJAX
-- requests.
generatePSModule :: (JS.GenerateList (F.Foreign api), F.HasForeign api) => PSSettings -> ST -> Proxy api -> ST
generatePSModule settings mname proxy =
    generatePSModule' settings mname $ JS.generateList (F.foreignFor proxy F.defReq)

-- | Given a list of foreign requests, generate a PureScript module containing a list of functions
-- for AJAX requests.
generatePSModule'
    :: PSSettings -- ^ PureScript rendering settings
    -> ST -- ^ Name of PureScript module
    -> [F.Req] -- ^ List of AJAX requests to render in module
    -> ST -- ^ Rendered PureScript module
generatePSModule' settings mname reqs = T.unlines $
        [ "module " <> mname <> " where"
        , ""
        , "import Prelude"
        , "import Data.Foreign"
        , "import Data.Maybe"
        , "import Network.HTTP.Affjax"
        , "import Network.HTTP.Method"
        , "import Network.HTTP.RequestHeader"
        , "import " <> (settings ^. utilModuleName) <> " (encodeURIComponent)"
        , ""
        , T.intercalate "\n" (generatePS settings <$> reqs)
        ]

-- | Generate a single PureScript function for an AJAX request.
-- To prevent conflicts, generates a unique function name for every available
-- function name and set of captures.
generatePS
    :: PSSettings -- ^ PureScript rendering settings
    -> F.Req -- ^ AJAX request to render
    -> ST -- ^ Rendered PureScript
generatePS settings req = ajaxRequest
  where
    args :: [ST]
    args = captures <> queryArgs <> body <> fmap (fst . snd) headerArgs

    captures :: [ST]
    captures = fmap (F.captureArg) . filter F.isCapture $ req ^. F.reqUrl . F.path

    queryArgs :: [ST]
    queryArgs  = fmap ((<>) "query" . view F.argName) queryParams

    headerArgs :: [(T.Text, (T.Text, F.HeaderArg))]
    headerArgs = fmap ( decapitalise . F.headerArgName &&&
                        JS.toValidFunctionName . (<>) "header" . F.headerArgName &&&
                        id )
                      (req ^. F.reqHeaders)

    fname :: ST
    fname = F.camelCase (req ^. F.funcName)
         <> if null captures then "" else "With"
         <> T.intercalate "And" (fmap capitalise captures)

    queryParams :: [F.QueryArg]
    queryParams = req ^.. F.reqUrl . F.queryStr . traverse

    body :: [ST]
    body = ["body" | req ^. F.reqBody]

    wrapHeaders :: [(T.Text, T.Text)] -> [(T.Text, (T.Text, F.HeaderArg))] -> ST
    wrapHeaders ihs hs =
        "[" <>
        (T.intercalate ", " (concat $
            [wrapImplicitHeader <$> ihs | not $ null ihs] ++
            [wrapHeader <$> hs | not $ null hs])) <>
        "]"

    wrapHeader :: (T.Text, (T.Text, F.HeaderArg)) -> ST
    wrapHeader (h, (_, o)) = "RequestHeader " <> cs (show h) <> " " <> psHeaderArg o

    wrapImplicitHeader :: (T.Text, T.Text) -> ST
    wrapImplicitHeader (k, v) = "RequestHeader " <> cs (show k) <> " " <> cs (show v)

    implicitHeaderArgs :: [(T.Text, T.Text)]
    implicitHeaderArgs =
        [ ("content-type", "application/json")
        , ("accept", "application/json")
        ]

    ajaxRequest :: ST
    ajaxRequest = T.unlines $
        typeSig :
        (fname <> argString <> " = affjax $ defaultRequest") :
        ("    { method = " <> req ^. F.reqMethod) :
        ("    , url = " <> urlString) :
        ("    , headers = " <> wrapHeaders implicitHeaderArgs headerArgs) :
        ["    , content = Just body" | req ^. F.reqBody] ++
        "    }" :
        []
      where
        typeSig :: ST
        typeSig = T.concat
            [ fname
            , " :: forall eff. "
            , T.intercalate " -> " typedArgs
            , if null args then "" else " -> "
            , "Affjax eff Foreign"
            ]

        typedArgs :: [ST]
        typedArgs = concat $
            [ fmap (const "String") captures
            , fmap (const "Maybe String") queryArgs ]
            <> [ bodyArgType ]
            <> [ fmap (const "String") headerArgs ]

        argString :: ST
        argString = case T.unwords args of
            "" -> ""
            s -> " " <> s

        urlString :: ST
        urlString = T.concat
            [ "\""
            , cs $ settings ^. baseURL
            , "/"
            , psPathSegments $ req ^.. F.reqUrl . F.path . traverse
            , if null queryParams then "\"" else "?\" <> " <> psParams queryParams
            ]

        bodyArgType :: [ST]
        bodyArgType = [ "String" | req ^. F.reqBody ]

-- | Show HeaderArg instance from Servant.JS, changes to use PureScript
-- monoidal bindings
psHeaderArg :: F.HeaderArg -> ST
psHeaderArg (F.HeaderArg n) = JS.toValidFunctionName ("header" <> n)
psHeaderArg (F.ReplaceHeaderArg n p)
    | pn `T.isPrefixOf` p = pv <> " <> \"" <> rp <> "\""
    | pn `T.isSuffixOf` p = "\"" <> rp <> "\" <> " <> pv
    | pn `T.isInfixOf` p  = "\"" <> T.replace pn ("\" + " <> pv <> " + \"") p <> "\""
    | otherwise           = p
  where
    pv = JS.toValidFunctionName ("header" <> n)
    pn = "{" <> n <> "}"
    rp = T.replace pn "" p

-- | Default PureScript settings: specifies an empty base URL
defaultSettings :: PSSettings
defaultSettings = PSSettings "" "Util"

-- | Capitalise a string for use in PureScript variable name
capitalise :: (ConvertibleStrings s String, ConvertibleStrings String s) => s -> s
capitalise = cs . capitalise' . cs

capitalise' :: String -> String
capitalise' [] = []
capitalise' (x:xs) = [toUpper x] <> xs

-- | Decapitalise a string for use as a Purescript variable name
decapitalise :: (ConvertibleStrings s String, ConvertibleStrings String s) => s -> s
decapitalise = cs . decapitalise' . cs

decapitalise' :: String -> String
decapitalise' [] = []
decapitalise' (x:xs) = [toLower x] <> xs

-- | Turn a list of path segments into a URL string
psPathSegments :: [F.Segment] -> ST
psPathSegments = T.intercalate "/" . fmap psSegmentToStr

-- | Turn an individual path segment into a PureScript variable handler
psSegmentToStr :: F.Segment -> ST
psSegmentToStr (F.Segment (F.Static s)) = s
psSegmentToStr (F.Segment (F.Cap s))    = "\" <> encodeURIComponent " <> s <> " <> \""

-- | Turn a list of query string params into a URL string
psParams :: [F.QueryArg] -> ST
psParams qa = "(intercalate \"&\" <<< catMaybes $ [" <> T.intercalate ", " (psParamToStr <$> qa) <> "])"

-- | Turn an individual query string param into a PureScript variable handler
--
-- Must handle Maybe String as the input value
psParamToStr :: F.QueryArg -> ST
psParamToStr qarg = case qarg ^. F.argType of
    F.Normal -> "((\"" <> name <> "=\" <>) <<< encodeURIComponent) <$> " <> qname
    F.List   -> "((\"" <> name <> "[]=\" <>) <<< encodeURIComponent) <$> " <> qname
    F.Flag   -> "\"" <> name <> "=\""
  where
    name = qarg ^. F.argName
    qname = "query" <> name
