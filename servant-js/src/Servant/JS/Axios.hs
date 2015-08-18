module Servant.JS.Axios where

import           Control.Lens
import           Data.Char           (toLower)
import           Data.List
import           Data.Monoid
import           Servant.JS.Internal

-- | Axios 'configuration' type
-- Let you customize the generation using Axios capabilities
data AxiosOptions = AxiosOptions
  { -- | indicates whether or not cross-site Access-Control requests
    -- should be made using credentials
    withCredentials :: !Bool
    -- | the name of the cookie to use as a value for xsrf token
  , xsrfCookieName  :: !(Maybe String)
    -- | the name of the header to use as a value for xsrf token
  , xsrfHeaderName  :: !(Maybe String)
  }

-- | Default instance of the AxiosOptions
-- Defines the settings as they are in the Axios documentation
-- by default
defAxiosOptions :: AxiosOptions
defAxiosOptions = AxiosOptions
  { withCredentials = False
  , xsrfCookieName = Nothing
  , xsrfHeaderName = Nothing
  }

-- | Generate regular javacript functions that use
--   the axios library, using default values for 'CommonGeneratorOptions'.
axios :: AxiosOptions -> JavaScriptGenerator
axios aopts = axiosWith aopts defCommonGeneratorOptions

-- | Generate regular javascript functions that use the axios library.
axiosWith :: AxiosOptions -> CommonGeneratorOptions -> JavaScriptGenerator
axiosWith aopts opts = intercalate "\n\n" . map (generateAxiosJSWith aopts opts)

-- | js codegen using axios library using default options
generateAxiosJS :: AxiosOptions -> AjaxReq -> String
generateAxiosJS aopts = generateAxiosJSWith aopts defCommonGeneratorOptions

-- | js codegen using axios library
generateAxiosJSWith :: AxiosOptions -> CommonGeneratorOptions -> AjaxReq -> String
generateAxiosJSWith aopts opts req = "\n" <>
    fname <> " = function(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  return axios({ url: " <> url <> "\n"
 <> "    , method: '" <> method <> "'\n"
 <> dataBody
 <> reqheaders
 <> withCreds
 <> xsrfCookie
 <> xsrfHeader
 <> "    });\n"
 <> "}\n"

  where argsStr = intercalate ", " args
        args = captures
            ++ map (view argName) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . headerArgName) hs

        captures = map captureArg
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if req ^. reqBody
                 then [requestBody opts]
                 else []

        dataBody =
          if req ^. reqBody
            then "    , data: body\n" <>
                 "    , responseType: 'json'\n"
            else ""

        withCreds =
          if withCredentials aopts
            then "    , withCredentials: true\n"
            else ""

        xsrfCookie =
          case xsrfCookieName aopts of
            Just name -> "    , xsrfCookieName: '" <> name <> "'\n"
            Nothing   -> ""

        xsrfHeader =
          case xsrfHeaderName aopts of
            Just name -> "    , xsrfHeaderName: '" <> name <> "'\n"
            Nothing   -> ""

        reqheaders =
          if null hs
            then ""
            else "    , headers: { " <> headersStr <> " }\n"

          where headersStr = intercalate ", " $ map headerStr hs
                headerStr header = "\"" ++
                  headerArgName header ++
                  "\": " ++ show header

        namespace =
               if hasNoModule
                  then "var "
                  else (moduleName opts) <> "."
               where
                  hasNoModule = null (moduleName opts)

        fname = namespace <> (functionNameBuilder opts $ req ^. funcName)

        method = map toLower $ req ^. reqMethod
        url = if url' == "'" then "'/'" else url'
        url' = "'"
           ++ urlPrefix opts
           ++ urlArgs
           ++ queryArgs

        urlArgs = jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" ++ jsParams queryparams
