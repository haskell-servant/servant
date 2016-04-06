{-#LANGUAGE OverloadedStrings #-}
module Servant.JS.Axios where

import           Control.Lens
import           Data.Maybe (isJust)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import           Servant.Foreign
import           Servant.JS.Internal

-- | Axios 'configuration' type
-- Let you customize the generation using Axios capabilities
data AxiosOptions = AxiosOptions
  { -- | indicates whether or not cross-site Access-Control requests
    -- should be made using credentials
    withCredentials :: !Bool
    -- | the name of the cookie to use as a value for xsrf token
  , xsrfCookieName  :: !(Maybe Text)
    -- | the name of the header to use as a value for xsrf token
  , xsrfHeaderName  :: !(Maybe Text)
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
axiosWith aopts opts = T.intercalate "\n\n" . map (generateAxiosJSWith aopts opts)

-- | js codegen using axios library using default options
generateAxiosJS :: AxiosOptions -> AjaxReq -> Text
generateAxiosJS aopts = generateAxiosJSWith aopts defCommonGeneratorOptions

-- | js codegen using axios library
generateAxiosJSWith :: AxiosOptions -> CommonGeneratorOptions -> AjaxReq -> Text
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

  where argsStr = T.intercalate ", " args
        args = captures
            ++ map (view $ queryArgName . argPath) queryparams
            ++ body
            ++ map ( toValidFunctionName
                   . (<>) "header"
                   . view (headerArg . argPath)
                   ) hs

        captures = map (view argPath . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if isJust (req ^. reqBody)
                 then [requestBody opts]
                 else []

        dataBody =
          if isJust (req ^. reqBody)
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

          where
            headersStr = T.intercalate ", " $ map headerStr hs
            headerStr header = "\"" <>
              header ^. headerArg . argPath <>
              "\": " <> toJSHeader header

        namespace =
               if hasNoModule
                  then "var "
                  else (moduleName opts) <> "."
               where
                  hasNoModule = moduleName opts == ""

        fname = namespace <> (functionNameBuilder opts $ req ^. reqFuncName)

        method = T.toLower . decodeUtf8 $ req ^. reqMethod
        url = if url' == "'" then "'/'" else url'
        url' = "'"
           <> urlPrefix opts
           <> urlArgs
           <> queryArgs

        urlArgs = jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" <> jsParams queryparams
