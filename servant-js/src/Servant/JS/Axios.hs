module Servant.JS.Axios where

import Servant.JS.Internal
import Control.Lens
import Data.Char (toLower)
import Data.List
import Data.Monoid

-- | Generate regular javacript functions that use
--   the axios library, using default values for 'CommonGeneratorOptions'.
axios :: JavaScriptGenerator
axios = axiosWith defCommonGeneratorOptions

-- | Generate regular javascript functions that use the axios library.
axiosWith :: CommonGeneratorOptions -> JavaScriptGenerator
axiosWith opts = intercalate "\n\n" . map (generateAxiosJSWith opts)

-- | js codegen using axios library using default options
generateAxiosJS :: AjaxReq -> String
generateAxiosJS = generateAxiosJSWith defCommonGeneratorOptions
    
-- | js codegen using axios library
generateAxiosJSWith :: CommonGeneratorOptions -> AjaxReq -> String
generateAxiosJSWith opts req = "\n" <>
    fname <> " = function(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  return axios({ url: " <> url <> "\n"
 <> "    , method: '" <> method <> "'\n"
 <> dataBody
 <> reqheaders
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

        reqheaders =
          if null hs
            then ""
            else "    , headers: { " ++ headersStr ++ " }\n"

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
                
        fname = namespace <> (functionRenamer opts $ req ^. funcName)
        
        method = map toLower $ req ^. reqMethod
        url = if url' == "'" then "'/'" else url'
        url' = "'"
           -- ++ urlPrefix opts
           ++ urlArgs
           ++ queryArgs

        urlArgs = jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" ++ jsParams queryparams
