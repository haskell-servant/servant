module Servant.JQuery.JQuery where

import Servant.JQuery.Internal
import Control.Lens
import Data.List
import Data.Monoid

-- | js codegen using JQuery using default options
generateJQueryJS :: AjaxReq -> String
generateJQueryJS = generateJQueryJSWith defCommonGeneratorOptions

-- | js codegen using JQuery
generateJQueryJSWith :: CommonGeneratorOptions -> AjaxReq -> String
generateJQueryJSWith opts req = "\n" <>
    fname <> " = function(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  $.ajax(\n"
 <> "    { url: " <> url <> "\n"
 <> "    , success: " <> onSuccess <> "\n"
 <> dataBody
 <> reqheaders
 <> "    , error: " <> onError <> "\n"
 <> "    , type: '" <> method <> "'\n"
 <> "    });\n"
 <> "}\n"

  where argsStr = intercalate ", " args
        args = captures
            ++ map (view argName) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . headerArgName) hs
            ++ [onSuccess, onError]

        captures = map captureArg
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if req ^. reqBody
                 then [requestBody opts]
                 else []
        
        onSuccess = successCallback opts
        onError = errorCallback opts

        dataBody =
          if req ^. reqBody
            then "    , data: JSON.stringify(body)\n" <>
                 "    , contentType: 'application/json'\n"
            else ""

        reqheaders =
          if null hs
            then ""
            else "    , headers: { " ++ headersStr ++ " }\n"

          where headersStr = intercalate ", " $ map headerStr hs
                headerStr header = "\"" ++
                  headerArgName header ++
                  "\": " ++ show header

        namespace = if null (moduleName opts)
                       then "var "
                       else (moduleName opts) <> "."
        fname = namespace <> (functionRenamer opts $ req ^. funcName)
        
        method = req ^. reqMethod
        url = if url' == "'" then "'/'" else url'
        url' = "'"
           ++ urlArgs
           ++ queryArgs

        urlArgs = jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" ++ jsParams queryparams
