module Servant.JS.Vanilla where

import           Control.Lens
import           Data.List
import           Data.Monoid
import           Servant.JS.Internal

-- | Generate vanilla javascript functions to make AJAX requests
--   to your API, using /XMLHttpRequest/. Uses 'defCommonGeneratorOptions'
--   for the 'CommonGeneratorOptions'.
vanillaJS :: JavaScriptGenerator
vanillaJS = concatMap generateVanillaJS

-- | Generate vanilla javascript functions to make AJAX requests
--   to your API, using /XMLHttpRequest/. Lets you specify your
--   own options.
vanillaJSWith :: CommonGeneratorOptions -> JavaScriptGenerator
vanillaJSWith opts = concatMap (generateVanillaJSWith opts)

-- | js codegen using XmlHttpRequest using default generation options
generateVanillaJS :: AjaxReq -> String
generateVanillaJS = generateVanillaJSWith defCommonGeneratorOptions

-- | js codegen using XmlHttpRequest
generateVanillaJSWith :: CommonGeneratorOptions -> AjaxReq -> String
generateVanillaJSWith opts req = "\n" <>
    fname <> " = function(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  var xhr = new XMLHttpRequest();\n"
 <> "  xhr.open('" <> method <> "', " <> url <> ", true);\n"
 <>    reqheaders
 <> "  xhr.onreadystatechange = function (e) {\n"
 <> "    if (xhr.readyState == 4) {\n"
 <> "        var value = JSON.parse(xhr.responseText);\n"
 <> "      if (xhr.status == 200 || xhr.status == 201) {\n"
 <> "        onSuccess(value);\n"
 <> "      } else {\n"
 <> "        onError(value);\n"
 <> "      }\n"
 <> "    }\n"
 <> "  }\n"
 <> "  xhr.send(" <> dataBody <> ");\n"
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
            then "JSON.stringify(body)\n"
            else "null"


        reqheaders =
          if null hs
            then ""
            else headersStr ++ "\n"

          where headersStr = intercalate "\n" $ map headerStr hs
                headerStr header = "  xhr.setRequestHeader(\"" ++
                  headerArgName header ++
                  "\", " ++ show header ++ ");"

        namespace = if null (moduleName opts)
                       then "var "
                       else (moduleName opts) <> "."
        fname = namespace <> (functionNameBuilder opts $ req ^. funcName)

        method = req ^. reqMethod
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
