{-#LANGUAGE OverloadedStrings #-}
module Servant.JS.Vanilla where

import           Control.Lens
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Servant.Foreign
import           Servant.JS.Internal

-- | Generate vanilla javascript functions to make AJAX requests
--   to your API, using /XMLHttpRequest/. Uses 'defCommonGeneratorOptions'
--   for the 'CommonGeneratorOptions'.
vanillaJS :: JavaScriptGenerator
vanillaJS = mconcat . map generateVanillaJS

-- | Generate vanilla javascript functions to make AJAX requests
--   to your API, using /XMLHttpRequest/. Lets you specify your
--   own options.
vanillaJSWith :: CommonGeneratorOptions -> JavaScriptGenerator
vanillaJSWith opts = mconcat . map (generateVanillaJSWith opts)

-- | js codegen using XmlHttpRequest using default generation options
generateVanillaJS :: AjaxReq -> Text
generateVanillaJS = generateVanillaJSWith defCommonGeneratorOptions

-- | js codegen using XmlHttpRequest
generateVanillaJSWith :: CommonGeneratorOptions -> AjaxReq -> Text
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

  where argsStr = T.intercalate ", " args
        args = captures
            ++ map (view $ argName._1) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . fst . headerArg) hs
            ++ [onSuccess, onError]

        captures = map (fst . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if isJust(req ^. reqBody)
                 then [requestBody opts]
                 else []

        onSuccess = successCallback opts
        onError = errorCallback opts

        dataBody =
          if isJust (req ^. reqBody)
            then "JSON.stringify(body)\n"
            else "null"


        reqheaders =
          if null hs
            then ""
            else headersStr <> "\n"

          where headersStr = T.intercalate "\n" $ map headerStr hs
                headerStr header = "  xhr.setRequestHeader(\"" <>
                  fst (headerArg header) <>
                  "\", " <> toJSHeader header <> ");"

        namespace = if moduleName opts == ""
                       then "var "
                       else (moduleName opts) <> "."
        fname = namespace <> (functionNameBuilder opts $ req ^. funcName)

        method = req ^. reqMethod
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
