{-#LANGUAGE OverloadedStrings #-}
module Servant.JS.JQuery where

import           Control.Lens
import           Data.Maybe (isJust)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Servant.Foreign
import           Servant.JS.Internal


-- | Generate javascript functions that use the /jQuery/ library
--   to make the AJAX calls. Uses 'defCommonGeneratorOptions'
--   for the generator options.
jquery :: JavaScriptGenerator
jquery = mconcat . map generateJQueryJS

-- | Generate javascript functions that use the /jQuery/ library
--   to make the AJAX calls. Lets you specify your own 'CommonGeneratorOptions'.
jqueryWith :: CommonGeneratorOptions -> JavaScriptGenerator
jqueryWith opts = mconcat . map (generateJQueryJSWith opts)

-- | js codegen using JQuery using default options
generateJQueryJS :: AjaxReq -> Text
generateJQueryJS = generateJQueryJSWith defCommonGeneratorOptions

-- | js codegen using JQuery
generateJQueryJSWith :: CommonGeneratorOptions -> AjaxReq -> Text
generateJQueryJSWith opts req = "\n" <>
    fname <> " = function(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  $.ajax(\n"
 <> "    { url: " <> url <> "\n"
 <> "    , success: " <> onSuccess <> "\n"
 <> dataBody
 <> reqheaders
 <> "    , error: " <> onError <> "\n"
 <> "    , type: '" <> decodeUtf8 method <> "'\n"
 <> "    });\n"
 <> "}\n"

  where argsStr = T.intercalate ", " args
        args = captures
            ++ map (view $ queryArgName . argPath) queryparams
            ++ body
            ++ map (toValidFunctionName
                   . (<>) "header"
                   . view (headerArg . argPath)
                   ) hs
            ++ [onSuccess, onError]

        captures = map (view argPath . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if isJust (req ^. reqBody)
                 then [requestBody opts]
                 else []

        onSuccess = successCallback opts
        onError = errorCallback opts

        dataBody =
          if isJust $ req ^. reqBody
            then "    , data: JSON.stringify(body)\n" <>
                 "    , contentType: 'application/json'\n"
            else ""

        reqheaders =
          if null hs
            then ""
            else "    , headers: { " <> headersStr <> " }\n"

          where
            headersStr = T.intercalate ", " $ map headerStr hs
            headerStr header = "\"" <>
              header ^. headerArg . argPath <>
              "\": " <> toJSHeader header

        namespace = if (moduleName opts) == ""
                       then "var "
                       else (moduleName opts) <> "."
        fname = namespace <> (functionNameBuilder opts $ req ^. reqFuncName)

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
