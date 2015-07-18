{-# LANGUAGE QuasiQuotes #-}
module Servant.JQuery.Vanilla where

import Servant.JQuery.Internal
import Control.Lens
import Data.List
import Data.Monoid
import Data.String.Interpolate

-- | js codegen using XmlHttpRequest using default generation options
generateVanillaJS :: AjaxReq -> String
generateVanillaJS = generateVanillaJSWith defCommonGeneratorOptions

-- | js codegen using XmlHttpRequest
generateVanillaJSWith :: CommonGeneratorOptions -> AjaxReq -> String
generateVanillaJSWith opts req = [i|
   #{fname} = function(#{argsStr})
   {
     var xhr = new XMLHttpRequest();
     xhr.open('#{method}', #{url}, true);
     #{reqheaders}
     xhr.onreadystatechange = function (e) {
       if (xhr.readyState == 4) {
         var value = JSON.parse(xhr.responseText);
           if (xhr.status == 200 || xhr.status == 201) {
             #{onSuccess}(value);
           } else {
             #{onError}(value);
           }
         }
       }
       xhr.send(#{dataBody});
   }
|]

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
