{-#LANGUAGE OverloadedStrings #-}
module Servant.JS.Angular where

import           Control.Lens
import           Data.Maybe (isJust)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Servant.Foreign
import           Servant.JS.Internal

-- | Options specific to the angular code generator
data AngularOptions = AngularOptions
  { serviceName :: Text                         -- ^ When generating code with wrapInService,
                                                  --   name of the service to generate
  , prologue    :: Text -> Text -> Text        -- ^ beginning of the service definition
  , epilogue    :: Text                            -- ^ end of the service definition
  }

-- | Default options for the Angular codegen. Used by 'wrapInService'.
defAngularOptions :: AngularOptions
defAngularOptions = AngularOptions
  { serviceName = ""
  , prologue = \svc m -> m <> "service('" <> svc <> "', function($http) {\n"
                           <> "  return ({"
  , epilogue = "});\n});\n"
    }

-- | Instead of simply generating top level functions, generates a service instance
-- on which your controllers can depend to access your API.
-- This variant uses default 'AngularOptions'.
angularService :: AngularOptions -> JavaScriptGenerator
angularService ngOpts = angularServiceWith ngOpts defCommonGeneratorOptions

-- | Instead of simply generating top level functions, generates a service instance
-- on which your controllers can depend to access your API
angularServiceWith :: AngularOptions -> CommonGeneratorOptions -> JavaScriptGenerator
angularServiceWith ngOpts opts reqs =
    prologue ngOpts svc mName
    <> T.intercalate "," (map generator reqs) <>
    epilogue ngOpts
    where
        generator req = generateAngularJSWith ngOpts opts req
        svc = serviceName ngOpts
        mName = if moduleName opts == ""
                   then "app."
                   else moduleName opts <> "."

-- | Generate regular javacript functions that use
--   the $http service, using default values for 'CommonGeneratorOptions'.
angular :: AngularOptions -> JavaScriptGenerator
angular ngopts = angularWith ngopts defCommonGeneratorOptions

-- | Generate regular javascript functions that use the $http service.
angularWith :: AngularOptions -> CommonGeneratorOptions -> JavaScriptGenerator
angularWith ngopts opts = T.intercalate "\n\n" . map (generateAngularJSWith ngopts opts)

-- | js codegen using $http service from Angular using default options
generateAngularJS :: AngularOptions -> AjaxReq -> Text
generateAngularJS ngOpts = generateAngularJSWith ngOpts defCommonGeneratorOptions

-- | js codegen using $http service from Angular
generateAngularJSWith ::  AngularOptions -> CommonGeneratorOptions -> AjaxReq -> Text
generateAngularJSWith ngOptions opts req = "\n" <>
    fname <> fsep <> " function(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  return $http(\n"
 <> "    { url: " <> url <> "\n"
 <> dataBody
 <> reqheaders
 <> "    , method: '" <> decodeUtf8 method <> "'\n"
 <> "    });\n"
 <> "}\n"

  where argsStr = T.intercalate ", " args
        args = http
            ++ captures
            ++ map (view $ queryArgName . argPath) queryparams
            ++ body
            ++ map ( toValidFunctionName
                   . (<>) "header"
                   . view (headerArg . argPath)
                   ) hs

        -- If we want to generate Top Level Function, they must depend on
        -- the $http service, if we generate a service, the functions will
        -- inherit this dependency from the service
        http = case T.length (serviceName ngOptions) of
                  0 -> ["$http"]
                  _ -> []

        captures = map (view argPath . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl . path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if isJust (req ^. reqBody)
                 then [requestBody opts]
                 else []

        dataBody =
          if isJust (req ^. reqBody)
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

        namespace =
            if hasService
               then ""
               else if hasNoModule
                    then "var "
                    else (moduleName opts) <> "."
            where
                hasNoModule = moduleName opts == ""

        hasService = serviceName ngOptions /= ""

        fsep = if hasService then ":" else " ="

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
