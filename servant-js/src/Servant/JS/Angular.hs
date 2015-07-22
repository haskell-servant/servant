module Servant.JS.Angular where

import Servant.JS.Internal
import Control.Lens
import Data.List
import Data.Monoid

data AngularOptions = AngularOptions
  { serviceName :: String                         -- ^ When generating code with wrapInService,
                                                  --   name of the service to generate
  , prologue :: String -> String -> String        -- ^ beginning of the service definition
  , epilogue :: String                            -- ^ end of the service definition
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
wrapInService :: AngularOptions -> [AjaxReq] -> String
wrapInService ngOpts reqs = wrapInServiceWith ngOpts defCommonGeneratorOptions reqs

-- | Instead of simply generating top level functions, generates a service instance
-- on which your controllers can depend to access your API
wrapInServiceWith :: AngularOptions -> CommonGeneratorOptions -> [AjaxReq] -> String
wrapInServiceWith ngOpts opts reqs = 
    ((prologue ngOpts) svc mName)
    <> (intercalate "," $ map generator reqs) <>
    (epilogue ngOpts)
    where
        generator req = (generateAngularJSWith ngOpts opts req)
        svc = serviceName ngOpts
        mName = if null (moduleName opts)
                   then "app."
                   else (moduleName opts) <> "."
    
-- js codegen using $http service from Angular using default options
generateAngularJS :: AngularOptions -> AjaxReq -> String
generateAngularJS ngOpts = generateAngularJSWith ngOpts defCommonGeneratorOptions
    
-- js codegen using $http service from Angular
generateAngularJSWith ::  AngularOptions -> CommonGeneratorOptions -> AjaxReq -> String
generateAngularJSWith ngOptions opts req = "\n" <>
    fname <> fsep <> " function(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  return $http(\n"
 <> "    { url: " <> url <> "\n"
 <> dataBody
 <> reqheaders
 <> "    , method: '" <> method <> "'\n"
 <> "    });\n"
 <> "}\n"

  where argsStr = intercalate ", " args
        args = http
            ++ captures
            ++ map (view argName) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . headerArgName) hs
        
        -- If we want to generate Top Level Function, they must depend on
        -- the $http service, if we generate a service, the functions will
        -- inherit this dependency from the service
        http = case length (serviceName ngOptions) of
                  0 -> ["$http"]
                  _ -> []

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

        namespace =
            if hasService
               then ""
               else if hasNoModule
                    then "var "
                    else (moduleName opts) <> "."
            where
                hasNoModule = null (moduleName opts)
        
        hasService = not $ null (serviceName ngOptions)
                
        fsep = if hasService then ":" else " ="
                
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
