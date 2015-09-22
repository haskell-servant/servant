module Servant.JS.Internal
  ( JavaScriptGenerator
  , CommonGeneratorOptions(..)
  , defCommonGeneratorOptions
  , AjaxReq
  , jsSegments
  , segmentToStr
  , segmentTypeToStr
  , jsParams
  , jsGParams
  , jsMParams
  , paramToStr
  -- re-exports
  , (:<|>)(..)
  , (:>)
  , defReq
  , reqHeaders
  , HasForeign(..)
  , HeaderArg(..)
  , concatCase
  , snakeCase
  , camelCase
  , ReqBody
  , JSON
  , FormUrlEncoded
  , Post
  , Get
  , Raw
  , Header
  ) where 

import Control.Lens hiding (List)
import Servant.Foreign

type AjaxReq = Req

-- A 'JavascriptGenerator' just takes the data found in the API type
-- for each endpoint and generates Javascript code in a String. Several
-- generators are available in this package.
type JavaScriptGenerator = [Req] -> String

-- | This structure is used by specific implementations to let you
-- customize the output
data CommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder :: FunctionName -> String  -- ^ function generating function names
  , requestBody :: String                -- ^ name used when a user want to send the request body (to let you redefine it)
  , successCallback :: String            -- ^ name of the callback parameter when the request was successful
  , errorCallback :: String              -- ^ name of the callback parameter when the request reported an error
  , moduleName :: String                 -- ^ namespace on which we define the foreign function (empty mean local var)
  , urlPrefix :: String                  -- ^ a prefix we should add to the Url in the codegen
  }

-- | Default options.
--
-- @
-- > defCommonGeneratorOptions = CommonGeneratorOptions
-- >   { functionNameBuilder = camelCase
-- >   , requestBody = "body"
-- >   , successCallback = "onSuccess"
-- >   , errorCallback = "onError"
-- >   , moduleName = ""
-- >   , urlPrefix = ""
-- >   }
-- @
defCommonGeneratorOptions :: CommonGeneratorOptions
defCommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder = camelCase
  , requestBody = "body"
  , successCallback = "onSuccess"
  , errorCallback = "onError"
  , moduleName = ""
  , urlPrefix = ""
  }

jsSegments :: [Segment] -> String
jsSegments []  = ""
jsSegments [x] = "/" ++ segmentToStr x False
jsSegments (x:xs) = "/" ++ segmentToStr x True ++ jsSegments xs

segmentToStr :: Segment -> Bool -> String
segmentToStr (Segment st ms) notTheEnd =
  segmentTypeToStr st ++ jsMParams ms ++ if notTheEnd then "" else "'"

segmentTypeToStr :: SegmentType -> String
segmentTypeToStr (Static s) = s
segmentTypeToStr (Cap s)    = "' + encodeURIComponent(" ++ s ++ ") + '"

jsGParams :: String -> [QueryArg] -> String
jsGParams _ []     = ""
jsGParams _ [x]    = paramToStr x False
jsGParams s (x:xs) = paramToStr x True ++ s ++ jsGParams s xs

jsParams :: [QueryArg] -> String
jsParams = jsGParams "&"

jsMParams :: [MatrixArg] -> String
jsMParams [] = ""
jsMParams xs = ";" ++ jsGParams ";" xs

paramToStr :: QueryArg -> Bool -> String
paramToStr qarg notTheEnd =
  case qarg ^. argType of
    Normal -> name
           ++ "=' + encodeURIComponent("
           ++ name
           ++ if notTheEnd then ") + '" else ")"
    Flag   -> name ++ "="
    List   -> name
           ++ "[]=' + encodeURIComponent("
           ++ name
           ++ if notTheEnd then ") + '" else ")"
  where name = qarg ^. argName
