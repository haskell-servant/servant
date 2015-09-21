module Servant.JS.Internal
  ( JavaScriptGenerator
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
  , defCommonGeneratorOptions
  , reqHeaders
  , CommonGeneratorOptions(..)
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
