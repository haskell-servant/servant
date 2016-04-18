{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
  , paramToStr
  , toValidFunctionName
  , toJSHeader
  -- re-exports
  , (:<|>)(..)
  , (:>)
  , defReq
  , reqHeaders
  , HasForeign(..)
  , HasForeignType(..)
  , GenerateList(..)
  , NoTypes
  , ArgType(..)
  , HeaderArg(..)
  , QueryArg(..)
  , Req(..)
  , Segment(..)
  , SegmentType(..)
  , Url(..)
  , Path
  , Arg(..)
  , FunctionName(..)
  , PathSegment(..)
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

import           Control.Lens ((^.))
import qualified Data.CharSet as Set
import qualified Data.CharSet.Unicode.Category as Set
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           Servant.Foreign

type AjaxReq = Req ()

-- A 'JavascriptGenerator' just takes the data found in the API type
-- for each endpoint and generates Javascript code in a Text. Several
-- generators are available in this package.
type JavaScriptGenerator = [Req ()] -> Text

-- | This structure is used by specific implementations to let you
-- customize the output
data CommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder :: FunctionName -> Text
    -- ^ function generating function names
  , requestBody :: Text
    -- ^ name used when a user want to send the request body
    -- (to let you redefine it)
  , successCallback :: Text
    -- ^ name of the callback parameter when the request was successful
  , errorCallback :: Text
    -- ^ name of the callback parameter when the request reported an error
  , moduleName :: Text
    -- ^ namespace on which we define the foreign function (empty mean local var)
  , urlPrefix :: Text
    -- ^ a prefix we should add to the Url in the codegen
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

-- | Attempts to reduce the function name provided to that allowed by @'Foreign'@.
--
-- https://mathiasbynens.be/notes/javascript-identifiers
-- Couldn't work out how to handle zero-width characters.
--
-- @TODO: specify better default function name, or throw error?
toValidFunctionName :: Text -> Text
toValidFunctionName t =
  case T.uncons t of
    Just (x,xs) ->
      setFirstChar x `T.cons` T.filter remainder xs
    Nothing -> "_"
  where
    setFirstChar c = if firstChar c then c else '_'
    firstChar c = prefixOK c || Set.member c firstLetterOK
    remainder c = prefixOK c || Set.member c remainderOK
    prefixOK c = c `elem` ['$','_']
    firstLetterOK = mconcat
                      [ Set.lowercaseLetter
                      , Set.uppercaseLetter
                      , Set.titlecaseLetter
                      , Set.modifierLetter
                      , Set.otherLetter
                      , Set.letterNumber
                      ]
    remainderOK   = firstLetterOK
               <> mconcat
                    [ Set.nonSpacingMark
                    , Set.spacingCombiningMark
                    , Set.decimalNumber
                    , Set.connectorPunctuation
                    ]

toJSHeader :: HeaderArg f -> Text
toJSHeader (HeaderArg n)
  = toValidFunctionName ("header" <> n ^. argName . _PathSegment)
toJSHeader (ReplaceHeaderArg n p)
  | pn `T.isPrefixOf` p = pv <> " + \"" <> rp <> "\""
  | pn `T.isSuffixOf` p = "\"" <> rp <> "\" + " <> pv
  | pn `T.isInfixOf` p  = "\"" <> (T.replace pn ("\" + " <> pv <> " + \"") p)
                             <> "\""
  | otherwise         = p
  where
    pv = toValidFunctionName ("header" <> n ^. argName . _PathSegment)
    pn = "{" <> n ^. argName . _PathSegment <> "}"
    rp = T.replace pn "" p

jsSegments :: [Segment f] -> Text
jsSegments []  = ""
jsSegments [x] = "/" <> segmentToStr x False
jsSegments (x:xs) = "/" <> segmentToStr x True <> jsSegments xs

segmentToStr :: Segment f -> Bool -> Text
segmentToStr (Segment st) notTheEnd =
  segmentTypeToStr st <> if notTheEnd then "" else "'"

segmentTypeToStr :: SegmentType f -> Text
segmentTypeToStr (Static s) = s ^. _PathSegment
segmentTypeToStr (Cap s)    =
  "' + encodeURIComponent(" <> s ^. argName . _PathSegment <> ") + '"

jsGParams :: Text -> [QueryArg f] -> Text
jsGParams _ []     = ""
jsGParams _ [x]    = paramToStr x False
jsGParams s (x:xs) = paramToStr x True <> s <> jsGParams s xs

jsParams :: [QueryArg f] -> Text
jsParams = jsGParams "&"

paramToStr :: QueryArg f -> Bool -> Text
paramToStr qarg notTheEnd =
  case qarg ^. queryArgType of
    Normal -> name
           <> "=' + encodeURIComponent("
           <> name
           <> if notTheEnd then ") + '" else ")"
    Flag   -> name <> "="
    List   -> name
           <> "[]=' + encodeURIComponent("
           <> name
           <> if notTheEnd then ") + '" else ")"
  where name = qarg ^. queryArgName . argName . _PathSegment
