-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign
  ( ArgType(..)
  , HeaderArg(..)
  , QueryArg(..)
  , Req(..)
  , Segment(..)
  , SegmentType(..)
  , Url(..)
    -- aliases
  , Path
  , Arg(..)
  , FunctionName(..)
  , PathSegment(..)
    -- lenses
  , argName
  , argType
  , argPath
  , reqUrl
  , reqMethod
  , reqHeaders
  , reqBody
  , reqReturnType
  , reqFuncName
  , path
  , queryStr
  , queryArgName
  , queryArgType
  , headerArg
    -- prisms
  , _PathSegment
  , _HeaderArg
  , _ReplaceHeaderArg
  , _Static
  , _Cap
  , _Normal
  , _Flag
  , _List
    -- rest of it
  , HasForeign(..)
  , HasForeignType(..)
  , GenerateList(..)
  , NoTypes
  , captureArg
  , isCapture
  , defReq
  , listFromAPI
    -- re-exports
  , module Servant.API
  , module Servant.Foreign.Inflections
  ) where

import Servant.API
import Servant.Foreign.Internal
import Servant.Foreign.Inflections
