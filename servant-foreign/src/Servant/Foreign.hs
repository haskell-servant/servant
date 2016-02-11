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
  , ForeignType
  , Arg
  , FunctionName
    -- lenses
  , reqUrl
  , reqMethod
  , reqHeaders
  , reqBody
  , reqReturnType
  , reqFuncName
  , path
  , queryStr
  , argName
  , argType
    -- prisms
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
  , HasNoForeignType
  , NoTypes
  , captureArg
  , isCapture
  , concatCase
  , snakeCase
  , camelCase
  , defReq
  , listFromAPI
  -- re-exports
  , module Servant.API
  ) where

import Servant.API
import Servant.Foreign.Internal
