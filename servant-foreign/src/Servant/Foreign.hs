-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign
  ( HasForeign(..)
  , Segment(..)
  , SegmentType(..)
  , FunctionName
  , QueryArg(..)
  , HeaderArg(..)
  , ArgType(..)
  , Req
  , captureArg
  , defReq
  , concatCase
  , snakeCase
  , camelCase
  -- lenses
  , argType
  , argName
  , isCapture
  , funcName
  , path
  , reqUrl
  , reqBody
  , reqHeaders
  , reqMethod
  , segment
  , queryStr
  -- re-exports
  , module Servant.API
  ) where

import Servant.API
import Servant.Foreign.Internal
