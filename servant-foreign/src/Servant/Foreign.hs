-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
--
-- See documentation of 'HasForeignType' for a simple example. 'listFromAPI' returns a list of all your endpoints and their foreign types, given a mapping from Haskell types to foreign types (conventionally called `ftypes` below).
module Servant.Foreign
  ( listFromAPI
  , Req(..)
  , defReq
  , HasForeignType(..)
  , GenerateList(..)
  , HasForeign(..)
  , NoTypes
  , ArgType(..)
  , HeaderArg(..)
  , QueryArg(..)
  , ReqBodyContentType(..)
  , Segment(..)
  , isCapture
  , captureArg
  , SegmentType(..)
  , Url(..)
    -- * aliases
  , Path
  , Arg(..)
  , FunctionName(..)
  , PathSegment(..)
    -- * lenses
  , argName
  , argType
  , argPath
  , reqUrl
  , reqMethod
  , reqHeaders
  , reqBody
  , reqBodyContentType
  , reqReturnType
  , reqFuncName
  , path
  , queryStr
  , queryArgName
  , queryArgType
  , headerArg
    -- * prisms
  , _PathSegment
  , _HeaderArg
  , _ReplaceHeaderArg
  , _Static
  , _Cap
  , _Normal
  , _Flag
  , _List
    -- * re-exports
  , module Servant.API
  , module Servant.Foreign.Inflections
  ) where

import           Servant.API
import           Servant.Foreign.Inflections
import           Servant.Foreign.Internal
