-------------------------------------------------------------------------------
-- | This module lets you get API docs for free. It lets you generate
-- an 'API' from the type that represents your API using 'docs':
--
-- @docs :: 'HasDocs' api => 'Proxy' api -> 'API'@
--
-- Alternatively, if you wish to add one or more introductions to your
-- documentation, use 'docsWithIntros':
--
-- @'docsWithIntros' :: 'HasDocs' api => [DocIntro] -> 'Proxy' api -> 'API'@
--
-- You can then call 'markdown' on the 'API' value:
--
-- @'markdown' :: 'API' -> String@
--
-- or define a custom pretty printer:
--
-- @yourPrettyDocs :: 'API' -> String -- or blaze-html's HTML, or ...@
--
-- The only thing you'll need to do will be to implement some classes
-- for your captures, get parameters and request or response bodies.
--
-- See example/greet.hs for an example.
module Servant.Docs
  ( -- * 'HasDocs' class and key functions
    HasDocs(..), docs, pretty, markdown
    -- ** Customising generated documentation
  , markdownWith, RenderingOptions(..), defRenderingOptions
  , requestExamples, responseExamples, ShowContentTypes(..), notesHeading
    -- * Generating docs with extra information
  , docsWith, docsWithIntros, docsWithOptions
  , ExtraInfo(..), extraInfo
  , DocOptions(..) , defaultDocOptions, maxSamples

  , -- * Classes you need to implement for your types
    ToSample(..)
  , toSample
  , noSamples
  , singleSample
  , samples
  , sampleByteString
  , sampleByteStrings
  , ToParam(..)
  , ToCapture(..)

  , -- * ADTs to represent an 'API'
    Endpoint, path, method, defEndpoint
  , API, apiIntros, apiEndpoints, emptyAPI
  , DocAuthentication(..), authIntro, authDataRequired
  , DocCapture(..), capSymbol, capDesc
  , DocQueryParam(..), ParamKind(..), paramName, paramValues, paramDesc, paramKind
  , DocNote(..), noteTitle, noteBody
  , DocIntro(..), introTitle, introBody
  , Response(..), respStatus, respTypes, respBody, defResponse
  , Action, authInfo, captures, headers, notes, params, rqtypes, rqbody, response, defAction
  , single
  ) where

import Servant.Docs.Internal
import Servant.Docs.Internal.Pretty
