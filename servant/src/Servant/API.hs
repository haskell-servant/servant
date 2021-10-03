module Servant.API (

  -- * Combinators
  module Servant.API.Sub,
  -- | Type-level combinator for expressing subrouting: @':>'@
  module Servant.API.Alternative,
  -- | Type-level combinator for alternative endpoints: @':<|>'@
  module Servant.API.Empty,
  -- | Type-level combinator for an empty API: @'EmptyAPI'@
  module Servant.API.Modifiers,
  -- | Type-level modifiers for 'QueryParam', 'Header' and 'ReqBody'.

  -- * Accessing information from the request
  module Servant.API.Capture,
  -- | Capturing parts of the url path as parsed values: @'Capture'@ and @'CaptureAll'@
  module Servant.API.Header,
  -- | Retrieving specific headers from the request
  module Servant.API.HttpVersion,
  -- | Retrieving the HTTP version of the request
  module Servant.API.QueryParam,
  -- | Retrieving parameters from the query string of the 'URI': @'QueryParam'@
  module Servant.API.Fragment,
  -- | Documenting the fragment of the 'URI': @'Fragment'@
  module Servant.API.ReqBody,
  -- | Accessing the request body as a JSON-encoded type: @'ReqBody'@
  module Servant.API.RemoteHost,
  -- | Retrieving the IP of the client
  module Servant.API.IsSecure,
  -- | Is the request made through HTTPS?
  module Servant.API.Vault,
  -- | Access the location for arbitrary data to be shared by applications and middleware
  module Servant.API.WithNamedContext,
  -- | Access context entries in combinators in servant-server

  -- * Actual endpoints, distinguished by HTTP method
  module Servant.API.Verbs,
  module Servant.API.UVerb,

  -- * Sub-APIs defined as records of routes
  module Servant.API.NamedRoutes,

  -- * Streaming endpoints, distinguished by HTTP method
  module Servant.API.Stream,

  -- * Authentication
  module Servant.API.BasicAuth,

  -- * Endpoints description
  module Servant.API.Description,

  -- * Content Types
  module Servant.API.ContentTypes,
  -- | Serializing and deserializing types based on @Accept@ and
  -- @Content-Type@ headers.

  -- * Response Headers
  module Servant.API.ResponseHeaders,

  -- * Untyped endpoints
  module Servant.API.Raw,
  -- | Plugging in a wai 'Network.Wai.Application', serving directories

  -- * FromHttpApiData and ToHttpApiData
  module Web.HttpApiData,
  -- | Classes and instances for types that can be converted to and from HTTP API data.


  -- * Experimental modules
  module Servant.API.Experimental.Auth,
  -- | General Authentication

  -- * Links
  module Servant.Links,
  -- | Type-safe internal URIs

  -- * Re-exports
  If,
  SBool (..), SBoolI (..)
  ) where

import           Data.Singletons.Bool
                 (SBool (..), SBoolI (..))
import           Data.Type.Bool
                 (If)
import           Servant.API.Alternative
                 ((:<|>) (..))
import           Servant.API.BasicAuth
                 (BasicAuth, BasicAuthData (..))
import           Servant.API.Capture
                 (Capture, Capture', CaptureAll)
import           Servant.API.ContentTypes
                 (Accept (..), FormUrlEncoded, JSON, MimeRender (..),
                 MimeUnrender (..), NoContent (NoContent), OctetStream,
                 PlainText)
import           Servant.API.Description
                 (Description, Summary)
import           Servant.API.Empty
                 (EmptyAPI (..))
import           Servant.API.Experimental.Auth
                 (AuthProtect)
import           Servant.API.Fragment
                 (Fragment)
import           Servant.API.Header
                 (Header, Header')
import           Servant.API.HttpVersion
                 (HttpVersion (..))
import           Servant.API.IsSecure
                 (IsSecure (..))
import           Servant.API.Modifiers
                 (Lenient, Optional, Required, Strict)
import           Servant.API.QueryParam
                 (QueryFlag, QueryParam, QueryParam', QueryParams)
import           Servant.API.Raw
                 (Raw)
import           Servant.API.RemoteHost
                 (RemoteHost)
import           Servant.API.ReqBody
                 (ReqBody, ReqBody')
import           Servant.API.ResponseHeaders
                 (AddHeader, BuildHeadersTo (buildHeadersTo),
                 GetHeaders (getHeaders), HList (..), HasResponseHeader,
                 Headers (..), ResponseHeader (..), addHeader, getHeadersHList,
                 getResponse, lookupResponseHeader, noHeader)
import           Servant.API.Stream
                 (FramingRender (..), FramingUnrender (..), FromSourceIO (..),
                 NetstringFraming, NewlineFraming, NoFraming, SourceIO, Stream,
                 StreamBody, StreamBody', StreamGet, StreamPost,
                 ToSourceIO (..))
import           Servant.API.Sub
                 ((:>))
import           Servant.API.UVerb
                 (HasStatus, IsMember, StatusOf, Statuses, UVerb, Union,
                 Unique, WithStatus (..), inject, statusOf)
import           Servant.API.Vault
                 (Vault)
import           Servant.API.NamedRoutes
                 (NamedRoutes)
import           Servant.API.Verbs
                 (Delete, DeleteAccepted, DeleteNoContent,
                 DeleteNonAuthoritative, Get, GetAccepted, GetNoContent,
                 GetNonAuthoritative, GetPartialContent, GetResetContent,
                 NoContentVerb, Patch, PatchAccepted, PatchNoContent,
                 PatchNonAuthoritative, Post, PostAccepted, PostCreated,
                 PostNoContent, PostNonAuthoritative, PostResetContent, Put,
                 PutAccepted, PutCreated, PutNoContent, PutNonAuthoritative,
                 ReflectMethod (reflectMethod), StdMethod (..), Verb)
import           Servant.API.WithNamedContext
                 (WithNamedContext)
import           Servant.Links
                 (HasLink (..), IsElem, IsElem', Link, URI (..), safeLink)
import           Web.HttpApiData
                 (FromHttpApiData (..), ToHttpApiData (..))
