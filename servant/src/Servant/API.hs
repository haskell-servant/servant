module Servant.API (

  -- * Combinators
  module Servant.API.Sub,
  -- | Type-level combinator for expressing subrouting: @':>'@
  module Servant.API.Alternative,
  -- | Type-level combinator for alternative endpoints: @':<|>'@
  module Servant.API.Authentication,
  -- | Type-level combinator for endpoints requiring auth: @'BasicAuth'@

  -- * Accessing information from the request
  module Servant.API.Capture,
  -- | Capturing parts of the url path as parsed values: @'Capture'@
  module Servant.API.Header,
  -- | Retrieving specific headers from the request
  module Servant.API.HttpVersion,
  -- | Retrieving the HTTP version of the request
  module Servant.API.QueryParam,
  -- | Retrieving parameters from the query string of the 'URI': @'QueryParam'@
  module Servant.API.ReqBody,
  -- | Accessing the request body as a JSON-encoded type: @'ReqBody'@
  module Servant.API.MatrixParam,
  -- | Retrieving matrix parameters from the 'URI' segment: @'MatrixParam'@
  module Servant.API.RemoteHost,
  -- | Retrieving the IP of the client
  module Servant.API.IsSecure,
  -- | Is the request made through HTTPS?
  module Servant.API.Vault,
  -- | Access the location for arbitrary data to be shared by applications and middleware

  -- * Actual endpoints, distinguished by HTTP method
  module Servant.API.Get,
  -- | @GET@ requests
  module Servant.API.Post,
  -- | @POST@ requests
  module Servant.API.Delete,
  -- | @DELETE@ requests
  module Servant.API.Put,
  -- | @PUT@ requests
  module Servant.API.Patch,
  -- | @PATCH@ requests

  -- * Content Types
  module Servant.API.ContentTypes,
  -- | Serializing and deserializing types based on @Accept@ and
  -- @Content-Type@ headers.

  -- * Response Headers
  module Servant.API.ResponseHeaders,

  -- * Untyped endpoints
  module Servant.API.Raw,
  -- | Plugging in a wai 'Network.Wai.Application', serving directories

  -- * FromText and ToText
  module Servant.Common.Text,
  -- | Classes and instances for types that can be converted to and from @Text@

  -- * Utilities
  module Servant.Utils.Links,
  -- | Type-safe internal URIs
  ) where

import           Servant.API.Alternative     ((:<|>) (..))
import           Servant.API.Authentication  (BasicAuth)
import           Servant.API.Capture         (Capture)
import           Servant.API.ContentTypes    (Accept (..), FormUrlEncoded,
                                              FromFormUrlEncoded (..), JSON,
                                              MimeRender (..),
                                              MimeUnrender (..), OctetStream,
                                              PlainText, ToFormUrlEncoded (..))
import           Servant.API.Delete          (Delete)
import           Servant.API.Get             (Get)
import           Servant.API.Header          (Header (..))
import           Servant.API.HttpVersion     (HttpVersion (..))
import           Servant.API.IsSecure        (IsSecure (..))
import           Servant.API.MatrixParam     (MatrixFlag, MatrixParam,
                                              MatrixParams)
import           Servant.API.Patch           (Patch)
import           Servant.API.Post            (Post)
import           Servant.API.Put             (Put)
import           Servant.API.QueryParam      (QueryFlag, QueryParam,
                                              QueryParams)
import           Servant.API.Raw             (Raw)
import           Servant.API.RemoteHost      (RemoteHost)
import           Servant.API.ReqBody         (ReqBody)
import           Servant.API.ResponseHeaders (AddHeader (addHeader),
                                              BuildHeadersTo (buildHeadersTo),
                                              GetHeaders (getHeaders),
                                              HList (..), Headers (..),
                                              getHeadersHList, getResponse)
import           Servant.API.Sub             ((:>))
import           Servant.API.Vault           (Vault)
import           Servant.Common.Text         (FromText (..), ToText (..))
import           Servant.Utils.Links         (HasLink (..), IsElem, IsElem',
                                              URI (..), safeLink)

