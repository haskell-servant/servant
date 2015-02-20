module Servant.API (

  -- * Combinators
  -- | Type-level combinator for expressing subrouting: @':>'@
  module Servant.API.Sub,
  -- | Type-level combinator for alternative endpoints: @':<|>'@
  module Servant.API.Alternative,

  -- * Accessing information from the request
  -- | Capturing parts of the url path as parsed values: @'Capture'@
  module Servant.API.Capture,
  -- | Retrieving specific headers from the request
  module Servant.API.Header,
  -- | Retrieving parameters from the query string of the 'URI': @'QueryParam'@
  module Servant.API.QueryParam,
  -- | Accessing the request body as a JSON-encoded type: @'ReqBody'@
  module Servant.API.ReqBody,
  -- | Retrieving matrix parameters from the 'URI' segment: @'MatrixParam'@
  module Servant.API.MatrixParam,

  -- * Actual endpoints, distinguished by HTTP method
  -- | GET requests
  module Servant.API.Get,
  -- | POST requests
  module Servant.API.Post,
  -- | DELETE requests
  module Servant.API.Delete,
  -- | PUT requests
  module Servant.API.Put,
  -- | PATCH requests
  module Servant.API.Patch,

  -- * Content Types
  module Servant.API.ContentTypes,

  -- * Untyped endpoints
  -- | Plugging in a wai 'Network.Wai.Application', serving directories
  module Servant.API.Raw,

  -- * Utilities
  -- | QuasiQuotes for endpoints
  module Servant.QQ,
  -- | Type-safe internal URIs
  module Servant.Utils.Links,
  ) where

import Servant.API.Alternative ( (:<|>)(..) )
import Servant.API.Capture ( Capture )
import Servant.API.ContentTypes ( JSON , PlainText, OctetStream
                                , MimeRender(..) , MimeUnrender(..))
import Servant.API.Delete ( Delete )
import Servant.API.Get ( Get )
import Servant.API.Header ( Header )
import Servant.API.Post ( Post )
import Servant.API.Put ( Put )
import Servant.API.Patch ( Patch )
import Servant.API.QueryParam ( QueryFlag, QueryParams, QueryParam )
import Servant.API.MatrixParam ( MatrixFlag, MatrixParams, MatrixParam )
import Servant.API.Raw ( Raw )
import Servant.API.ReqBody ( ReqBody )
import Servant.API.Sub ( (:>)(..) )
import Servant.QQ ( sitemap )
import Servant.Utils.Links ( safeLink, URI(..), IsElem, IsElem', HasLink(..) )
