module Servant.API (

  -- * Combinators
  -- | Type-level combinator for expressing routing, captures, get parameters, etc.
  module Servant.API.Sub,
  -- | Type-level combinator for alternative endpoints
  module Servant.API.Union,

  -- * Accessing information from the request
  -- | Capturing parts of the url path as parsed values
  module Servant.API.Capture,
  -- | Retrieving parameters from the query part of the 'URI'
  module Servant.API.GetParam,
  -- | Accessing the request's body
  module Servant.API.RQBody,

  -- * Actual endpoints, distinguished by HTTP method
  -- | GET requests
  module Servant.API.Get,
  -- | POST requests
  module Servant.API.Post,
  -- | DELETE requests
  module Servant.API.Delete,
  -- | PUT requests
  module Servant.API.Put,
  ) where

import Servant.API.Capture
import Servant.API.Delete
import Servant.API.Get
import Servant.API.GetParam
import Servant.API.Post
import Servant.API.Put
import Servant.API.RQBody
import Servant.API.Sub
import Servant.API.Union
