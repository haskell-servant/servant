{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API (

  -- * Combinators
  module Servant.API.Sub,
  -- | Type-level combinator for expressing subrouting: @':>'@
  module Servant.API.Alternative,
  -- | Type-level combinator for alternative endpoints: @':<|>'@

  -- * Accessing information from the request
  module Servant.API.Capture,
  -- | Capturing parts of the url path as parsed values: @'Capture'@
  module Servant.API.Header,
  -- | Retrieving specific headers from the request
  module Servant.API.QueryParam,
  -- | Retrieving parameters from the query string of the 'URI': @'QueryParam'@
  module Servant.API.ReqBody,
  -- | Accessing the request body as a JSON-encoded type: @'ReqBody'@
  module Servant.API.MatrixParam,
  -- | Retrieving matrix parameters from the 'URI' segment: @'MatrixParam'@

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

  -- * Canonicalizing (flattening) API types
  Canonicalize,
  canonicalize,

  -- * Utilities
  module Servant.Utils.Links,
  -- | Type-safe internal URIs
  ) where

import           Data.Proxy                  (Proxy (..))
import           Servant.API.Alternative     ((:<|>) (..))
import           Servant.API.Capture         (Capture)
import           Servant.API.ContentTypes    (Accept (..), FormUrlEncoded,
                                              FromFormUrlEncoded (..), JSON,
                                              MimeRender (..),
                                              MimeUnrender (..), OctetStream,
                                              PlainText, ToFormUrlEncoded (..))
import           Servant.API.Delete          (Delete)
import           Servant.API.Get             (Get)
import           Servant.API.Header          (Header (..))
import           Servant.API.MatrixParam     (MatrixFlag, MatrixParam,
                                              MatrixParams)
import           Servant.API.Patch           (Patch)
import           Servant.API.Post            (Post)
import           Servant.API.Put             (Put)
import           Servant.API.QueryParam      (QueryFlag, QueryParam,
                                              QueryParams)
import           Servant.API.Raw             (Raw)
import           Servant.API.ReqBody         (ReqBody)
import           Servant.API.ResponseHeaders (AddHeader (addHeader),
                                              BuildHeadersTo (buildHeadersTo),
                                              GetHeaders (getHeaders),
                                              HList (..), Headers (..),
                                              getHeadersHList, getResponse)
import           Servant.API.Sub             ((:>))
import           Servant.Common.Text         (FromText (..), ToText (..))
import           Servant.Utils.Links         (HasLink (..), IsElem, IsElem',
                                              URI (..), safeLink)

-- | Turn an API type into its canonical form.
--
-- The canonical form of an API type is basically the all-flattened form
-- of the original type. More formally, it takes a type as input and hands you
-- back an /equivalent/ type formed of toplevel `:<|>`-separated chains of `:>`s,
-- i.e with all `:>`s distributed inside the `:<|>`s.
--
-- It basically turns:
--
-- > "hello" :> (Get Hello :<|> ReqBody Hello :> Put Hello)
--
-- into
--
-- > ("hello" :> Get Hello) :<|> ("hello" :> ReqBody Hello :> Put Hello)
--
-- i.e distributing all ':>'-separated bits into the subsequent ':<|>'s.
type family Canonicalize api :: * where
  -- requires UndecidableInstances
  Canonicalize (a :> (b :<|> c)) = a :> Canonicalize b :<|> a :> Canonicalize c
  Canonicalize ((a :<|> b) :> c) = a :> Canonicalize c :<|> b :> Canonicalize c
  Canonicalize (a :> b)          = Redex b (Canonicalize b) a
  Canonicalize (a :<|> b)        = Canonicalize a :<|> Canonicalize b
  Canonicalize a                 = a

type family Redex a b c :: * where
  Redex a a first = Canonicalize first :> a
  Redex a b first = Canonicalize (first :> b)

canonicalize :: Proxy layout -> Proxy (Canonicalize layout)
canonicalize Proxy = Proxy
