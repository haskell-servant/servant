{-# OPTIONS_HADDOCK not-home #-}

-- | Type safe generation of internal links.
--
-- Given an API with a few endpoints:
--
-- >>> :set -XDataKinds -XTypeFamilies -XTypeOperators -XPolyKinds
-- >>> import Servant.API
-- >>> import Servant.Links
-- >>> import Web.HttpApiData (toUrlPiece)
-- >>> import Data.Proxy
-- >>> import Data.Text (Text)
-- >>>
-- >>> type Hello = "hello" :> Get '[JSON] Int
-- >>> type Bye   = "bye"   :> QueryParam "name" String :> Delete '[JSON] NoContent
-- >>> type API   = Hello :<|> Bye
-- >>> let api = Proxy :: Proxy API
--
-- It is possible to generate links that are guaranteed to be within 'API' with
-- 'safeLink'. The first argument to 'safeLink' is a type representing the API
-- you would like to restrict links to. The second argument is the destination
-- endpoint you would like the link to point to, this will need to end with a
-- verb like GET or POST. Further arguments may be required depending on the
-- type of the endpoint. If everything lines up you will get a 'Link' out the
-- other end.
--
-- You may omit 'QueryParam's and the like should you not want to provide them,
-- but types which form part of the URL path like 'Capture' must be included.
-- The reason you may want to omit 'QueryParam's is that safeLink is a bit
-- magical: if parameters are included that could take input it will return a
-- function that accepts that input and generates a link. This is best shown
-- with an example. Here, a link is generated with no parameters:
--
-- >>> let hello = Proxy :: Proxy ("hello" :> Get '[JSON] Int)
-- >>> toUrlPiece (safeLink api hello :: Link)
-- "hello"
--
-- If the API has an endpoint with parameters then we can generate links with
-- or without those:
--
-- >>> let with = Proxy :: Proxy ("bye" :> QueryParam "name" String :> Delete '[JSON] NoContent)
-- >>> toUrlPiece $ safeLink api with (Just "Hubert")
-- "bye?name=Hubert"
--
-- >>> let without = Proxy :: Proxy ("bye" :> Delete '[JSON] NoContent)
-- >>> toUrlPiece $ safeLink api without
-- "bye"
--
-- If you would like to create a helper for generating links only within that API,
-- you can partially apply safeLink if you specify a correct type signature
-- like so:
--
-- >>> :set -XConstraintKinds
-- >>> :{
-- >>> let apiLink :: (IsElem endpoint API, HasLink endpoint)
-- >>>             => Proxy endpoint -> MkLink endpoint Link
-- >>>     apiLink = safeLink api
-- >>> :}
--
-- `safeLink'` allows you to specialise the output:
--
-- >>> safeLink' toUrlPiece api without
-- "bye"
--
-- >>> :{
-- >>> let apiTextLink :: (IsElem endpoint API, HasLink endpoint)
-- >>>                  => Proxy endpoint -> MkLink endpoint Text
-- >>>     apiTextLink = safeLink' toUrlPiece api
-- >>> :}
--
-- >>> apiTextLink without
-- "bye"
--
-- Attempting to construct a link to an endpoint that does not exist in api
-- will result in a type error like this:
--
-- >>> let bad_link = Proxy :: Proxy ("hello" :> Delete '[JSON] NoContent)
-- >>> safeLink api bad_link
-- ...
-- ...Could not ...
-- ...
--
--  This error is essentially saying that the type family couldn't find
--  bad_link under api after trying the open (but empty) type family
--  `IsElem'` as a last resort.
--
--  @since 0.14.1
module Servant.Links
  ( module Servant.API.TypeLevel

    -- * Building and using safe links

  --

    -- | Note that 'URI' is from the "Network.URI" module in the @network-uri@ package.
  , safeLink
  , safeLink'
  , allLinks
  , allLinks'
  , URI (..)

    -- * Generics
  , AsLink
  , fieldLink
  , fieldLink'
  , allFieldLinks
  , allFieldLinks'

    -- * Adding custom types
  , HasLink (..)
  , Link
  , linkURI
  , linkURI'
  , LinkArrayElementStyle (..)

    -- ** Link accessors
  , Param (..)
  , linkSegments
  , linkQueryParams
  , linkFragment
  , addQueryParam
  , addSegment
  , Escaped
  , escaped
  , getEscaped
  )
where

import Network.URI (URI (..))

import Servant.API.TypeLevel
import Servant.Internal.Links
