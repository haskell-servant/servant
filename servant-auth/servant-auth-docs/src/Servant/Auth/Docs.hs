{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Auth.Docs
  (
  -- | The purpose of this package is provide the instance for 'servant-auth'
  -- combinators needed for 'servant-docs' documentation generation.
  --
  -- >>> type API = Auth '[JWT, Cookie, BasicAuth] Int :> Get '[JSON] Int
  -- >>> putStr $ markdown $ docs (Proxy :: Proxy API)
  -- ## GET /
  -- ...
  -- ... Authentication
  -- ...
  -- This part of the API is protected by the following authentication mechanisms:
  -- ...
  --  * JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))
  --  * [Cookies](https://en.wikipedia.org/wiki/HTTP_cookie)
  --  * [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
  -- ...
  -- Clients must supply the following data
  -- ...
  -- One of the following:
  -- ...
  --  * A JWT Token signed with this server's key
  --  * Cookies automatically set by browsers, plus a header
  --  * Cookies automatically set by browsers, plus a header
  -- ...

  -- * Re-export
    JWT
  , BasicAuth
  , Cookie
  , Auth
  ) where

import Control.Lens          ((%~), (&), (|>))
import Data.List             (intercalate)
import Data.Monoid
import Data.Proxy            (Proxy (Proxy))
import Servant.API           hiding (BasicAuth)
import Servant.Auth
import Servant.Docs          hiding (pretty)
import Servant.Docs.Internal (DocAuthentication (..), authInfo)

instance (AllDocs auths, HasDocs api) => HasDocs (Auth auths r :> api) where
  docsFor _ (endpoint, action) =
    docsFor (Proxy :: Proxy api) (endpoint, action & authInfo %~ (|> info))
    where
      (intro, reqData) = pretty $ allDocs (Proxy :: Proxy auths)
      info = DocAuthentication intro reqData


pretty :: [(String, String)] -> (String, String)
pretty [] = error "shouldn't happen"
pretty [(i, d)] =
  ( "This part of the API is protected by " <> i
  , d
  )
pretty rs =
  ( "This part of the API is protected by the following authentication mechanisms:\n\n"
  ++  " * " <> intercalate "\n * " (fst <$> rs)
  , "\nOne of the following:\n\n"
  ++  " * " <> intercalate "\n * " (snd <$> rs)
  )


class AllDocs (x :: [*]) where
  allDocs :: proxy x
              -- intro, req
          -> [(String, String)]

instance (OneDoc a, AllDocs as) => AllDocs (a ': as) where
  allDocs _ = oneDoc (Proxy :: Proxy a) : allDocs (Proxy :: Proxy as)

instance AllDocs '[] where
  allDocs _ = []

class OneDoc a where
  oneDoc :: proxy a -> (String, String)

instance OneDoc JWT where
  oneDoc _ =
    ("JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))"
     , "A JWT Token signed with this server's key")

instance OneDoc Cookie where
  oneDoc _ =
    ("[Cookies](https://en.wikipedia.org/wiki/HTTP_cookie)"
    , "Cookies automatically set by browsers, plus a header")

instance OneDoc BasicAuth where
  oneDoc _ =
    ( "[Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)"
    , "Cookies automatically set by browsers, plus a header")

-- $setup
-- >>> instance ToSample Int where toSamples _ = singleSample 1729
