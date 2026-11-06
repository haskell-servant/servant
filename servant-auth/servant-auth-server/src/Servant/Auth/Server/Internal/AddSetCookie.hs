{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Auth.Server.Internal.AddSetCookie where

import Blaze.ByteString.Builder (toByteString)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import qualified Data.SOP as SOP
import qualified Network.HTTP.Types as HTTP
import Network.Wai (mapResponseHeaders)
import Servant
import Servant.API.Generic
import Servant.API.MultiVerb
  ( AsUnion (..)
  , MultiVerb
  , OptHeader
  , ResponseType
  , ResponseTypes
  , WithHeaders
  )
import Servant.Server.Generic
import Web.Cookie

-- What are we doing here? Well, the idea is to add headers to the response,
-- but the headers come from the authentication check. In order to do that, we
-- tweak a little the general theme of recursing down the API tree; this time,
-- we recurse down a variation of it that adds headers to all the endpoints.
-- This involves the usual type-level checks.
--
-- TODO: If the endpoints already have headers, this will not work as is.

data Nat = Z | S Nat

type family AddSetCookiesApi (n :: Nat) a where
  AddSetCookiesApi ('S 'Z) a = AddSetCookieApi a
  AddSetCookiesApi ('S n) a = AddSetCookiesApi n (AddSetCookieApi a)

type family AddSetCookieApiVerb a where
  AddSetCookieApiVerb (Headers ls a) = Headers (Header "Set-Cookie" SetCookie ': ls) a
  AddSetCookieApiVerb a = Headers '[Header "Set-Cookie" SetCookie] a

#if MIN_VERSION_servant_server(0,18,1)
type family MapAddSetCookieApiVerb (as :: [Type]) where
  MapAddSetCookieApiVerb '[] = '[]
  MapAddSetCookieApiVerb (a ': as) = (AddSetCookieApiVerb a ': MapAddSetCookieApiVerb as)
#endif

type family AddSetCookieApi a :: Type

type instance AddSetCookieApi (a :> b) = a :> AddSetCookieApi b

type instance AddSetCookieApi (a :<|> b) = AddSetCookieApi a :<|> AddSetCookieApi b
#if MIN_VERSION_servant_server(0,19,0)
type instance AddSetCookieApi (NamedRoutes api) = AddSetCookieApi (ToServantApi api)
#endif
type instance
  AddSetCookieApi (Verb method stat ctyps a) =
    Verb method stat ctyps (AddSetCookieApiVerb a)
#if MIN_VERSION_servant_server(0,18,1)
type instance AddSetCookieApi (UVerb method ctyps as)
  = UVerb method ctyps (MapAddSetCookieApiVerb as)
#endif
type instance AddSetCookieApi Raw = Raw
#if MIN_VERSION_servant_server(0,15,0)
type instance AddSetCookieApi (Stream method stat framing ctyps a)
  = Stream method stat framing ctyps (AddSetCookieApiVerb a)
#endif
type instance AddSetCookieApi (Headers hs a) = AddSetCookieApiVerb (Headers hs a)

-- | Every response of a 'MultiVerb' endpoint gets an optional @Set-Cookie@
-- header. The handler gives the cookie together with its result: for an
-- endpoint with one response, as a pair; for an endpoint with several, as
-- 'WithSetCookie'.
type instance
  AddSetCookieApi (MultiVerb method cs as r) =
    MultiVerb method cs (AddSetCookieResponses as) (AddSetCookieMultiVerbResult as r)

-- | The responses of a 'MultiVerb' endpoint, each with an optional
-- @Set-Cookie@ header.
type family AddSetCookieResponses (as :: [Type]) :: [Type] where
  AddSetCookieResponses '[] = '[]
  AddSetCookieResponses (a ': as) =
    WithHeaders '[OptHeader (Header "Set-Cookie" SetCookie)] (ResponseType a, Maybe SetCookie) a
      ': AddSetCookieResponses as

-- | The handler result of a 'MultiVerb' endpoint with the cookie it sets.
-- The result of an endpoint with one response is paired with the cookie,
-- because servant converts such an endpoint's result with its own
-- 'AsUnion' instance.
type family AddSetCookieMultiVerbResult (as :: [Type]) r where
  AddSetCookieMultiVerbResult '[a] r = (r, Maybe SetCookie)
  AddSetCookieMultiVerbResult as r = WithSetCookie as r

-- | The result of a handler for a 'MultiVerb' endpoint with several
-- responses @as@, and the cookie to set on whichever response it gives.
data WithSetCookie (as :: [Type]) r = WithSetCookie (Maybe SetCookie) r

instance
  (AddSetCookieUnion as, AsUnion as r, bs ~ AddSetCookieResponses as)
  => AsUnion bs (WithSetCookie as r)
  where
  toUnion (WithSetCookie cookie r) =
    addSetCookieUnion (Proxy :: Proxy as) cookie (toUnion @as r)
  fromUnion u =
    let (cookie, u') = dropSetCookieUnion (Proxy :: Proxy as) u
     in WithSetCookie cookie (fromUnion @as u')

-- | Moves a 'MultiVerb' result between the responses @as@ and the same
-- responses with an optional @Set-Cookie@ header.
class AddSetCookieUnion (as :: [Type]) where
  addSetCookieUnion
    :: Proxy as
    -> Maybe SetCookie
    -> Union (ResponseTypes as)
    -> Union (ResponseTypes (AddSetCookieResponses as))
  dropSetCookieUnion
    :: Proxy as
    -> Union (ResponseTypes (AddSetCookieResponses as))
    -> (Maybe SetCookie, Union (ResponseTypes as))

instance AddSetCookieUnion '[] where
  addSetCookieUnion _ _ u = u
  dropSetCookieUnion _ u = (Nothing, u)

instance AddSetCookieUnion as => AddSetCookieUnion (a ': as) where
  addSetCookieUnion _ cookie (SOP.Z (SOP.I x)) = SOP.Z (SOP.I (x, cookie))
  addSetCookieUnion _ cookie (SOP.S rest) =
    SOP.S (addSetCookieUnion (Proxy :: Proxy as) cookie rest)
  dropSetCookieUnion _ (SOP.Z (SOP.I (x, cookie))) = (cookie, SOP.Z (SOP.I x))
  dropSetCookieUnion _ (SOP.S rest) = SOP.S <$> dropSetCookieUnion (Proxy :: Proxy as) rest

data SetCookieList (n :: Nat) :: Type where
  SetCookieNil :: SetCookieList 'Z
  SetCookieCons :: Maybe SetCookie -> SetCookieList n -> SetCookieList ('S n)

class AddSetCookies (n :: Nat) orig new where
  addSetCookies :: SetCookieList n -> orig -> new

instance
  {-# OVERLAPS #-}
  AddSetCookies ('S n) oldb newb
  => AddSetCookies ('S n) (a -> oldb) (a -> newb)
  where
  addSetCookies cookies oldfn = addSetCookies cookies . oldfn

instance orig1 ~ orig2 => AddSetCookies 'Z orig1 orig2 where
  addSetCookies _ = id

instance
  {-# OVERLAPPABLE #-}
  ( AddHeader mods "Set-Cookie" SetCookie cookied new
  , AddSetCookies n (m old) (m cookied)
  , Functor m
  )
  => AddSetCookies ('S n) (m old) (m new)
  where
  addSetCookies (mCookie `SetCookieCons` rest) oldVal =
    case mCookie of
      Nothing -> noHeader' <$> addSetCookies rest oldVal
      Just cookie -> addHeader' cookie <$> addSetCookies rest oldVal

-- | A 'MultiVerb' endpoint with one response: the result paired with the cookie.
instance
  {-# OVERLAPPING #-}
  (AddSetCookies n (m old) (m new), Functor m)
  => AddSetCookies ('S n) (m old) (m (new, Maybe SetCookie))
  where
  addSetCookies (mCookie `SetCookieCons` rest) oldVal =
    (,mCookie) <$> addSetCookies rest oldVal

-- | A 'MultiVerb' endpoint with several responses: the result with the cookie.
instance
  {-# OVERLAPPING #-}
  (AddSetCookies n (m old) (m new), Functor m)
  => AddSetCookies ('S n) (m old) (m (WithSetCookie as new))
  where
  addSetCookies (mCookie `SetCookieCons` rest) oldVal =
    WithSetCookie mCookie <$> addSetCookies rest oldVal

instance
  {-# OVERLAPS #-}
  (AddSetCookies ('S n) a a', AddSetCookies ('S n) b b')
  => AddSetCookies ('S n) (a :<|> b) (a' :<|> b')
  where
  addSetCookies cookies (a :<|> b) = addSetCookies cookies a :<|> addSetCookies cookies b

instance
  {-# OVERLAPPING #-}
  (AddSetCookies ('S n) a a, AddSetCookies ('S n) b b')
  => AddSetCookies ('S n) (a :<|> b) (a :<|> b')
  where
  addSetCookies cookies (a :<|> b) = addSetCookies cookies a :<|> addSetCookies cookies b

instance
  {-# OVERLAPS #-}
  ( AddSetCookies ('S n) (ServerT (ToServantApi api) m) cookiedApi
  , GServantProduct (Rep (api (AsServerT m)))
  , Generic (api (AsServerT m))
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  )
  => AddSetCookies ('S n) (api (AsServerT m)) cookiedApi
  where
  addSetCookies cookies = addSetCookies cookies . toServant

-- | for @servant <0.11@
instance AddSetCookies ('S n) Application Application where
  addSetCookies cookies r request respond =
    r request $ respond . mapResponseHeaders (++ mkHeaders cookies)

-- | for @servant >=0.11@
instance AddSetCookies ('S n) (Tagged m Application) (Tagged m Application) where
  addSetCookies cookies r = Tagged $ \request respond ->
    unTagged r request $ respond . mapResponseHeaders (++ mkHeaders cookies)

mkHeaders :: SetCookieList x -> [HTTP.Header]
mkHeaders x = ("Set-Cookie",) <$> mkCookies x
  where
    mkCookies :: forall y. SetCookieList y -> [BS.ByteString]
    mkCookies SetCookieNil = []
    mkCookies (SetCookieCons Nothing rest) = mkCookies rest
    mkCookies (SetCookieCons (Just y) rest) =
      toByteString (renderSetCookie y) : mkCookies rest
