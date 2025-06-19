module Servant.Auth.Server.Internal.ConfigTypes
  ( module Servant.Auth.Server.Internal.ConfigTypes
  , Servant.API.IsSecure (..)
  )
where

import Crypto.JOSE as Jose
import Crypto.JWT as Jose
import qualified Data.ByteString as BS
import Data.Default
import Data.Time
import GHC.Generics (Generic)
import Servant.API (IsSecure (..))

data IsMatch = Matches | DoesNotMatch
  deriving (Eq, Generic, Ord, Read, Show)

data IsPasswordCorrect = PasswordCorrect | PasswordIncorrect
  deriving (Eq, Generic, Ord, Read, Show)

-- The @SameSite@ attribute of cookies determines whether cookies will be sent
-- on cross-origin requests.
--
-- See <https://tools.ietf.org/html/draft-west-first-party-cookies-07 this document>
-- for more information.
data SameSite = AnySite | SameSiteStrict | SameSiteLax
  deriving (Eq, Generic, Ord, Read, Show)

-- | @JWTSettings@ are used to generate cookies, and to verify JWTs.
data JWTSettings = JWTSettings
  { signingKey :: Jose.JWK
  -- ^ Key used to sign JWT.
  , jwtAlg :: Maybe Jose.Alg
  -- ^ Algorithm used to sign JWT.
  , validationKeys :: IO Jose.JWKSet
  -- ^ Keys used to validate JWT.
  , audienceMatches :: Jose.StringOrURI -> IsMatch
  -- ^ An @aud@ predicate. The @aud@ is a string or URI that identifies the
  -- intended recipient of the JWT.
  }
  deriving (Generic)

-- | A @JWTSettings@ where the audience always matches.
defaultJWTSettings :: Jose.JWK -> JWTSettings
defaultJWTSettings k =
  JWTSettings
    { signingKey = k
    , jwtAlg = Nothing
    , validationKeys = pure $ Jose.JWKSet [k]
    , audienceMatches = const Matches
    }

-- | The policies to use when generating cookies.
--
-- If *both* 'cookieMaxAge' and 'cookieExpires' are @Nothing@, browsers will
-- treat the cookie as a *session cookie*. These will be deleted when the
-- browser is closed.
--
-- Note that having the setting @Secure@ may cause testing failures if you are
-- not testing over HTTPS.
data CookieSettings = CookieSettings
  { cookieIsSecure :: !IsSecure
  -- ^ 'Secure' means browsers will only send cookies over HTTPS. Default:
  -- @Secure@.
  , cookieMaxAge :: !(Maybe DiffTime)
  -- ^ How long from now until the cookie expires. Default: @Nothing@.
  , cookieExpires :: !(Maybe UTCTime)
  -- ^ At what time the cookie expires. Default: @Nothing@.
  , cookiePath :: !(Maybe BS.ByteString)
  -- ^ The URL path and sub-paths for which this cookie is used. Default: @Just "/"@.
  , cookieDomain :: !(Maybe BS.ByteString)
  -- ^ Domain name, if set cookie also allows subdomains. Default: @Nothing@.
  , cookieSameSite :: !SameSite
  -- ^ 'SameSite' settings. Default: @SameSiteLax@.
  , sessionCookieName :: !BS.ByteString
  -- ^ What name to use for the cookie used for the session.
  , cookieXsrfSetting :: !(Maybe XsrfCookieSettings)
  -- ^ The optional settings to use for XSRF protection. Default: @Just def@.
  }
  deriving (Eq, Generic, Show)

instance Default CookieSettings where
  def = defaultCookieSettings

defaultCookieSettings :: CookieSettings
defaultCookieSettings =
  CookieSettings
    { cookieIsSecure = Secure
    , cookieMaxAge = Nothing
    , cookieExpires = Nothing
    , cookiePath = Just "/"
    , cookieDomain = Nothing
    , cookieSameSite = SameSiteLax
    , sessionCookieName = "JWT-Cookie"
    , cookieXsrfSetting = Just def
    }

-- | The policies to use when generating and verifying XSRF cookies
data XsrfCookieSettings = XsrfCookieSettings
  { xsrfCookieName :: !BS.ByteString
  -- ^ What name to use for the cookie used for XSRF protection.
  , xsrfCookiePath :: !(Maybe BS.ByteString)
  -- ^ What path to use for the cookie used for XSRF protection. Default @Just "/"@.
  , xsrfHeaderName :: !BS.ByteString
  -- ^ What name to use for the header used for XSRF protection.
  , xsrfExcludeGet :: !Bool
  -- ^ Exclude GET request method from XSRF protection.
  }
  deriving (Eq, Generic, Show)

instance Default XsrfCookieSettings where
  def = defaultXsrfCookieSettings

defaultXsrfCookieSettings :: XsrfCookieSettings
defaultXsrfCookieSettings =
  XsrfCookieSettings
    { xsrfCookieName = "XSRF-TOKEN"
    , xsrfCookiePath = Just "/"
    , xsrfHeaderName = "X-XSRF-TOKEN"
    , xsrfExcludeGet = False
    }

------------------------------------------------------------------------------
-- Internal {{{

jwtSettingsToJwtValidationSettings :: JWTSettings -> Jose.JWTValidationSettings
jwtSettingsToJwtValidationSettings s =
  defaultJWTValidationSettings (toBool <$> audienceMatches s)
  where
    toBool Matches = True
    toBool DoesNotMatch = False

-- }}}
