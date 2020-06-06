[OpenID Connect](https://openid.net/connect/)
=============================================

Use OpenID Connect to authenticate your users.
This example use google OIDC provider.
It was made for a working with single page application where
some login token would be saved in the user agent local storage.

Workflow:

1.  user is presented with a login button,
2.  when the user clicks on the button it is redirected to the OIDC
    provider,
3.  the user login in the OIDC provider,
4.  the OIDC provider will redirect the user and provide a `code`,
5.  the server will use this code to make a POST to the OIDC provider
    and will get back authentication infos,
6.  The user will get display an HTML page that will save a secret
    identifying him in the local storage, then it will be redirected to
    /.

Let's put the imports behind us:

``` haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import           Protolude

import           Data.Aeson
                 (FromJSON (..), (.:))
import qualified Data.Aeson                       as JSON
import qualified Data.Aeson.Types                 as AeT
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import qualified Data.Text                        as Text
import           Jose.Jwt
                 (Jwt (..), decodeClaims)
import           Network.HTTP.Client
                 (Manager, newManager)
import           Network.HTTP.Client.TLS
                 (tlsManagerSettings)
import           Network.Wai.Handler.Warp
                 (run)
import           Servant
import           Servant.HTML.Blaze
                 (HTML)
import qualified System.Random                    as Random
import           Text.Blaze
                 (ToMarkup (..))
import qualified Text.Blaze.Html                  as H
import           Text.Blaze.Html5
                 ((!))
import qualified Text.Blaze.Html5                 as H
import qualified Text.Blaze.Html5.Attributes      as HA
import           Text.Blaze.Renderer.Utf8
                 (renderMarkup)
import qualified Web.OIDC.Client                  as O
```

You'll need to create a new OpenID Connect client in an OpenID Provider.
This example was tested with Google.

You can find a list of public OIDC provider here:
https://connect2id.com/products/nimbus-oauth-openid-connect-sdk/openid-connect-providers

I copied some here:

- Google: https://developers.google.com/identity/protocols/OpenIDConnect
  more precisely: https://console.developers.google.com/apis/credentials
- Microsoft: https://docs.microsoft.com/en-us/previous-versions/azure/dn645541(v=azure.100)
- Yahoo: https://developer.yahoo.com/oauth2/guide/openid_connect/
- PayPal: https://developer.paypal.com/docs/integration/direct/identity/log-in-with-paypal/

During the configuration you'll need to provide a redirect uri.
The redirect_uri should correspond to the uri user will be redirected to
after a successful login into the OpenID provider.

So during your test, you should certainly just use `http://localhost:3000/login/cb`.
In general you should use your own domain name.

You'll then be given a `client_id` and a `client_password`.
Fill those values in here:

``` haskell
oidcConf :: OIDCConf
oidcConf = OIDCConf { redirectUri = "http://localhost:3000/login/cb"
                    , clientId = "xxxxxxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.apps.googleusercontent.com"
                    , clientPassword = "************************" }
```

Then we declare our main server:

``` haskell
main :: IO ()
main = do
  oidcEnv <- initOIDC oidcConf
  run 3000 (app oidcEnv)

type API = IdentityRoutes Customer
         :<|> Get '[HTML] Homepage

api :: Proxy API
api = Proxy

server :: OIDCEnv -> Server API
server oidcEnv = serveOIDC oidcEnv handleOIDCLogin
               :<|> return Homepage

-- | Then main app
app :: OIDCEnv -> Application
app oidcEnv = serve api (server oidcEnv)
```

OIDC
----

That part try to separate concern, and certainly in a real world
application that should be in its distinct module.

``` haskell
-- * OIDC

data OIDCConf =
  OIDCConf { redirectUri    :: ByteString
           , clientId       :: ByteString
           , clientPassword :: ByteString
           } deriving (Show, Eq)
```

First we need to initialize OIDC.
A short explanation about it:

- to complete the workflow we need to make a POST request to the OIDC provider.
  So we need to create an http manager to make those call properly.
- Then in order to prevent replay attack, each time an user wants to login we
  should provide a random string called the `state`. When the user is
  redirected to the `redirect_uri`, the OIDC provider should provide the same
 `state` along a `code` parameter.

``` haskell
initOIDC :: OIDCConf -> IO OIDCEnv
initOIDC OIDCConf{..} = do
  mgr  <- newManager tlsManagerSettings
  prov <- O.discover "https://accounts.google.com" mgr
  let oidc     = O.setCredentials clientId clientPassword redirectUri (O.newOIDC prov)
  return OIDCEnv { oidc = oidc
                 , mgr = mgr
                 , genState = genRandomBS
                 , prov = prov
                 , redirectUri = redirectUri
                 , clientId = clientId
                 , clientPassword = clientPassword
                 }

data OIDCEnv = OIDCEnv { oidc           :: O.OIDC
                       , mgr            :: Manager
                       , genState       :: IO ByteString
                       , prov           :: O.Provider
                       , redirectUri    :: ByteString
                       , clientId       :: ByteString
                       , clientPassword :: ByteString
                       }
```

The `IdentityRoutes` are two endpoints:

- an endpoint to redirect the users to the OIDC Provider,
- another one the user will be redirected to from the OIDC Provider.

``` haskell
type IdentityRoutes a =
  "login" :> ( -- redirect User to the OpenID Provider
              Get '[JSON] NoContent
               -- render the page that will save the user creds in the user-agent
              :<|> "cb" :> QueryParam "error" Text
                        :> QueryParam "code" Text
                        :> Get '[HTML] User)

-- | gen a 302 redirect helper
redirects :: (StringConv s ByteString) => s -> Handler ()
redirects url = throwError err302 { errHeaders = [("Location",toS url)]}
```

That function will generate the URL to redirect the users to when
they'll click on the login link: `https://yourdomain/login`.

``` haskell
genOIDCURL :: OIDCEnv -> IO ByteString
genOIDCURL OIDCEnv{..} = do
  st <- genState -- generate a random string
  let oidcCreds = O.setCredentials clientId clientPassword redirectUri (O.newOIDC prov)
  loc <- O.getAuthenticationRequestUrl oidcCreds [O.openId, O.email, O.profile] (Just st) []
  return (show loc)

handleLogin :: OIDCEnv -> Handler NoContent
handleLogin oidcenv = do
  loc <- liftIO (genOIDCURL oidcenv)
  redirects loc
  return NoContent
```

The `AuthInfo` is about the infos we can grab from OIDC provider.

To be more precise, the user should come with a `code` (a token) and
POSTing that code to the correct OIDC provider endpoint should return a JSON
object. One of the fields should be named `id_token` which should be a
JWT containing all the information we need. Depending on the scopes we
asked we might get more information.

``` haskell
-- | @AuthInfo@
data AuthInfo = AuthInfo { email         :: Text
                         , emailVerified :: Bool
                         , name :: Text } deriving (Eq, Show, Generic)

instance FromJSON AuthInfo where
  parseJSON (JSON.Object v) = do
    email   :: Text <- v .: "email"
    email_verified   :: Bool <- v .: "email_verified"
    name :: Text <- v .: "name"
    return $ AuthInfo (toS email) email_verified (toS name)
  parseJSON invalid    = AeT.typeMismatch "Coord" invalid
instance JSON.ToJSON AuthInfo where
  toJSON (AuthInfo e ev n) =
    JSON.object [ "email"   JSON..= (toS e :: Text)
                , "email_verified"  JSON..= ev
                , "name" JSON..= (toS n :: Text)
                ]

type LoginHandler = AuthInfo -> IO (Either Text User)
```

The `handleLoggedIn` is that part that will retrieve the information from
the user once he is redirected from the OIDC Provider after login.

If the user is redirected to the `redirect_uri` but with an `error` query
parameter then it means something went wrong.
If there is no error query param but a `code` query param it means the user
successfully logged in. From there we need to make a request to the token
endpoint of the OIDC provider. It's a POST that should contain the code
as well as the client id and secret.
Making this HTTP POST is the responsibility of `requestTokens`.

From there we extract the `claims` of the JWT contained in one of the value
of the JSON returned by the POST HTTP Request.

``` haskell
data User = User { userId          :: Text
                 , userSecret      :: Text
                 , localStorageKey :: Text
                 , redirectUrl     :: Maybe Text
                 } deriving (Show,Eq,Ord)

handleLoggedIn :: OIDCEnv
               -> LoginHandler -- ^ handle successful id
               -> Maybe Text -- ^ error
               -> Maybe Text -- ^ code
               -> Handler User
handleLoggedIn oidcenv handleSuccessfulId err mcode =
  case err of
    Just errorMsg -> forbidden errorMsg
    Nothing -> case mcode of
      Just oauthCode -> do
        tokens <- liftIO $ O.requestTokens (oidc oidcenv) (toS oauthCode) (mgr oidcenv)
        putText . show . O.claims . O.idToken $ tokens
        let jwt = toS . unJwt . O.jwt . O.idToken $ tokens
            eAuthInfo = decodeClaims jwt :: Either O.JwtError (O.JwtHeader,AuthInfo)
        case eAuthInfo of
          Left jwtErr -> forbidden $ "JWT decode/check problem: " <> show jwtErr
          Right (_,authInfo) ->
            if emailVerified authInfo
              then do
                user <- liftIO $ handleSuccessfulId authInfo
                either forbidden return user
              else forbidden "Please verify your email"
      Nothing -> do
        liftIO $ putText "No code param"
        forbidden "no code parameter given"
```

When you render a User with blaze-html, it will generate a page with a js
that will put a secret for that user in the local storage. And it will
redirect the user to /.

``` haskell
instance ToMarkup User where
  toMarkup User{..} = H.docTypeHtml $ do
    H.head $
      H.title "Logged In"
    H.body $ do
      H.h1 "Logged In"
      H.p (H.toHtml ("Successful login with id " <> userId))
      H.script (H.toHtml ("localStorage.setItem('" <> localStorageKey <> "','" <> userSecret <> "');"
                          <> "localStorage.setItem('user-id','" <> userId <> "');"
                         <> "window.location='" <> fromMaybe "/" redirectUrl <> "';" -- redirect the user to /
                         ));

serveOIDC :: OIDCEnv -> LoginHandler -> Server (IdentityRoutes a)
serveOIDC oidcenv loginHandler =
  handleLogin oidcenv :<|> handleLoggedIn oidcenv loginHandler

-- * Auth
type APIKey = ByteString
type Account = Text.Text
type Conf = [(APIKey,Account)]
data Customer = Customer {
  account :: Account
  , apiKey :: APIKey
  , mail :: Maybe Text
  , fullname :: Maybe Text
  }
```

Here is the code that displays the homepage.
It should contain a link to the the `/login` URL.
When the user clicks on this link it will be redirected to Google login page
with some generated information.

The page also displays the content of the local storage.
And in particular the items `api-key` and `user-id`.
Those items should be set after a successful login when the user is redirected to
`/login/cb`.

The logic used generally is to use that api-key to uniquely identify an user.
Another option would have been to set a cookie.

``` haskell
data Homepage = Homepage

instance ToMarkup Homepage where
  toMarkup Homepage = H.docTypeHtml $ do
    H.head $ do
      H.title "OpenID Connect Servant Example"
      H.style (H.toHtml ("body { font-family: monospace; font-size: 18px; }" :: Text.Text))
    H.body $ do
      H.h1 "OpenID Connect Servant Example"
      H.div $
        H.a ! HA.href "/login" $ "Click here to login"
      H.ul $ do
        H.li $ do
          H.span "API Key in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('api-key'));" :: Text.Text))
        H.li $ do
          H.span "User ID in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('user-id'));" :: Text.Text))
```

We need some helpers to generate random string for generating state and API Keys.

``` haskell
-- | generate a random ByteString, not necessarily extremely good randomness
-- still the password will be long enough to be very difficult to crack
genRandomBS :: IO ByteString
genRandomBS = do
  g <- Random.newStdGen
  Random.randomRs (0, n) g & take 42 & fmap toChar & readable 0 & toS & return
  where
    n = length letters - 1
    toChar i = letters List.!! i
    letters = ['A'..'Z'] <> ['0'..'9'] <> ['a'..'z']
    readable :: Int -> [Char] -> [Char]
    readable _ [] = []
    readable i str =
      let blocksize = case n of
            0 -> 8
            1 -> 4
            2 -> 4
            3 -> 4
            _ -> 12
          block = take blocksize str
          rest = drop blocksize str
      in if List.null rest
         then str
         else block <> "-" <> readable (i+1) rest

customerFromAuthInfo :: AuthInfo -> IO Customer
customerFromAuthInfo authinfo = do
  apikey <- genRandomBS
  return Customer { account = toS (email authinfo)
                  , apiKey = apikey
                  , mail = Just (toS (email authinfo))
                  , fullname = Just (toS (name authinfo))
                  }

handleOIDCLogin :: LoginHandler
handleOIDCLogin authInfo = do
  custInfo <- customerFromAuthInfo authInfo
  if emailVerified authInfo
    then return . Right . customerToUser $ custInfo
    else return (Left "You emails is not verified by your provider. Please verify your email.")
  where
    customerToUser :: Customer -> User
    customerToUser c =
      User { userId = toS (account c)
           , userSecret = toS (apiKey c)
           , redirectUrl = Nothing
           , localStorageKey = "api-key"
           }
```

`Error` helpers
---------------

``` haskell
data Err = Err { errTitle :: Text
               , errMsg :: Text }

instance ToMarkup Err where
  toMarkup Err{..} = H.docTypeHtml $ do
    H.head $ do
      H.title "Error"
    H.body $ do
      H.h1 (H.a ! HA.href "/" $ "Home")
      H.h2 (H.toHtml errTitle)
      H.p (H.toHtml errMsg)

format :: ToMarkup a => a -> LBS.ByteString
format err = toMarkup err & renderMarkup

appToErr :: ServerError -> Text -> ServerError
appToErr x msg = x
  { errBody = toS $ format (Err (toS (errReasonPhrase x)) msg)
  , errHeaders =  [("Content-Type","text/html")]}

unauthorized :: (MonadError ServerError m) => Text -> m a
unauthorized = throwError . unauthorizedErr

unauthorizedErr :: Text -> ServerError
unauthorizedErr = appToErr err401

forbidden :: (MonadError ServerError m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServerError
forbiddenErr = appToErr err403

notFound :: ( MonadError ServerError m) => Text -> m a
notFound = throwError . notFoundErr

notFoundErr :: Text -> ServerError
notFoundErr = appToErr err404

preconditionFailed :: ( MonadError ServerError m) => Text -> m a
preconditionFailed = throwError . preconditionFailedErr

preconditionFailedErr :: Text -> ServerError
preconditionFailedErr = appToErr err412

serverError :: ( MonadError ServerError m) => Text -> m a
serverError = throwError . serverErrorErr

serverErrorErr :: Text -> ServerError
serverErrorErr = appToErr err500
```
