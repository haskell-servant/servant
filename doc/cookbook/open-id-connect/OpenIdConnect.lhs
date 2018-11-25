[OpenID Connect](https://openid.net/connect/)
=============================================

Use OpenID Connect to authenticate your users. In this example, we'll
only focus on google OIDC provider.

Mainly the workflow use the OAuth2 Workflow and focus on providing
authentication infos.

That example was made for a working with single page application where
some login token would be saved in the user agent local storage.

Workflow:

1.  user is presentend with a login button,
2.  when the user click on the button it is redirected to the OIDC
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
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import qualified Data.Text                        as Text
import           Jose.Jwt
                 (Jwt (..), decodeClaims)
import           Network.HTTP.Client
                 (Manager, newManager)
import           Network.HTTP.Client.TLS
                 (tlsManagerSettings)
import           Network.Wai
                 (Request, requestHeaders)
import           Network.Wai.Handler.Warp
                 (run)
import           Servant
import           Servant.HTML.Blaze
                 (HTML)
import           Servant.Server.Experimental.Auth
                 (AuthHandler, AuthServerData, mkAuthHandler)
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

Still you can find a list of public OIDC provider here:
https://connect2id.com/products/nimbus-oauth-openid-connect-sdk/openid-connect-providers

I copied some here:

- Google: https://developers.google.com/identity/protocols/OpenIDConnect
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
                    , clientId = "client-id:xxx-xxx-xxx"
                    , clientPassword = "****" }
```

Then we declare our main server:

``` haskell
main :: IO ()
main = do
  oidcEnv <- initOIDC oidcConf
  run 3000 (app oidcEnv)

type API = IdentityRoutes Customer

api :: Proxy API
api = Proxy

server :: OIDCEnv -> Server API
server oidcEnv = serveOIDC oidcEnv handleOIDCLogin

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
```

``` haskell
data OIDCConf =
  OIDCConf { redirectUri    :: ByteString
           , clientId       :: ByteString
           , clientPassword :: ByteString
           } deriving (Show, Eq)
```

First we need to initialize OIDC.
A short explanation about what some detail is:

- to complete the workflow we need to make a POST request to the OIDC provider.
  So we need to create an http manager to make those call properly.
- Then in order to prevent replay attack, each time an user want to login we
  should provide a random string called the `state`. When the user is
  redirected to the redirect_uri, the OIDC provider should provide the same `state`
  along a `code` parameter.

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

We see here how `IdentityRoutes` are about two routes. The one to
redirect, and the other one the user will come back after login.

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
they'll click on the login button.
Concretely, you should provide a link on your web page to `https://yourdomain/login`
and when the user will click on it, it will be redirected to the OpenID provider.

``` haskell
genOIDCURL :: OIDCEnv -> IO ByteString
genOIDCURL OIDCEnv{..} = do
  st <- genState
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
POSTing that code to the OIDC provider we should be returned with a JSON
object. One of the field should be named `id_token` which should be a
JWT containing all the informations we need. Depending on the scopes we
asked we might get more informations.

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

The handleLoggedIn is that part that will retrieve the informations from
the user once he comes to us with a code.

``` haskell
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

data User = User { userId          :: Text
                 , userSecret      :: Text
                 , localStorageKey :: Text
                 , redirectUrl     :: Maybe Text
                 } deriving (Show,Eq,Ord)
```

When you render a User with blaze, it will generate a page with a js
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

-- | generate a random API Key, not necessarily extremely good randomness
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

appToErr :: ServantErr -> Text -> ServantErr
appToErr x msg = x
  { errBody = toS $ format (Err (toS (errReasonPhrase x)) msg)
  , errHeaders =  [("Content-Type","text/html")]}

unauthorized :: (MonadError ServantErr m) => Text -> m a
unauthorized = throwError . unauthorizedErr

unauthorizedErr :: Text -> ServantErr
unauthorizedErr = appToErr err401

forbidden :: (MonadError ServantErr m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServantErr
forbiddenErr = appToErr err403

notFound :: ( MonadError ServantErr m) => Text -> m a
notFound = throwError . notFoundErr

notFoundErr :: Text -> ServantErr
notFoundErr = appToErr err404

preconditionFailed :: ( MonadError ServantErr m) => Text -> m a
preconditionFailed = throwError . preconditionFailedErr

preconditionFailedErr :: Text -> ServantErr
preconditionFailedErr = appToErr err412

serverError :: ( MonadError ServantErr m) => Text -> m a
serverError = throwError . serverErrorErr

serverErrorErr :: Text -> ServantErr
serverErrorErr = appToErr err500
```
