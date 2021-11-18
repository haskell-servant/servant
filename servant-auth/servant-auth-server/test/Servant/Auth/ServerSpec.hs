{-# LANGUAGE CPP #-}
module Servant.Auth.ServerSpec (spec) where

#if !MIN_VERSION_servant_server(0,16,0)
#define ServerError ServantErr
#endif

import           Control.Lens
import           Control.Monad.Except                (runExceptT)
import           Control.Monad.IO.Class              (liftIO)
import           Crypto.JOSE                         (Alg (HS256, None), Error,
                                                      JWK, JWSHeader,
                                                      KeyMaterialGenParam (OctGenParam),
                                                      ToCompact, encodeCompact,
                                                      genJWK, newJWSHeader)
import           Crypto.JWT                          (Audience (..), ClaimsSet,
                                                      NumericDate (NumericDate),
                                                      SignedJWT,
                                                      claimAud, claimNbf,
                                                      signClaims,
                                                      emptyClaimsSet,
                                                      unregisteredClaims)
import           Data.Aeson                          (FromJSON, ToJSON, Value,
                                                      toJSON, encode)
import           Data.Aeson.Lens                     (_JSON)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BSL
import           Data.CaseInsensitive                (mk)
import           Data.Foldable                       (find)
import           Data.Monoid
import           Data.Time
import           Data.Time.Clock                     (getCurrentTime)
import           GHC.Generics                        (Generic)
import           Network.HTTP.Client                 (cookie_http_only,
                                                      cookie_name, cookie_value,
                                                      cookie_expiry_time,
                                                      destroyCookieJar)
import           Network.HTTP.Types                  (Status, status200,
                                                      status401)
import           Network.Wai                         (responseLBS)
import           Network.Wai.Handler.Warp            (testWithApplication)
import           Network.Wreq                        (Options, auth, basicAuth,
                                                      cookieExpiryTime, cookies,
                                                      defaults, get, getWith, postWith,
                                                      header, oauth2Bearer,
                                                      responseBody,
                                                      responseCookieJar,
                                                      responseHeader,
                                                      responseStatus)
import           Network.Wreq.Types                  (Postable(..))
import           Servant                             hiding (BasicAuth,
                                                      IsSecure (..), header)
import           Servant.Auth.Server
import           Servant.Auth.Server.Internal.Cookie (expireTime)
import           Servant.Auth.Server.SetCookieOrphan ()
#if MIN_VERSION_servant_server(0,15,0)
import qualified Servant.Types.SourceT             as S
#endif
import           System.IO.Unsafe                    (unsafePerformIO)
import           Test.Hspec
import           Test.QuickCheck
import qualified Network.HTTP.Client as HCli



spec :: Spec
spec = do
  authSpec
  cookieAuthSpec
  jwtAuthSpec
  throwAllSpec
  basicAuthSpec

------------------------------------------------------------------------------
-- * Auth {{{

authSpec :: Spec
authSpec
  = describe "The Auth combinator"
  $ around (testWithApplication . return $ app jwtAndCookieApi) $ do

  it "returns a 401 if all authentications are Indefinite" $ \port -> do
    get (url port) `shouldHTTPErrorWith` status401

  it "succeeds if one authentication suceeds" $ \port -> property $
                                                \(user :: User) -> do
    jwt <- makeJWT user jwtCfg Nothing
    opts <- addJwtToHeader jwt
    resp <- getWith opts (url port)
    resp ^? responseBody . _JSON `shouldBe` Just (length $ name user)

  it "fails (403) if one authentication fails" $ const $
    pendingWith "Authentications don't yet fail, only are Indefinite"

  it "doesn't clobber pre-existing response headers" $ \port -> property $
                                                \(user :: User) -> do
    jwt <- makeJWT user jwtCfg Nothing
    opts <- addJwtToHeader jwt
    resp <- getWith opts (url port ++ "/header")
    resp ^. responseHeader "Blah" `shouldBe` "1797"
    resp ^. responseHeader "Set-Cookie" `shouldSatisfy` (/= "")

  context "Raw" $ do

    it "gets the response body" $ \port -> property $ \(user :: User) -> do
      jwt <- makeJWT user jwtCfg Nothing
      opts <- addJwtToHeader jwt
      resp <- getWith opts (url port ++ "/raw")
      resp ^. responseBody `shouldBe` "how are you?"

    it "doesn't clobber pre-existing reponse headers" $ \port -> property $
                                                \(user :: User) -> do
      jwt <- makeJWT user jwtCfg Nothing
      opts <- addJwtToHeader jwt
      resp <- getWith opts (url port ++ "/raw")
      resp ^. responseHeader "hi" `shouldBe` "there"
      resp ^. responseHeader "Set-Cookie" `shouldSatisfy` (/= "")


  context "Setting cookies" $ do

    it "sets cookies that it itself accepts" $ \port -> property $ \user -> do
      jwt <- createJWT theKey (newJWSHeader ((), HS256))
        (claims $ toJSON user)
      opts' <- addJwtToCookie cookieCfg jwt
      let opts = addCookie (opts' & header (mk (xsrfField xsrfHeaderName cookieCfg)) .~ ["blah"])
                           (xsrfField xsrfCookieName cookieCfg <> "=blah")
      resp <- getWith opts (url port)
      let (cookieJar:_) = resp ^.. responseCookieJar
          Just xxsrf = find (\x -> cookie_name x == xsrfField xsrfCookieName cookieCfg)
                     $ destroyCookieJar cookieJar
          opts2 = defaults
            & cookies .~ Just cookieJar
            & header (mk (xsrfField xsrfHeaderName cookieCfg)) .~ [cookie_value xxsrf]
      resp2 <- getWith opts2 (url port)
      resp2 ^? responseBody . _JSON `shouldBe` Just (length $ name user)

    it "uses the Expiry from the configuration" $ \port -> property $ \(user :: User) -> do
      jwt <- createJWT theKey (newJWSHeader ((), HS256))
        (claims $ toJSON user)
      opts' <- addJwtToCookie cookieCfg jwt
      let opts = addCookie (opts' & header (mk (xsrfField xsrfHeaderName cookieCfg)) .~ ["blah"])
                           (xsrfField xsrfCookieName cookieCfg <> "=blah")
      resp <- getWith opts (url port)
      let (cookieJar:_) = resp ^.. responseCookieJar
          Just xxsrf = find (\x -> cookie_name x == xsrfField xsrfCookieName cookieCfg)
                     $ destroyCookieJar cookieJar
      xxsrf ^. cookieExpiryTime `shouldBe` future

    it "sets the token cookie as HttpOnly" $ \port -> property $ \(user :: User) -> do
      jwt <- createJWT theKey (newJWSHeader ((), HS256))
        (claims $ toJSON user)
      opts' <- addJwtToCookie cookieCfg jwt
      let opts = addCookie (opts' & header (mk (xsrfField xsrfHeaderName cookieCfg)) .~ ["blah"])
                           (xsrfField xsrfCookieName cookieCfg <> "=blah")
      resp <- getWith opts (url port)
      let (cookieJar:_) = resp ^.. responseCookieJar
          Just token = find (\x -> cookie_name x == sessionCookieName cookieCfg)
                     $ destroyCookieJar cookieJar
      cookie_http_only token `shouldBe` True



-- }}}
------------------------------------------------------------------------------
-- * Cookie Auth {{{

cookieAuthSpec :: Spec
cookieAuthSpec
  = describe "The Auth combinator" $ do
      describe "With XSRF check" $
       around (testWithApplication . return $ app cookieOnlyApi) $ do

        it "fails if XSRF header and cookie don't match" $ \port -> property
                                                         $ \(user :: User) -> do
          jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims $ toJSON user)
          opts' <- addJwtToCookie cookieCfg jwt
          let opts = addCookie (opts' & header (mk (xsrfField xsrfHeaderName cookieCfg)) .~ ["blah"])
                               (xsrfField xsrfCookieName cookieCfg <> "=blerg")
          getWith opts (url port) `shouldHTTPErrorWith` status401

        it "fails with no XSRF header or cookie" $ \port -> property
                                                         $ \(user :: User) -> do
          jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims $ toJSON user)
          opts <- addJwtToCookie cookieCfg jwt
          getWith opts (url port) `shouldHTTPErrorWith` status401

        it "succeeds if XSRF header and cookie match, and JWT is valid" $ \port -> property
                                                                        $ \(user :: User) -> do
          jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims $ toJSON user)
          opts' <- addJwtToCookie cookieCfg jwt
          let opts = addCookie (opts' & header (mk (xsrfField xsrfHeaderName cookieCfg)) .~ ["blah"])
                               (xsrfField xsrfCookieName cookieCfg <> "=blah")
          resp <- getWith opts (url port)
          resp ^? responseBody . _JSON `shouldBe` Just (length $ name user)

        it "sets and clears the right cookies" $ \port -> property
                                               $ \(user :: User) -> do
          let optsFromResp resp =
                let jar = resp ^. responseCookieJar
                    Just xsrfCookieValue = cookie_value <$> find (\c -> cookie_name c == xsrfField xsrfCookieName cookieCfg) (destroyCookieJar jar)
                in defaults
                    & cookies .~ Just jar -- real cookie jars aren't updated by being replaced
                    & header (mk (xsrfField xsrfHeaderName cookieCfg)) .~ [xsrfCookieValue]

          resp <- postWith defaults (url port ++ "/login") user
          (resp ^. responseCookieJar) `shouldMatchCookieNames`
            [ sessionCookieName cookieCfg
            , xsrfField xsrfCookieName cookieCfg
            ]
          let loggedInOpts = optsFromResp resp

          resp <- getWith loggedInOpts (url port)
          resp ^? responseBody . _JSON `shouldBe` Just (length $ name user)

          -- logout
          resp <- getWith loggedInOpts (url port ++ "/logout")

          -- assert cookies were expired
          now <- getCurrentTime
          let assertCookie c = now >= cookie_expiry_time c
          all assertCookie (destroyCookieJar (resp ^. responseCookieJar)) `shouldBe` True

          let loggedOutOpts = optsFromResp resp
          getWith loggedOutOpts (url port) `shouldHTTPErrorWith` status401

      describe "With no XSRF check for GET requests" $ let
            noXsrfGet xsrfCfg = xsrfCfg { xsrfExcludeGet = True }
            cookieCfgNoXsrfGet = cookieCfg { cookieXsrfSetting = fmap noXsrfGet $ cookieXsrfSetting cookieCfg }
       in around (testWithApplication . return $ appWithCookie cookieOnlyApi cookieCfgNoXsrfGet) $ do

        it "succeeds with no XSRF header or cookie for GET" $ \port -> property
                                                            $ \(user :: User) -> do
          jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims $ toJSON user)
          opts <- addJwtToCookie cookieCfgNoXsrfGet jwt
          resp <- getWith opts (url port)
          resp ^? responseBody . _JSON `shouldBe` Just (length $ name user)

        it "fails with no XSRF header or cookie for POST" $ \port -> property
                                                          $ \(user :: User) number -> do
          jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims $ toJSON user)
          opts <- addJwtToCookie cookieCfgNoXsrfGet jwt
          postWith opts (url port) (toJSON (number :: Int)) `shouldHTTPErrorWith` status401

      describe "With no XSRF check at all" $ let
            cookieCfgNoXsrf = cookieCfg { cookieXsrfSetting = Nothing }
       in around (testWithApplication . return $ appWithCookie cookieOnlyApi cookieCfgNoXsrf) $ do

        it "succeeds with no XSRF header or cookie for GET" $ \port -> property
                                                            $ \(user :: User) -> do
          jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims $ toJSON user)
          opts <- addJwtToCookie cookieCfgNoXsrf jwt
          resp <- getWith opts (url port)
          resp ^? responseBody . _JSON `shouldBe` Just (length $ name user)

        it "succeeds with no XSRF header or cookie for POST" $ \port -> property
                                                             $ \(user :: User) number -> do
          jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims $ toJSON user)
          opts <- addJwtToCookie cookieCfgNoXsrf jwt
          resp <- postWith opts (url port) $ toJSON (number :: Int)
          resp ^? responseBody . _JSON `shouldBe` Just number

        it "sets and clears the right cookies" $ \port -> property
                                               $ \(user :: User) -> do
          let optsFromResp resp = defaults
                & cookies .~ Just (resp ^. responseCookieJar) -- real cookie jars aren't updated by being replaced

          resp <- postWith defaults (url port ++ "/login") user
          (resp ^. responseCookieJar) `shouldMatchCookieNames`
            [ sessionCookieName cookieCfg
            , "NO-XSRF-TOKEN"
            ]
          let loggedInOpts = optsFromResp resp

          resp <- getWith (loggedInOpts) (url port)
          resp ^? responseBody . _JSON `shouldBe` Just (length $ name user)

          resp <- getWith loggedInOpts (url port ++ "/logout")
          (resp ^. responseCookieJar) `shouldMatchCookieNameValues`
            [ (sessionCookieName cookieCfg, "value")
            , ("NO-XSRF-TOKEN", "")
            ]

          -- assert cookies were expired
          now <- getCurrentTime
          let assertCookie c = now >= cookie_expiry_time c
          all assertCookie (destroyCookieJar (resp ^. responseCookieJar)) `shouldBe` True

          let loggedOutOpts = optsFromResp resp

          getWith loggedOutOpts (url port) `shouldHTTPErrorWith` status401

-- }}}
------------------------------------------------------------------------------
-- * JWT Auth {{{

jwtAuthSpec :: Spec
jwtAuthSpec
  = describe "The JWT combinator"
  $ around (testWithApplication . return $ app jwtOnlyApi) $ do

  it "fails if 'aud' does not match predicate" $ \port -> property $
                                                \(user :: User) -> do
    jwt <- createJWT theKey (newJWSHeader ((), HS256))
      (claims (toJSON user) & claimAud .~ Just (Audience ["boo"]))
    opts <- addJwtToHeader (jwt >>= (return . encodeCompact))
    getWith opts (url port) `shouldHTTPErrorWith` status401

  it "succeeds if 'aud' does match predicate" $ \port -> property $
                                                \(user :: User) -> do
    jwt <- createJWT theKey (newJWSHeader ((), HS256))
      (claims (toJSON user) & claimAud .~ Just (Audience ["anythingElse"]))
    opts <- addJwtToHeader (jwt >>= (return . encodeCompact))
    resp <- getWith opts (url port)
    resp ^. responseStatus `shouldBe` status200

  it "fails if 'nbf' is set to a future date" $ \port -> property $
                                                \(user :: User) -> do
    jwt <- createJWT theKey (newJWSHeader ((), HS256))
      (claims (toJSON user) & claimNbf .~ Just (NumericDate future))
    opts <- addJwtToHeader (jwt >>= (return . encodeCompact))
    getWith opts (url port) `shouldHTTPErrorWith` status401

  it "fails if 'exp' is set to a past date" $ \port -> property $
                                              \(user :: User) -> do
    jwt <- makeJWT user jwtCfg (Just past)
    opts <- addJwtToHeader jwt
    getWith opts (url port) `shouldHTTPErrorWith` status401

  it "succeeds if 'exp' is set to a future date" $ \port -> property $
                                                   \(user :: User) -> do
    jwt <- makeJWT user jwtCfg (Just future)
    opts <- addJwtToHeader jwt
    resp <- getWith opts (url port)
    resp ^. responseStatus `shouldBe` status200

  it "fails if JWT is not signed" $ \port -> property $ \(user :: User) -> do
    jwt <- createJWT theKey (newJWSHeader ((), None))
                               (claims $ toJSON user)
    opts <- addJwtToHeader (jwt >>= (return . encodeCompact))
    getWith opts (url port) `shouldHTTPErrorWith` status401

  it "fails if JWT does not use expected algorithm" $ const $
    pendingWith "Need https://github.com/frasertweedale/hs-jose/issues/19"

  it "fails if data is not valid JSON" $ \port -> do
    jwt <- createJWT theKey (newJWSHeader ((), HS256)) (claims "{{")
    opts <- addJwtToHeader (jwt >>= (return .encodeCompact))
    getWith opts (url port) `shouldHTTPErrorWith` status401

  it "suceeds as wreq's oauth2Bearer" $ \port -> property $ \(user :: User) -> do
    jwt <- createJWT theKey (newJWSHeader ((), HS256))
                               (claims $ toJSON user)
    resp <- case jwt >>= (return . encodeCompact) of
      Left (e :: Error) -> fail $ show e
      Right v -> getWith (defaults & auth ?~ oauth2Bearer (BSL.toStrict v)) (url port)
    resp ^. responseStatus `shouldBe` status200

-- }}}
------------------------------------------------------------------------------
-- * Basic Auth {{{

basicAuthSpec :: Spec
basicAuthSpec = describe "The BasicAuth combinator"
  $ around (testWithApplication . return $ app basicAuthApi) $ do

  it "succeeds with the correct password and username" $ \port -> do
    resp <- getWith (defaults & auth ?~ basicAuth "ali" "Open sesame") (url port)
    resp ^. responseStatus `shouldBe` status200

  it "fails with non-existent user" $ \port -> do
    getWith (defaults & auth ?~ basicAuth "thief" "Open sesame") (url port)
      `shouldHTTPErrorWith` status401

  it "fails with incorrect password" $ \port -> do
    getWith (defaults & auth ?~ basicAuth "ali" "phatic") (url port)
      `shouldHTTPErrorWith` status401

  it "fails with no auth header" $ \port -> do
    get (url port) `shouldHTTPErrorWith` status401

-- }}}
------------------------------------------------------------------------------
-- * ThrowAll {{{

throwAllSpec :: Spec
throwAllSpec = describe "throwAll" $ do

  it "works for plain values" $ do
    let t :: Either ServerError Int :<|> Either ServerError Bool :<|> Either ServerError String
        t = throwAll err401
    t `shouldBe` throwError err401 :<|> throwError err401 :<|> throwError err401

  it "works for function types" $ property $ \i -> do
    let t :: Int -> (Either ServerError Bool :<|> Either ServerError String)
        t = throwAll err401
        expected _ = throwError err401 :<|> throwError err401
    t i `shouldBe` expected i

-- }}}
------------------------------------------------------------------------------
-- * API and Server {{{

type API auths
    = Auth auths User :>
        ( Get '[JSON] Int
       :<|> ReqBody '[JSON] Int :> Post '[JSON] Int
       :<|> "header" :> Get '[JSON] (Headers '[Header "Blah" Int] Int)
#if MIN_VERSION_servant_server(0,15,0)
       :<|> "stream" :> StreamGet NoFraming OctetStream (SourceIO BS.ByteString)
#endif
       :<|> "raw" :> Raw
        )
      :<|> "login" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                     , Header "Set-Cookie" SetCookie ] NoContent)
      :<|> "logout" :> Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                             , Header "Set-Cookie" SetCookie ] NoContent)

jwtOnlyApi :: Proxy (API '[Servant.Auth.Server.JWT])
jwtOnlyApi = Proxy

cookieOnlyApi :: Proxy (API '[Cookie])
cookieOnlyApi = Proxy

basicAuthApi :: Proxy (API '[BasicAuth])
basicAuthApi = Proxy

jwtAndCookieApi :: Proxy (API '[Servant.Auth.Server.JWT, Cookie])
jwtAndCookieApi = Proxy

theKey :: JWK
theKey = unsafePerformIO . genJWK $ OctGenParam 256
{-# NOINLINE theKey #-}


cookieCfg :: CookieSettings
cookieCfg = def
  { cookieExpires = Just future
  , cookieIsSecure = NotSecure
  , sessionCookieName = "RuncibleSpoon"
  , cookieXsrfSetting = pure $ def
    { xsrfCookieName = "TheyDinedOnMince"
    , xsrfHeaderName = "AndSlicesOfQuince"
    }
  }
xsrfField :: (XsrfCookieSettings -> a) -> CookieSettings -> a
xsrfField f = maybe (error "expected XsrfCookieSettings for test") f . cookieXsrfSetting

jwtCfg :: JWTSettings
jwtCfg = (defaultJWTSettings theKey) { audienceMatches = \x ->
    if x == "boo" then DoesNotMatch else Matches }

instance FromBasicAuthData User where
  fromBasicAuthData (BasicAuthData usr pwd) _
    = return $ if usr == "ali" && pwd == "Open sesame"
      then Authenticated $ User "ali" "ali@the-thieves-den.com"
      else Indefinite

-- Could be anything, really, but since this is already in the cfg we don't
-- have to add it
type instance BasicAuthCfg = JWK

appWithCookie :: AreAuths auths '[CookieSettings, JWTSettings, JWK] User
  => Proxy (API auths) -> CookieSettings -> Application
appWithCookie api ccfg = serveWithContext api ctx $ server ccfg
  where
    ctx = ccfg :. jwtCfg :. theKey :. EmptyContext

-- | Takes a proxy parameter indicating which authentication systems to enable.
app :: AreAuths auths '[CookieSettings, JWTSettings, JWK] User
  => Proxy (API auths) -> Application
app api = appWithCookie api cookieCfg

server :: CookieSettings -> Server (API auths)
server ccfg =
    (\authResult -> case authResult of
        Authenticated usr -> getInt usr
                        :<|> postInt usr
                        :<|> getHeaderInt
#if MIN_VERSION_servant_server(0,15,0)
                        :<|> return (S.source ["bytestring"])
#endif
                        :<|> raw
        Indefinite -> throwAll err401
        _ -> throwAll err403
    )
    :<|> getLogin
    :<|> getLogout
  where
    getInt :: User -> Handler Int
    getInt usr = return . length $ name usr

    postInt :: User -> Int -> Handler Int
    postInt _ = return

    getHeaderInt :: Handler (Headers '[Header "Blah" Int] Int)
    getHeaderInt = return $ addHeader 1797 17

    getLogin :: User -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                          , Header "Set-Cookie" SetCookie ] NoContent)
    getLogin user = do
        maybeApplyCookies <- liftIO $ acceptLogin ccfg jwtCfg user
        case maybeApplyCookies of
          Just applyCookies -> return $ applyCookies NoContent
          Nothing -> error "cookies failed to apply"

    getLogout :: Handler (Headers '[ Header "Set-Cookie" SetCookie
                                   , Header "Set-Cookie" SetCookie ] NoContent)
    getLogout = return $ clearSession ccfg NoContent

    raw :: Server Raw
    raw =
#if MIN_VERSION_servant_server(0,11,0)
      Tagged $
#endif
      \_req respond ->
        respond $ responseLBS status200 [("hi", "there")] "how are you?"

-- }}}
------------------------------------------------------------------------------
-- * Utils {{{

past :: UTCTime
past = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "1970-01-01"

future :: UTCTime
future = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2070-01-01"

addJwtToHeader :: Either Error BSL.ByteString -> IO Options
addJwtToHeader jwt = case jwt of
  Left e -> fail $ show e
  Right v -> return
    $ defaults & header "Authorization" .~ ["Bearer " <> BSL.toStrict v]

createJWT :: JWK -> JWSHeader () -> ClaimsSet -> IO (Either Error Crypto.JWT.SignedJWT)
createJWT k a b = runExceptT $ signClaims k a b

addJwtToCookie :: ToCompact a => CookieSettings -> Either Error a -> IO Options
addJwtToCookie ccfg jwt = case jwt >>= (return . encodeCompact) of
  Left e -> fail $ show e
  Right v -> return
    $ defaults & header "Cookie" .~ [sessionCookieName ccfg <> "=" <> BSL.toStrict v]

addCookie :: Options -> BS.ByteString -> Options
addCookie opts cookie' = opts & header "Cookie" %~ \c -> case c of
                        [h] -> [cookie' <> "; " <> h]
                        []  -> [cookie']
                        _   -> error "expecting single cookie header"


shouldHTTPErrorWith :: IO a -> Status -> Expectation
shouldHTTPErrorWith act stat = act `shouldThrow` \e -> case e of
#if MIN_VERSION_http_client(0,5,0)
  HCli.HttpExceptionRequest _ (HCli.StatusCodeException resp _)
    -> HCli.responseStatus resp == stat
#else
  HCli.StatusCodeException x _ _ -> x == stat
#endif
  _ -> False

shouldMatchCookieNames :: HCli.CookieJar -> [BS.ByteString] -> Expectation
shouldMatchCookieNames cj patterns
    = fmap cookie_name (destroyCookieJar cj)
    `shouldMatchList` patterns

shouldMatchCookieNameValues :: HCli.CookieJar -> [(BS.ByteString, BS.ByteString)] -> Expectation
shouldMatchCookieNameValues cj patterns
    = fmap ((,) <$> cookie_name <*> cookie_value) (destroyCookieJar cj)
    `shouldMatchList` patterns

url :: Int -> String
url port = "http://localhost:" <> show port

claims :: Value -> ClaimsSet
claims val = emptyClaimsSet & unregisteredClaims . at "dat" .~ Just val
-- }}}
------------------------------------------------------------------------------
-- * Types {{{

data User = User
  { name :: String
  , _id  :: String
  } deriving (Eq, Show, Read, Generic)

instance FromJWT User
instance ToJWT User
instance FromJSON User
instance ToJSON User

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary

instance Postable User where
  postPayload user request = return $ request
    { HCli.requestBody = HCli.RequestBodyLBS $ encode user
    , HCli.requestHeaders = (mk "Content-Type", "application/json") : HCli.requestHeaders request
    }


-- }}}
