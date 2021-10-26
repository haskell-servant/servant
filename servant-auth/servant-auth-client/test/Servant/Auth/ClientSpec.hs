{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
module Servant.Auth.ClientSpec (spec) where

import           Crypto.JOSE              (JWK,
                                           KeyMaterialGenParam (OctGenParam),
                                           genJWK)
import           Data.Aeson               (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Lazy     as BSL
import           Data.Time                (UTCTime, defaultTimeLocale,
                                           parseTimeOrError)
import           GHC.Generics             (Generic)
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)
import           Network.HTTP.Types       (status401)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Servant
import           Servant.Client           (BaseUrl (..), Scheme (Http),
                                           ClientError (FailureResponse),
#if MIN_VERSION_servant_client(0,16,0)
                                           ResponseF(..),
#elif MIN_VERSION_servant_client(0,13,0)
                                           GenResponse(..),
#elif MIN_VERSION_servant_client(0,12,0)
                                           Response(..),
#endif
                                           client)
import           System.IO.Unsafe         (unsafePerformIO)
import           Test.Hspec
import           Test.QuickCheck

#if MIN_VERSION_servant_client(0,13,0)
import Servant.Client (mkClientEnv, runClientM)
#elif MIN_VERSION_servant_client(0,9,0)
import Servant.Client (ClientEnv (..), runClientM)
#else
import Control.Monad.Trans.Except (runExceptT)
#endif
#if !MIN_VERSION_servant_server(0,16,0)
#define ClientError ServantError
#endif

import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

spec :: Spec
spec = describe "The JWT combinator" $ do
  hasClientSpec


------------------------------------------------------------------------------
-- * HasClient {{{

hasClientSpec :: Spec
hasClientSpec = describe "HasClient" $ around (testWithApplication $ return app) $ do

  let mkTok :: User -> Maybe UTCTime -> IO Token
      mkTok user mexp = do
        Right tok <- makeJWT user jwtCfg mexp
        return $ Token $ BSL.toStrict tok

  it "succeeds when the token does not have expiry" $ \port -> property $ \user -> do
    tok <- mkTok user Nothing
    v <- getIntClient tok mgr (BaseUrl Http "localhost" port "")
    v `shouldBe` Right (length $ name user)

  it "succeeds when the token is not expired" $ \port -> property $ \user -> do
    tok <- mkTok user (Just future)
    v <- getIntClient tok mgr (BaseUrl Http "localhost" port "")
    v `shouldBe` Right (length $ name user)

  it "fails when token is expired" $ \port -> property $ \user -> do
    tok <- mkTok user (Just past)
#if MIN_VERSION_servant_client(0,16,0)
    Left (FailureResponse _ (Response stat _ _ _))
#elif MIN_VERSION_servant_client(0,12,0)
    Left (FailureResponse (Response stat _ _ _))
#elif MIN_VERSION_servant_client(0,11,0)
    Left (FailureResponse _ stat _ _)
#else
    Left (FailureResponse stat _ _)
#endif
      <- getIntClient tok mgr (BaseUrl Http "localhost" port "")
    stat `shouldBe` status401


getIntClient :: Token -> Manager -> BaseUrl -> IO (Either ClientError Int)
#if MIN_VERSION_servant(0,13,0)
getIntClient tok m burl = runClientM (client api tok) (mkClientEnv m burl)
#elif MIN_VERSION_servant(0,9,0)
getIntClient tok m burl = runClientM (client api tok) (ClientEnv m burl)
#else
getIntClient tok m burl = runExceptT $ client api tok m burl
#endif
-- }}}
------------------------------------------------------------------------------
-- * API and Server {{{

type API = Auth '[JWT] User :> Get '[JSON] Int

api :: Proxy API
api = Proxy

theKey :: JWK
theKey = unsafePerformIO . genJWK $ OctGenParam 256
{-# NOINLINE theKey #-}

mgr :: Manager
mgr = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE mgr #-}

app :: Application
app = serveWithContext api ctx server
  where
    ctx = cookieCfg :. jwtCfg :. EmptyContext

jwtCfg :: JWTSettings
jwtCfg = defaultJWTSettings theKey

cookieCfg :: CookieSettings
cookieCfg = defaultCookieSettings


server :: Server API
server = getInt
  where
    getInt :: AuthResult User -> Handler Int
    getInt (Authenticated u) = return . length $ name  u
    getInt _ = throwAll err401


-- }}}
------------------------------------------------------------------------------
-- * Utils {{{

past :: UTCTime
past = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "1970-01-01"

future :: UTCTime
future = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2070-01-01"


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

-- }}}
