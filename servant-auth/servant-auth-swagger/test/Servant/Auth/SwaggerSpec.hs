{-# LANGUAGE CPP #-}

module Servant.Auth.SwaggerSpec (spec) where

import Control.Lens
import Data.Proxy
import Data.Swagger
import Servant.API
import Servant.Auth
import Servant.Swagger
import Test.Hspec

import Servant.Auth.Swagger ()

spec :: Spec
spec = describe "HasSwagger instance" $ do
  let swag = toSwagger (Proxy :: Proxy API)
      secDefs = unSecDefs $ swag ^. securityDefinitions

  it "adds security definitions at the top level" $ do
    length secDefs `shouldSatisfy` (> 0)

  it "adds security at sub-apis" $ do
    swag ^. security `shouldBe` []
    show (swag ^. paths . at "/secure") `shouldContain` "JwtSecurity"
    show (swag ^. paths . at "/insecure") `shouldNotContain` "JwtSecurity"
  where
#if MIN_VERSION_swagger2(2,6,0)
    unSecDefs (SecurityDefinitions defs) = defs
#else
    unSecDefs = id
#endif

-- * API

type API =
  "secure" :> Auth '[JWT, Cookie] Int :> SecureAPI
    :<|> "insecure" :> InsecureAPI

type SecureAPI = Get '[JSON] Int :<|> ReqBody '[JSON] Int :> Post '[JSON] Int

type InsecureAPI = SecureAPI
