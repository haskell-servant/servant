{-# LANGUAGE CPP #-}
module Servant.Auth.SwaggerSpec (spec) where

import Control.Lens
import Data.Proxy
import Servant.API
import Servant.Auth
import Servant.Auth.Swagger
import Data.Swagger
import Servant.Swagger
import Test.Hspec

spec :: Spec
spec = describe "HasSwagger instance" $ do

  let swag = toSwagger (Proxy :: Proxy API)

  it "adds security definitions at the top level" $ do
#if MIN_VERSION_swagger2(2,6,0)
    let (SecurityDefinitions secDefs) = swag ^. securityDefinitions
#else
    let secDefs = swag ^. securityDefinitions
#endif
    length secDefs `shouldSatisfy` (> 0)

  it "adds security at sub-apis" $ do
    swag ^. security `shouldBe` []
    show (swag ^. paths . at "/secure") `shouldContain` "JwtSecurity"
    show (swag ^. paths . at "/insecure") `shouldNotContain` "JwtSecurity"

-- * API

type API =   "secure" :> Auth '[JWT, Cookie] Int :> SecureAPI
        :<|> "insecure" :> InsecureAPI

type SecureAPI = Get '[JSON] Int :<|> ReqBody '[JSON] Int :> Post '[JSON] Int

type InsecureAPI = SecureAPI
