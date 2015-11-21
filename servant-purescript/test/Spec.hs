{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Monad
import           Data.Either (isRight)
import           Data.Proxy
import           Data.String.Conversions
import qualified Language.PureScript as P
import           Servant.API
import           Servant.Foreign as F
import           Servant.JS
import           Servant.PureScript
import           Test.Hspec
import qualified Text.Parsec as TP


type TestAPI = "simple" :> ReqBody '[JSON] String :> Post '[JSON] Bool
          :<|> "has.extension" :> Get '[JSON] Bool

type TopLevelRawAPI = "rwa" :> Get '[JSON] Int
                  :<|> Raw

type HeaderHandlingAPI = "oiy" :> Header "Foo" String :> Get '[JSON] Int

type QueryHandlingAPI = "urq" :> QueryParam "Bar" String :> Get '[JSON] Int

-- | 'Raw' has 'Foreign', but its type is @Method -> Req@, which doesn't have a 'GenerateList'
-- instance.  Since @Foreign.Method@ is not exported, we need to keep it polymorphic.
instance {-# OVERLAPPABLE #-} GenerateList (a -> F.Req) where
    generateList _ = []

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "generateJS" $ do
        it "should generate valid purescript" $ do
            let m = generatePSModule defaultSettings "Main" (Proxy :: Proxy TestAPI)
            shouldParse m
        it "should use non-empty function names" $ do
            let m = generatePSModule defaultSettings "Main" (Proxy :: Proxy TopLevelRawAPI)
            shouldParse m
            (cs m :: String) `shouldContain` "getRwa :: "
        it "should generate valid header variables" $ do
            let m = generatePSModule defaultSettings "Main" (Proxy :: Proxy HeaderHandlingAPI)
            shouldParse m
            (cs m :: String) `shouldContain` "String"
            (cs m :: String) `shouldContain` "headerFoo"
            (cs m :: String) `shouldContain` "RequestHeader \"foo\" headerFoo"
        it "should generate valid query params" $ do
            let m = generatePSModule defaultSettings "Main" (Proxy :: Proxy QueryHandlingAPI)
            shouldParse m


shouldParse :: ST -> Expectation
shouldParse =
    (`shouldSatisfy` isRight) . (TP.runParser P.parseModule (P.ParseState 0) "" <=< P.lex "") . cs
