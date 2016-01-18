{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.MockSpec where

import           Data.Aeson as Aeson
import           Data.ByteString.Conversion.To
import           Data.Proxy
import           Data.String
import           GHC.Generics
import           Network.Wai
import           Servant.API
import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai
import           Test.QuickCheck

import           Servant
import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Mock

-- This declaration simply checks that all instances are in place.
_ = mock comprehensiveAPI (Proxy :: Proxy '[NamedConfig "foo" '[]])

data Body
  = Body
  | ArbitraryBody
  deriving (Generic)

instance ToJSON Body

instance Arbitrary Body where
  arbitrary = return ArbitraryBody

data TestHeader
  = TestHeader
  | ArbitraryHeader
  deriving (Show)

instance ToByteString TestHeader where
  builder = fromString . show

instance Arbitrary TestHeader where
  arbitrary = return ArbitraryHeader

spec :: Spec
spec = do
  describe "mock" $ do
    context "Get" $ do
      let api :: Proxy (Get '[JSON] Body)
          api = Proxy
          app = serve api EmptyConfig (mock api Proxy)
      with (return app) $ do
        it "serves arbitrary response bodies" $ do
          get "/" `shouldRespondWith` 200{
            matchBody = Just $ Aeson.encode ArbitraryBody
          }

    context "response headers" $ do
      let withHeader :: Proxy (Get '[JSON] (Headers '[Header "foo" TestHeader] Body))
          withHeader = Proxy
          withoutHeader :: Proxy (Get '[JSON] (Headers '[] Body))
          withoutHeader = Proxy
          toApp :: (HasMock api '[]) => Proxy api -> IO Application
          toApp api = return $ serve api EmptyConfig (mock api (Proxy :: Proxy '[]))
      with (toApp withHeader) $ do
        it "serves arbitrary response bodies" $ do
          get "/" `shouldRespondWith` 200{
            matchHeaders = return $ MatchHeader $ \ h ->
             if h == [("Content-Type", "application/json"), ("foo", "ArbitraryHeader")]
                then Nothing
                else Just ("headers not correct\n")
          }

      with (toApp withoutHeader) $ do
        it "works for no additional headers" $ do
          get "/" `shouldRespondWith` 200{
            matchHeaders = return $ MatchHeader $ \ h ->
             if h == [("Content-Type", "application/json")]
                then Nothing
                else Just ("headers not correct\n")
          }
