{-# LANGUAGE FlexibleContexts, DeriveFunctor, GeneralizedNewtypeDeriving, DataKinds, TypeOperators, OverloadedStrings #-}
{-# OPTIONS_GHC -Werror #-}
module Servant.Client.Core.Internal.HasClientSpec (spec) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Free (Free (..), liftF)
import Servant.API
import Servant.Client.Core
import Data.Text (Text)
import Data.Proxy (Proxy (..))
import Servant.API.Internal.Test.ComprehensiveAPI
import Test.Hspec

import qualified Data.ByteString.Builder as B

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPI

spec :: Spec
spec = describe "Servant.Client" $ do
    issue781trailingSlashSpec

type Issue781 = Get '[JSON] Text
    :<|> "foo" :> "bar" :> Get '[JSON] Text 

issue781 :: Proxy Issue781
issue781 = Proxy

issue781trailingSlashSpec :: Spec
issue781trailingSlashSpec = describe "issue 781: trailing slash in baseurl" $ do
    it "Empty request has / as a path" $ do
        B.toLazyByteString . requestPath <$> matchSingleRequest emptyCli
            `shouldBe` Just "/"

    it "Path components are intercalated with /" $ do
        B.toLazyByteString . requestPath <$> matchSingleRequest concatCli
            `shouldBe` Just "/foo/bar"
  where
    emptyCli :<|> concatCli = client issue781

-------------------------------------------------------------------------------
-- Client
-------------------------------------------------------------------------------

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

data ClientF a
    = SingleRequest Request (Response -> a)
    | StreamingRequest Request (StreamingResponse -> a)
    | Throw ServantError
  deriving (Functor)

newtype ClientM a = ClientM { unClientM :: Free ClientF a }
  deriving (Functor, Applicative, Monad)

-- | Extract 'Request' from first 'SingleRequest'
matchSingleRequest :: ClientM a -> Maybe Request
matchSingleRequest (ClientM (Free (SingleRequest req _))) = Just req
matchSingleRequest _ = Nothing

instance RunClient ClientM where
    runRequest req        = ClientM $ liftF $ SingleRequest req id
    streamingRequest req  = ClientM $ liftF $ StreamingRequest req id
    throwServantError err = ClientM $ liftF $ Throw err

    -- catch is not algebraic
    catchServantError x' handler = ClientM (go (unClientM x')) where
        go x@(Pure _)         = x
        go (Free (Throw err)) = unClientM (handler err)
        go (Free f')          = Free (fmap go f')
