{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Server.ContentTypesSpec where

import Control.Applicative
import Data.Aeson (encode)
import Data.ByteString.Char8
import Data.Function (on)
import Data.Maybe (isJust, fromJust)
import Data.List (maximumBy)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Data.String.Conversions (cs)
import Network.HTTP.Types (hAccept)
import Network.Wai (pathInfo, requestHeaders)
import Network.Wai.Test ( runSession, request, defaultRequest
                        , assertContentType, assertStatus )
import Test.Hspec
import Test.QuickCheck

import Servant.API
import Servant.Server
import Servant.Server.ContentTypes


spec :: Spec
spec = describe "Servant.Server.ContentTypes" $ do
    handleAcceptHSpec
    contentTypeSpec

handleAcceptHSpec :: Spec
handleAcceptHSpec = describe "handleAcceptH" $ do

    it "should return Just if the 'Accept' header matches" $ do
        handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (3 :: Int)
            `shouldSatisfy` isJust
        handleAcceptH (Proxy :: Proxy '[XML, JSON]) "application/json" (3 :: Int)
            `shouldSatisfy` isJust
        handleAcceptH (Proxy :: Proxy '[XML, JSON, HTML]) "text/html" (3 :: Int)
            `shouldSatisfy` isJust

    it "should return the Content-Type as the first element of the tuple" $ do
        handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (3 :: Int)
            `shouldSatisfy` ((== "application/json") . fst . fromJust)
        handleAcceptH (Proxy :: Proxy '[XML, JSON]) "application/json" (3 :: Int)
            `shouldSatisfy` ((== "application/json") . fst . fromJust)
        handleAcceptH (Proxy :: Proxy '[XML, JSON, HTML]) "text/html" (3 :: Int)
            `shouldSatisfy` ((== "text/html") . fst . fromJust)

    it "should return the appropriately serialized representation" $ do
        property $ \x -> handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (x :: Int)
            == Just ("application/json", encode x)

    it "respects the Accept spec ordering" $
        property $ \a b c i -> fst (fromJust $ val a b c i) == (fst $ highest a b c)
          where
            highest a b c = maximumBy (compare `on` snd) [ ("text/html", a)
                                                      , ("application/json", b)
                                                      , ("application/xml", c)
                                                      ]
            acceptH a b c = addToAccept (Proxy :: Proxy HTML) a $
                            addToAccept (Proxy :: Proxy JSON) b $
                            addToAccept (Proxy :: Proxy XML ) c ""
            val a b c i = handleAcceptH (Proxy :: Proxy '[HTML, JSON, XML])
                                        (acceptH a b c) (i :: Int)

type ContentTypeApi = "foo" :> Get '[JSON] Int
                 :<|> "bar" :> Get '[JSON, PlainText] Int

contentTypeApi :: Proxy ContentTypeApi
contentTypeApi = Proxy

contentTypeServer :: Server ContentTypeApi
contentTypeServer = return 5 :<|> return 3

contentTypeSpec :: Spec
contentTypeSpec = do
    describe "Accept Headers" $ do

        it "uses the highest quality possible in the header" $
            flip runSession (serve contentTypeApi contentTypeServer) $ do
                let acceptH = "text/plain; q=0.9, application/json; q=0.8"
                response <- Network.Wai.Test.request defaultRequest{
                    requestHeaders = [(hAccept, acceptH)] ,
                    pathInfo = ["bar"]
                }
                assertContentType "text/plain" response

        it "returns the first content-type if the Accept header is missing" $
            flip runSession (serve contentTypeApi contentTypeServer) $ do
                response <- Network.Wai.Test.request defaultRequest{
                    pathInfo = ["bar"]
                }
                assertContentType "application/json" response

        it "returns 406 if it can't serve the requested content-type" $
            flip runSession (serve contentTypeApi contentTypeServer) $ do
                let acceptH = "text/css"
                response <- Network.Wai.Test.request defaultRequest{
                    requestHeaders = [(hAccept, acceptH)] ,
                    pathInfo = ["bar"]
                }
                assertStatus 406 response


instance Show a => MimeRender HTML a where
    toByteString _ = cs . show

instance Show a => MimeRender XML a where
    toByteString _ = cs . show

instance IsString AcceptHeader where
    fromString = AcceptHeader . fromString

addToAccept :: Accept a => Proxy a -> ZeroToOne -> AcceptHeader -> AcceptHeader
addToAccept p (ZeroToOne f) (AcceptHeader h) = AcceptHeader (cont h)
    where new = cs (show $ contentType p) `append` "; q=" `append` pack (show f)
          cont "" = new
          cont old = old `append` ", " `append` new

newtype ZeroToOne = ZeroToOne Float
    deriving (Eq, Show, Ord)

instance Arbitrary ZeroToOne where
    arbitrary = ZeroToOne <$> elements [ x / 10 | x <- [1..10]]
