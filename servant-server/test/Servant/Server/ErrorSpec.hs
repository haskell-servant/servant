{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Servant.Server.ErrorSpec (spec) where

import Test.Hspec
import Data.Proxy
import Test.Hspec.Wai (request, with, shouldRespondWith)
import Network.HTTP.Types (methodGet, methodPost)
import Data.Aeson (encode)

import Servant


-- 1) Check whether one or more endpoints have the right path. Otherwise return 404.
-- 2) Check whether the one of those have the right method. Otherwise return
-- 405. If so, pick the first. We've now commited to calling at most one handler. *
-- 3) Check whether the Content-Type is known. Otherwise return 415.
-- 4) Check whether that one deserializes the body. Otherwise return 400. If there
-- was no Content-Type, try the first one of the API content-type list.
-- 5) Check whether the request is authorized. Otherwise return a 401.
-- 6) Check whether the request is forbidden. If so return 403.
-- 7) Check whether the request has a known Accept. Otherwise return 406.
-- 8) Check whether Accept-Language, Accept-Charset and Accept-Encoding exist and
-- match. We can follow the webmachine order here.
-- 9) Call the handler. Whatever it returns, we return.

spec :: Spec
spec = do
    errorOrder


type ErrorOrderApi = "home"
                  :> ReqBody '[JSON] Int
                  :> Capture "t" Int
                  :> Post '[JSON] Int

errorOrderApi :: Proxy ErrorOrderApi
errorOrderApi = Proxy

errorOrderServer :: Server ErrorOrderApi
errorOrderServer = \_ _ -> return 10

errorOrder :: Spec
errorOrder = describe "HTTP error order"
           $ with (return $ serve errorOrderApi errorOrderServer) $ do
    let badContentType  = ("Content-Type", "text/plain")
        badAccept       = ("Accept", "text/plain")
        badMethod       = methodGet
        badUrl          = "home/nonexistent"
        badBody         = "nonsense"
        goodContentType = ("Content-Type", "application/json")
        goodAccept      = ("Accept", "application/json")
        goodMethod      = methodPost
        goodUrl         = "home/5"
        goodBody        = encode (5 :: Int)

    it "has 404 as its highest priority error" $ do
        request badMethod badUrl [badContentType, badAccept] badBody
            `shouldRespondWith` 404

    it "has 405 as its second highest priority error" $ do
        request badMethod goodUrl [badContentType, badAccept] badBody
            `shouldRespondWith` 405

    it "has 415 as its third highest priority error" $ do
        request goodMethod goodUrl [badContentType, badAccept] badBody
            `shouldRespondWith` 415

    it "has 400 as its fourth highest priority error" $ do
        request goodMethod goodUrl [goodContentType, badAccept] badBody
            `shouldRespondWith` 400

    it "has 406 as its fifth highest priority error" $ do
        request goodMethod goodUrl [goodContentType, badAccept] goodBody
            `shouldRespondWith` 406


