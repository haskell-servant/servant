{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Server.ErrorSpec (spec) where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Char8      as BC
import           Data.Proxy
import           Network.HTTP.Types         (hAccept, hContentType, methodGet,
                                             methodPost, methodPut)
import           Test.Hspec
import           Test.Hspec.Wai

import           Servant


-- The semantics of routing and handling requests should be as follows:
--
--    1) Check whether one or more endpoints have the right path. Otherwise
--    return 404.
--    2) Check whether the one of those have the right method. Otherwise return
--    405. If so, pick the first. We've now committed to calling at most one
--    handler.
--    3) Check whether the Content-Type is known. Otherwise return 415.
--    4) Check whether that one deserializes the body. Otherwise return 400. If
--    there was no Content-Type, try the first one of the API content-type list.
--    5) Check whether the request is authorized. Otherwise return a 401.
--    6) Check whether the request is forbidden. If so return 403.
--    7) Check whether the request has a known Accept. Otherwise return 406.
--    8) Check whether Accept-Language, Accept-Charset and Accept-Encoding
--    exist and match. We can follow the webmachine order here.
--    9) Call the handler. Whatever it returns, we return.

spec :: Spec
spec = describe "HTTP Errors" $ do
    errorOrderSpec
    prioErrorsSpec
    errorRetrySpec
    errorChoiceSpec

------------------------------------------------------------------------------
-- * Error Order {{{

type ErrorOrderApi = "home"
                  :> ReqBody '[JSON] Int
                  :> Capture "t" Int
                  :> Post '[JSON] Int


errorOrderApi :: Proxy ErrorOrderApi
errorOrderApi = Proxy

errorOrderServer :: Server ErrorOrderApi
errorOrderServer = \_ _ -> return 5

errorOrderSpec :: Spec
errorOrderSpec = describe "HTTP error order"
           $ with (return $ serve errorOrderApi errorOrderServer) $ do
  let badContentType  = (hContentType, "text/plain")
      badAccept       = (hAccept, "text/plain")
      badMethod       = methodGet
      badUrl          = "home/nonexistent"
      badBody         = "nonsense"
      goodContentType = (hContentType, "application/json")
      goodMethod      = methodPost
      goodUrl         = "home/2"
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

type PrioErrorsApi = ReqBody '[JSON] Integer :> "foo" :> Get '[JSON] Integer

prioErrorsApi :: Proxy PrioErrorsApi
prioErrorsApi = Proxy

-- Check whether matching continues even if a 'ReqBody' or similar construct
-- is encountered early in a path. We don't want to see a complaint about the
-- request body unless the path actually matches.
prioErrorsSpec :: Spec
prioErrorsSpec = describe "PrioErrors" $ do
  let server = return
  with (return $ serve prioErrorsApi server) $ do
    let check (mdescr, method) path (cdescr, ctype, body) resp =
          it fulldescr $
            Test.Hspec.Wai.request method path [(hContentType, ctype)] body
              `shouldRespondWith` resp
          where
            fulldescr = "returns " ++ show (matchStatus resp) ++ " on " ++ mdescr
                     ++ " " ++ (BC.unpack path) ++ " (" ++ cdescr ++ ")"

        get' = ("GET", methodGet)
        put' = ("PUT", methodPut)

        txt   = ("text"        , "text/plain;charset=utf8"      , "42"        )
        ijson = ("invalid json", "application/json;charset=utf8", "invalid"   )
        vjson = ("valid json"  , "application/json;charset=utf8", encode (5 :: Int))

    check get' "/"    txt   404
    check get' "/bar" txt   404
    check get' "/foo" txt   415
    check put' "/"    txt   404
    check put' "/bar" txt   404
    check put' "/foo" txt   405
    check get' "/"    ijson 404
    check get' "/bar" ijson 404
    check get' "/foo" ijson 400
    check put' "/"    ijson 404
    check put' "/bar" ijson 404
    check put' "/foo" ijson 405
    check get' "/"    vjson 404
    check get' "/bar" vjson 404
    check get' "/foo" vjson 200
    check put' "/"    vjson 404
    check put' "/bar" vjson 404
    check put' "/foo" vjson 405

-- }}}
------------------------------------------------------------------------------
-- * Error Retry {{{

type ErrorRetryApi
     = "a" :> ReqBody '[JSON] Int      :> Post '[JSON] Int                -- 0
  :<|> "a" :> ReqBody '[PlainText] Int :> Post '[JSON] Int                -- 1
  :<|> "a" :> ReqBody '[JSON] Int      :> Post '[PlainText] Int           -- 2
  :<|> "a" :> ReqBody '[JSON] String   :> Post '[JSON] Int                -- 3
  :<|> "a" :> ReqBody '[JSON] Int      :> Get  '[JSON] Int                -- 4
  :<|> "a" :> ReqBody '[JSON] Int      :> Get  '[PlainText] Int           -- 5
  :<|>        ReqBody '[JSON] Int      :> Get  '[JSON] Int                -- 6
  :<|>        ReqBody '[JSON] Int      :> Post '[JSON] Int                -- 7

errorRetryApi :: Proxy ErrorRetryApi
errorRetryApi = Proxy

errorRetryServer :: Server ErrorRetryApi
errorRetryServer
     = (\_ -> return 0)
  :<|> (\_ -> return 1)
  :<|> (\_ -> return 2)
  :<|> (\_ -> return 3)
  :<|> (\_ -> return 4)
  :<|> (\_ -> return 5)
  :<|> (\_ -> return 6)
  :<|> (\_ -> return 7)

errorRetrySpec :: Spec
errorRetrySpec = describe "Handler search"
           $ with (return $ serve errorRetryApi errorRetryServer) $ do

  let plainCT     = (hContentType, "text/plain")
      plainAccept = (hAccept, "text/plain")
      jsonCT      = (hContentType, "application/json")
      jsonAccept  = (hAccept, "application/json")
      jsonBody    = encode (1797 :: Int)

  it "should continue when URLs don't match" $ do
    request methodPost "" [jsonCT, jsonAccept] jsonBody
     `shouldRespondWith` 201 { matchBody = Just $ encode (7 :: Int) }

  it "should continue when methods don't match" $ do
    request methodGet "a" [jsonCT, jsonAccept] jsonBody
     `shouldRespondWith` 200 { matchBody = Just $ encode (4 :: Int) }

  it "should not continue when Content-Types don't match" $ do
    request methodPost "a" [plainCT, jsonAccept] jsonBody
     `shouldRespondWith` 415

  it "should not continue when body can't be deserialized" $ do
    request methodPost "a" [jsonCT, jsonAccept] (encode ("nonsense" :: String))
     `shouldRespondWith` 400

  it "should not continue when Accepts don't match" $ do
    request methodPost "a" [jsonCT, plainAccept] jsonBody
     `shouldRespondWith` 406

-- }}}
------------------------------------------------------------------------------
-- * Error Choice {{{

type ErrorChoiceApi
     = "path0" :> Get '[JSON] Int                                     -- 0
  :<|> "path1" :> Post '[JSON] Int                                    -- 1
  :<|> "path2" :> Post '[PlainText] Int                               -- 2
  :<|> "path3" :> ReqBody '[JSON] Int :> Post '[PlainText] Int        -- 3
  :<|> "path4" :> (ReqBody '[PlainText] Int :> Post '[PlainText] Int  -- 4
             :<|>  ReqBody '[PlainText] Int :> Post '[JSON] Int)      -- 5

errorChoiceApi :: Proxy ErrorChoiceApi
errorChoiceApi = Proxy

errorChoiceServer :: Server ErrorChoiceApi
errorChoiceServer = return 0
               :<|> return 1
               :<|> return 2
               :<|> (\_ -> return 3)
               :<|> (\_ -> return 4)
               :<|> (\_ -> return 5)


errorChoiceSpec :: Spec
errorChoiceSpec = describe "Multiple handlers return errors"
                $ with (return $ serve errorChoiceApi errorChoiceServer) $ do

  it "should respond with 404 if no path matches" $ do
    request methodGet "" [] "" `shouldRespondWith` 404

  it "should respond with 405 if a path but not method matches" $ do
    request methodGet "path2" [] "" `shouldRespondWith` 405

  it "should respond with the corresponding error if path and method match" $ do
    request methodPost "path3" [(hContentType, "text/plain;charset=utf-8")] ""
      `shouldRespondWith` 415
    request methodPost "path3" [(hContentType, "application/json")] ""
      `shouldRespondWith` 400
    request methodPost "path4" [(hContentType, "text/plain;charset=utf-8"),
                                (hAccept, "application/json")] ""
      `shouldRespondWith` 406


-- }}}
------------------------------------------------------------------------------
-- * Instances {{{

instance MimeUnrender PlainText Int where
    mimeUnrender _ = Right . read . BCL.unpack

instance MimeRender PlainText Int where
    mimeRender _ = BCL.pack . show
-- }}}
--

