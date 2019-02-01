{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Server.ErrorSpec (spec) where

import           Control.Monad
                 (when)
import           Data.Aeson
                 (encode)
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Monoid
                 ((<>))
import           Data.Proxy
import           Network.HTTP.Types
                 (hAccept, hAuthorization, hContentType, methodGet, methodPost,
                 methodPut)
import           Safe
                 (readMay)
import           Test.Hspec
import           Test.Hspec.Wai

import           Servant

spec :: Spec
spec = describe "HTTP Errors" $ do
    errorOrderSpec
    prioErrorsSpec
    errorRetrySpec
    errorChoiceSpec

-- * Auth machinery (reused throughout)

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
errorOrderAuthCheck :: BasicAuthCheck ()
errorOrderAuthCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized ())
        else return Unauthorized
  in BasicAuthCheck check

------------------------------------------------------------------------------
-- * Error Order {{{

type ErrorOrderApi = "home"
                  :> BasicAuth "error-realm" ()
                  :> ReqBody '[JSON] Int
                  :> Capture "t" Int
                  :> QueryParam "param" Int
                  :> Post '[JSON] Int

errorOrderApi :: Proxy ErrorOrderApi
errorOrderApi = Proxy

errorOrderServer :: Server ErrorOrderApi
errorOrderServer = \_ _ _ _ -> throwError err402

-- On error priorities:
--
-- We originally had
--
-- 404, 405, 401, 415, 400, 406, 402
--
-- but we changed this to
--
-- 404, 405, 401, 406, 415, 400, 402
--
-- for servant-0.7.
--
-- This change is due to the body check being irreversible (to support
-- streaming). Any check done after the body check has to be made fatal,
-- breaking modularity. We've therefore moved the accept check before
-- the body check, to allow it being recoverable and modular, and this
-- goes along with promoting the error priority of 406.
errorOrderSpec :: Spec
errorOrderSpec =
  describe "HTTP error order" $
    with (return $ serveWithContext errorOrderApi
                   (errorOrderAuthCheck :. EmptyContext)
                   errorOrderServer
         ) $ do
  let badContentType  = (hContentType, "text/plain")
      badAccept       = (hAccept, "text/plain")
      badMethod       = methodGet
      badUrl          = "nonexistent"
      badBody         = "nonsense"
      badAuth         = (hAuthorization, "Basic foofoofoo")
      goodContentType = (hContentType, "application/json")
      goodAccept      = (hAccept, "application/json")
      goodMethod      = methodPost
      goodUrl         = "home/2?param=55"
      badParams       = goodUrl <> "?param=foo"
      goodBody        = encode (5 :: Int)
      -- username:password = servant:server
      goodAuth        = (hAuthorization, "Basic c2VydmFudDpzZXJ2ZXI=")

  it "has 404 as its highest priority error" $ do
    request badMethod badUrl [badAuth, badContentType, badAccept] badBody
      `shouldRespondWith` 404

  it "has 405 as its second highest priority error" $ do
    request badMethod badParams [badAuth, badContentType, badAccept] badBody
      `shouldRespondWith` 405

  it "has 401 as its third highest priority error (auth)" $ do
    request goodMethod badParams [badAuth, badContentType, badAccept] badBody
      `shouldRespondWith` 401

  it "has 406 as its fourth highest priority error" $ do
    request goodMethod badParams [goodAuth, badContentType, badAccept] badBody
      `shouldRespondWith` 406

  it "has 415 as its fifth highest priority error" $ do
    request goodMethod badParams [goodAuth, badContentType, goodAccept] badBody
      `shouldRespondWith` 415

  it "has 400 as its sixth highest priority error" $ do
    badParamsRes <- request goodMethod badParams [goodAuth, goodContentType, goodAccept] goodBody
    badBodyRes <- request goodMethod goodUrl [goodAuth, goodContentType, goodAccept] badBody

    -- Both bad body and bad params result in 400
    return badParamsRes `shouldRespondWith` 400
    return badBodyRes `shouldRespondWith` 400

    -- Param check should occur before body checks
    both <- request goodMethod badParams [goodAuth, goodContentType, goodAccept ] badBody
    when (both /= badParamsRes) $ liftIO $
        expectationFailure $ "badParams + badBody /= badParams: " ++ show both ++ ", " ++ show badParamsRes
    when (both == badBodyRes) $ liftIO $
        expectationFailure $ "badParams + badBody == badBody: " ++ show both

  it "has handler-level errors as last priority" $ do
    request goodMethod goodUrl [goodAuth, goodContentType, goodAccept] goodBody
      `shouldRespondWith` 402

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
                     ++ " " ++ BC.unpack path ++ " (" ++ cdescr ++ ")"

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
     = "a" :> ReqBody '[JSON] Int      :> Post '[JSON] Int                -- err402
  :<|> "a" :> ReqBody '[PlainText] Int :> Post '[JSON] Int                -- 1
  :<|> "a" :> ReqBody '[JSON] Int      :> Post '[PlainText] Int           -- 2
  :<|> "a" :> ReqBody '[JSON] String   :> Post '[JSON] Int                -- 3
  :<|> "a" :> ReqBody '[JSON] Int      :> Get  '[JSON] Int                -- 4
  :<|> "a" :> BasicAuth "bar-realm" ()
           :> ReqBody '[JSON] Int      :> Get  '[PlainText] Int           -- 5
  :<|> "a" :> ReqBody '[JSON] Int      :> Get  '[PlainText] Int           -- 6

  :<|>        ReqBody '[JSON] Int      :> Get  '[JSON] Int                -- 7
  :<|>        ReqBody '[JSON] Int      :> Post '[JSON] Int                -- 8

errorRetryApi :: Proxy ErrorRetryApi
errorRetryApi = Proxy

errorRetryServer :: Server ErrorRetryApi
errorRetryServer
     = (\_ -> throwError err402)
  :<|> (\_ -> return 1)
  :<|> (\_ -> return 2)
  :<|> (\_ -> return 3)
  :<|> (\_ -> return 4)
  :<|> (\_ _ -> return 5)
  :<|> (\_ -> return 6)
  :<|> (\_ -> return 7)
  :<|> (\_ -> return 8)

errorRetrySpec :: Spec
errorRetrySpec =
  describe "Handler search" $
    with (return $ serveWithContext errorRetryApi
                         (errorOrderAuthCheck :. EmptyContext)
                         errorRetryServer
         ) $ do

  let jsonCT      = (hContentType, "application/json")
      jsonAccept  = (hAccept, "application/json")
      jsonBody    = encode (1797 :: Int)

  it "should continue when URLs don't match" $ do
    request methodPost "" [jsonCT, jsonAccept] jsonBody
     `shouldRespondWith` 200 { matchBody = mkBody $ encode (8 :: Int) }

  it "should continue when methods don't match" $ do
    request methodGet "a" [jsonCT, jsonAccept] jsonBody
     `shouldRespondWith` 200 { matchBody = mkBody $ encode (4 :: Int) }
  where
    mkBody b = MatchBody $ \_ b' ->
      if b == b'
        then Nothing
        else Just "body not correct\n"

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
  :<|> "path5" :> (ReqBody '[JSON] Int      :> Post '[PlainText] Int  -- 6
             :<|>  ReqBody '[PlainText] Int :> Post '[PlainText] Int) -- 7

errorChoiceApi :: Proxy ErrorChoiceApi
errorChoiceApi = Proxy

errorChoiceServer :: Server ErrorChoiceApi
errorChoiceServer = return 0
               :<|> return 1
               :<|> return 2
               :<|> (\_ -> return 3)
               :<|> ((\_ -> return 4) :<|> (\_ -> return 5))
               :<|> ((\_ -> return 6) :<|> (\_ -> return 7))


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
                                (hAccept, "blah")] "5"
      `shouldRespondWith` 406
  it "should respond with 415 only if none of the subservers supports the request's content type" $ do
    request methodPost "path5" [(hContentType, "text/plain;charset=utf-8")] "1"
      `shouldRespondWith` 200
    request methodPost "path5" [(hContentType, "application/json")] "1"
      `shouldRespondWith` 200
    request methodPost "path5" [(hContentType, "application/not-supported")] ""
      `shouldRespondWith` 415


-- }}}
------------------------------------------------------------------------------
-- * Instances {{{

instance MimeUnrender PlainText Int where
    mimeUnrender _ x = maybe (Left "no parse") Right (readMay $ BCL.unpack x)

instance MimeRender PlainText Int where
    mimeRender _ = BCL.pack . show
-- }}}
