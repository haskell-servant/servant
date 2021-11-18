{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Server.RouterSpec (spec) where

import           Control.Monad
                 (unless)
import           Data.Proxy
                 (Proxy (..))
import           Data.Text
                 (unpack)
import           Network.HTTP.Types
                 (Status (..))
import           Network.Wai
                 (responseBuilder)
import           Network.Wai.Internal
                 (Response (ResponseBuilder))
import           Servant.API
import           Servant.Server
import           Servant.Server.Internal
import           Test.Hspec
import           Test.Hspec.Wai
                 (get, shouldRespondWith, with)

spec :: Spec
spec = describe "Servant.Server.Internal.Router" $ do
  routerSpec
  distributivitySpec

routerSpec :: Spec
routerSpec = do
  describe "tweakResponse" $ do
    let app' :: Application
        app' = toApplication $ runRouter (const err404) router'

        router', router :: Router ()
        router' = tweakResponse (fmap twk) router
        router = leafRouter $ \_ _ cont -> cont (Route $ responseBuilder (Status 201 "") [] "")

        twk :: Response -> Response
        twk (ResponseBuilder (Status i s) hs b) = ResponseBuilder (Status (i + 1) s) hs b
        twk b = b

    with (return app') $ do
      it "calls f on route result" $ do
        get "" `shouldRespondWith` 202

  describe "runRouter" $ do
    let toApp :: Router () -> Application
        toApp = toApplication . runRouter (const err404)

        cap :: Router ()
        cap = CaptureRouter $
          let delayed = addCapture (emptyDelayed $ Route pure) (const $ delayedFail err400)
          in leafRouter
             $ \env req res ->
                 runAction delayed env req res
                 . const
                 $ Route success

        router :: Router ()
        router = leafRouter (\_ _ res -> res $ Route success)
          `Choice` cap

        success :: Response
        success = responseBuilder (Status 200 "") [] ""

    with (pure $ toApp router) $ do
      it "capture failure returns a 400 response" $ do
        get "/badcapture" `shouldRespondWith` 400

distributivitySpec :: Spec
distributivitySpec =
  describe "choice" $ do
    it "distributes endpoints through static paths" $ do
      endpoint `shouldHaveSameStructureAs` endpointRef
    it "distributes nested routes through static paths" $ do
      static `shouldHaveSameStructureAs` staticRef
    it "distributes nested routes through dynamic paths" $ do
      dynamic `shouldHaveSameStructureAs` dynamicRef
    it "properly reorders permuted static paths" $ do
      permute `shouldHaveSameStructureAs` permuteRef
    it "properly reorders permuted static paths in the presence of QueryParams" $ do
      permuteQuery `shouldHaveSameStructureAs` permuteRef
    it "properly reorders permuted static paths in the presence of Raw in end" $ do
      permuteRawEnd `shouldHaveSameStructureAs` permuteRawEndRef
    it "properly reorders permuted static paths in the presence of Raw in beginning" $ do
      permuteRawBegin `shouldHaveSameStructureAs` permuteRawBeginRef
    it "properly reorders permuted static paths in the presence of Raw in middle" $ do
      permuteRawMiddle `shouldHaveSameStructureAs` permuteRawMiddleRef
    it "properly reorders permuted static paths in the presence of a root endpoint in end" $ do
      permuteEndEnd `shouldHaveSameStructureAs` permuteEndRef
    it "properly reorders permuted static paths in the presence of a root endpoint in beginning" $ do
      permuteEndBegin `shouldHaveSameStructureAs` permuteEndRef
    it "properly reorders permuted static paths in the presence of a root endpoint in middle" $ do
      permuteEndMiddle `shouldHaveSameStructureAs` permuteEndRef
    it "properly handles mixing static paths at different levels" $ do
      level `shouldHaveSameStructureAs` levelRef

shouldHaveSameStructureAs ::
  (HasServer api1 '[], HasServer api2 '[]) => Proxy api1 -> Proxy api2 -> Expectation
shouldHaveSameStructureAs p1 p2 =
  unless (sameStructure (makeTrivialRouter p1) (makeTrivialRouter p2)) $
    expectationFailure ("expected:\n" ++ unpack (layout p2) ++ "\nbut got:\n" ++ unpack (layout p1))

makeTrivialRouter :: (HasServer layout '[]) => Proxy layout -> Router ()
makeTrivialRouter p =
  route p EmptyContext (emptyDelayed (FailFatal err501))

type End = Get '[JSON] NoContent

-- The latter version looks more efficient,
-- but the former should be compiled to the
-- same layout:

type Endpoint    = "a" :> End :<|> "a" :> End
type EndpointRef = "a" :> (End :<|> End)

endpoint :: Proxy Endpoint
endpoint = Proxy

endpointRef :: Proxy EndpointRef
endpointRef = Proxy

-- Again, the latter version looks more efficient,
-- but the former should be compiled to the same
-- layout:

type Static    = "a" :> "b" :> End :<|> "a" :> "c" :> End
type StaticRef = "a" :> ("b" :> End :<|> "c" :> End)

static :: Proxy Static
static = Proxy

staticRef :: Proxy StaticRef
staticRef = Proxy

-- Even for dynamic path components, we expect the
-- router to simplify the layout, because captures
-- are delayed and only actually performed once
-- reaching an endpoint. So the former version and
-- the latter should be compiled to the same router
-- structure:

type Dynamic =
       "a" :> Capture "foo" Int  :> "b" :> End
  :<|> "a" :> Capture "bar" Bool :> "c" :> End
  :<|> "a" :> Capture "baz" Char :> "d" :> End

type DynamicRef =
  "a" :> Capture "anything" () :>
    ("b" :> End :<|> "c" :> End :<|> "d" :> End)

dynamic :: Proxy Dynamic
dynamic = Proxy

dynamicRef :: Proxy DynamicRef
dynamicRef = Proxy

-- A more complicated example of static route reordering.
-- All the permuted paths should be correctly grouped,
-- so both 'Permute' and 'PermuteRef' should compile to
-- the same layout:

type Permute =
       "a" :> "b" :> "c" :> End
  :<|> "b" :> "a" :> "c" :> End
  :<|> "a" :> "c" :> "b" :> End
  :<|> "c" :> "a" :> "b" :> End
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> "b" :> "a" :> End

type PermuteRef =
       "a" :> (    "b" :> "c" :> End
              :<|> "c" :> "b" :> End
              )
  :<|> "b" :> (    "a" :> "c" :> End
              :<|> "c" :> "a" :> End
              )
  :<|> "c" :> (    "a" :> "b" :> End
              :<|> "b" :> "a" :> End
              )

permute :: Proxy Permute
permute = Proxy

permuteRef :: Proxy PermuteRef
permuteRef = Proxy

-- Adding a "QueryParam" should not affect structure

type PermuteQuery =
       QueryParam "1" Int :> "a" :> "b" :> "c" :> End
  :<|> QueryParam "2" Int :> "b" :> "a" :> "c" :> End
  :<|> QueryParam "3" Int :> "a" :> "c" :> "b" :> End
  :<|> QueryParam "4" Int :> "c" :> "a" :> "b" :> End
  :<|> QueryParam "5" Int :> "b" :> "c" :> "a" :> End
  :<|> QueryParam "6" Int :> "c" :> "b" :> "a" :> End

permuteQuery :: Proxy PermuteQuery
permuteQuery = Proxy

-- Adding a 'Raw' in one of the ends should have minimal
-- effect on the grouping.

type PermuteRawEnd =
       "a" :> "b" :> "c" :> End
  :<|> "b" :> "a" :> "c" :> End
  :<|> "a" :> "c" :> "b" :> End
  :<|> "c" :> "a" :> "b" :> End
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> "b" :> "a" :> End
  :<|> Raw

type PermuteRawEndRef = PermuteRef :<|> Raw

type PermuteRawBegin =
       Raw
  :<|> "a" :> "b" :> "c" :> End
  :<|> "b" :> "a" :> "c" :> End
  :<|> "a" :> "c" :> "b" :> End
  :<|> "c" :> "a" :> "b" :> End
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> "b" :> "a" :> End

type PermuteRawBeginRef = Raw :<|> PermuteRef

permuteRawBegin :: Proxy PermuteRawBegin
permuteRawBegin = Proxy

permuteRawBeginRef :: Proxy PermuteRawBeginRef
permuteRawBeginRef = Proxy

permuteRawEnd :: Proxy PermuteRawEnd
permuteRawEnd = Proxy

permuteRawEndRef :: Proxy PermuteRawEndRef
permuteRawEndRef = Proxy

-- Adding a 'Raw' in the middle will disrupt grouping,
-- because we commute things past a 'Raw'. But the two
-- halves should still be grouped.

type PermuteRawMiddle =
       "a" :> "b" :> "c" :> End
  :<|> "b" :> "a" :> "c" :> End
  :<|> "a" :> "c" :> "b" :> End
  :<|> Raw
  :<|> "c" :> "a" :> "b" :> End
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> "b" :> "a" :> End

type PermuteRawMiddleRef =
       "a" :> (    "b" :> "c" :> End
              :<|> "c" :> "b" :> End
              )
  :<|> "b" :> "a" :> "c" :> End
  :<|> Raw
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> (    "a" :> "b" :> End
              :<|> "b" :> "a" :> End
              )

permuteRawMiddle :: Proxy PermuteRawMiddle
permuteRawMiddle = Proxy

permuteRawMiddleRef :: Proxy PermuteRawMiddleRef
permuteRawMiddleRef = Proxy

-- Adding an endpoint at the top-level in various places
-- is also somewhat critical for grouping, but it should
-- not disrupt grouping at all, even if it is placed in
-- the middle.

type PermuteEndEnd =
       "a" :> "b" :> "c" :> End
  :<|> "b" :> "a" :> "c" :> End
  :<|> "a" :> "c" :> "b" :> End
  :<|> "c" :> "a" :> "b" :> End
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> "b" :> "a" :> End
  :<|> End

type PermuteEndBegin =
       End
  :<|> "a" :> "b" :> "c" :> End
  :<|> "b" :> "a" :> "c" :> End
  :<|> "a" :> "c" :> "b" :> End
  :<|> "c" :> "a" :> "b" :> End
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> "b" :> "a" :> End

type PermuteEndMiddle =
       "a" :> "b" :> "c" :> End
  :<|> "b" :> "a" :> "c" :> End
  :<|> "a" :> "c" :> "b" :> End
  :<|> End
  :<|> "c" :> "a" :> "b" :> End
  :<|> "b" :> "c" :> "a" :> End
  :<|> "c" :> "b" :> "a" :> End

type PermuteEndRef = PermuteRef :<|> End

permuteEndEnd :: Proxy PermuteEndEnd
permuteEndEnd = Proxy

permuteEndBegin :: Proxy PermuteEndBegin
permuteEndBegin = Proxy

permuteEndMiddle :: Proxy PermuteEndMiddle
permuteEndMiddle = Proxy

permuteEndRef :: Proxy PermuteEndRef
permuteEndRef = Proxy

-- An API with routes on different nesting levels that
-- is composed out of different fragments should still
-- be reordered correctly.

type LevelFragment1 =
       "a" :> "b" :> End
  :<|> "a" :> End

type LevelFragment2 =
       "b" :> End
  :<|> "a" :> "c" :> End
  :<|> End

type Level = LevelFragment1 :<|> LevelFragment2

type LevelRef =
       "a" :> ("b" :> End :<|> "c" :> End :<|> End)
  :<|> "b" :> End
  :<|> End

level :: Proxy Level
level = Proxy

levelRef :: Proxy LevelRef
levelRef = Proxy
