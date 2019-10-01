{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.LinksSpec where

import           Data.Proxy
                 (Proxy (..))
import           Data.String
                 (fromString)
import           GHC.Generics
import           Test.Hspec
                 (Expectation, Spec, describe, it, shouldBe)
import           Web.FormUrlEncoded
                 (ToForm(..))

import           Servant.API
import           Servant.Test.ComprehensiveAPI
                 (comprehensiveAPIWithoutRaw)
import           Servant.Links

type TestApi =
  -- Capture and query params
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Delete '[JSON] NoContent
  :<|> "hi"    :> Capture "name" String :> QueryParam' '[Required] "capital" Bool :> Delete '[JSON] NoContent
  :<|> "formR" :> QueryParamForm'  '[Required, Strict] "someform" TestForm :> Delete '[JSON] NoContent
  :<|> "form-opt" :> QueryParamForm  "someform" TestForm :> Delete '[JSON] NoContent
  :<|> "all" :> CaptureAll "names" String :> Get '[JSON] NoContent

  -- Flags
  :<|> "balls" :> QueryFlag "bouncy" :> QueryFlag "fast" :> Delete '[JSON] NoContent

  -- All of the verbs
  :<|> "get" :> Get '[JSON] NoContent
  :<|> "put" :> Put '[JSON] NoContent
  :<|> "post" :> ReqBody '[JSON] Bool :> Post '[JSON] NoContent
  :<|> "delete" :> Header "ponies" String :> Delete '[JSON] NoContent
  :<|> "raw" :> Raw
  :<|> NoEndpoint

type LinkableApi =
       "all" :> CaptureAll "names" String :> Get '[JSON] NoContent
  :<|> "get" :> Get '[JSON] NoContent


apiLink :: (IsElem endpoint TestApi, HasLink endpoint)
         => Proxy endpoint -> MkLink endpoint Link
apiLink = safeLink (Proxy :: Proxy TestApi)

data TestForm = TestForm {
    testing :: String
    , time :: String
} deriving (Eq, Generic)

instance ToForm TestForm

-- | Convert a link to a URI and ensure that this maps to the given string
-- given string
shouldBeLink :: Link -> String -> Expectation
shouldBeLink link expected =
    toUrlPiece link `shouldBe` fromString expected

spec :: Spec
spec = describe "Servant.Links" $ do
    it "generates correct links for capture query params" $ do
        let l1 = Proxy :: Proxy ("hello" :> Capture "name" String :> Delete '[JSON] NoContent)
        apiLink l1 "hi" `shouldBeLink` "hello/hi"

        let l2 = Proxy :: Proxy ("hello" :> Capture "name" String
                                         :> QueryParam "capital" Bool
                                         :> Delete '[JSON] NoContent)
        apiLink l2 "bye" (Just True) `shouldBeLink` "hello/bye?capital=true"

        let l4 = Proxy :: Proxy ("hi" :> Capture "name" String
                                      :> QueryParam' '[Required] "capital" Bool
                                      :> Delete '[JSON] NoContent)
        apiLink l4 "privet" False `shouldBeLink` "hi/privet?capital=false"

    it "generates query param form links" $ do
        -- most who use QueryParamForm are not going to use it Required, Strict, so we'll test it both ways
        let l3 = Proxy :: Proxy ("formR" :> QueryParamForm'  '[Required, Strict] "someform" TestForm
                                         :> Delete '[JSON] NoContent)
        -- We allow `urlEncodeAsFormStable` to uri Escape for us. Validating that assumption here:
        apiLink l3 (TestForm "sure" "später") `shouldBeLink` "formR?testing=sure&time=sp%C3%A4ter"

        let l4 = Proxy :: Proxy ("form-opt" :> QueryParamForm  "someform" TestForm
                                            :> Delete '[JSON] NoContent)
        apiLink l4 (Just $ TestForm "sure" "später") `shouldBeLink` "form-opt?testing=sure&time=sp%C3%A4ter"

    it "generates correct links for CaptureAll" $ do
        apiLink (Proxy :: Proxy ("all" :> CaptureAll "names" String :> Get '[JSON] NoContent))
          ["roads", "lead", "to", "rome"]
          `shouldBeLink` "all/roads/lead/to/rome"

    it "generates correct links for query flags" $ do
        let l1 = Proxy :: Proxy ("balls" :> QueryFlag "bouncy"
                                         :> QueryFlag "fast" :> Delete '[JSON] NoContent)
        apiLink l1 True True `shouldBeLink` "balls?bouncy&fast"
        apiLink l1 False True `shouldBeLink` "balls?fast"

    it "generates correct links for all of the verbs" $ do
        apiLink (Proxy :: Proxy ("get" :> Get '[JSON] NoContent)) `shouldBeLink` "get"
        apiLink (Proxy :: Proxy ("put" :> Put '[JSON] NoContent)) `shouldBeLink` "put"
        apiLink (Proxy :: Proxy ("post" :> Post '[JSON] NoContent)) `shouldBeLink` "post"
        apiLink (Proxy :: Proxy ("delete" :> Delete '[JSON] NoContent)) `shouldBeLink` "delete"
        apiLink (Proxy :: Proxy ("raw" :> Raw)) `shouldBeLink` "raw"

    it "can generate all links for an API that has only linkable endpoints" $ do
        let (allNames :<|> simple) = allLinks (Proxy :: Proxy LinkableApi)
        simple `shouldBeLink` "get"
        allNames ["Seneca", "Aurelius"] `shouldBeLink` "all/Seneca/Aurelius"

    it "can generate all links for ComprehensiveAPIWithoutRaw" $ do
        let firstLink :<|> _ = allLinks comprehensiveAPIWithoutRaw
        firstLink `shouldBeLink` ""

-- |
-- Before https://github.com/CRogers/should-not-typecheck/issues/5 is fixed,
-- we'll just use doctest
--
-- with TypeError comparing for errors is difficult.
--
-- >>> apiLink (Proxy :: Proxy WrongPath)
-- ...
-- ......:...:...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongReturnType)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongContentType)
-- ...
-- ......:...:...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongMethod)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> apiLink (Proxy :: Proxy NotALink)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> linkURI $ apiLink (Proxy :: Proxy NoEndpoint)
-- ...
-- <interactive>...
-- ...
--
-- sanity check
-- >>> toUrlPiece $ apiLink (Proxy :: Proxy AllGood)
-- "get"
type WrongPath = "getTypo" :> Get '[JSON] NoContent
type WrongReturnType = "get" :> Get '[JSON] Bool
type WrongContentType = "get" :> Get '[OctetStream] NoContent
type WrongMethod = "get" :> Post '[JSON] NoContent
type NotALink = "hello" :> ReqBody '[JSON] Bool :> Get '[JSON] Bool
type AllGood = "get" :> Get '[JSON] NoContent
type NoEndpoint = "empty" :> EmptyAPI
