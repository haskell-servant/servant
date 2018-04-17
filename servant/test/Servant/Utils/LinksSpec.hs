{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ < 709
{-# OPTIONS_GHC -fcontext-stack=41 #-}
#endif
module Servant.Utils.LinksSpec where

import           Data.Proxy              (Proxy (..))
import           Test.Hspec              (Expectation, Spec, describe, it,
                                          shouldBe)
import           Data.String             (fromString)

import           Servant.API
import           Servant.Utils.Links
import           Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPIWithoutRaw)

type TestApi =
  -- Capture and query params
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Delete '[JSON] Int
  :<|> "hi"    :> Capture "name" String :> QueryParam' '[Required] "capital" Bool :> Delete '[JSON] Int
  :<|> "all" :> CaptureAll "names" String :> Get '[JSON] Int

  -- Flags
  :<|> "balls" :> QueryFlag "bouncy" :> QueryFlag "fast" :> Delete '[JSON] Int

  -- All of the verbs
  :<|> "get" :> Get '[JSON] Int
  :<|> "put" :> Put '[JSON] Int
  :<|> "post" :> ReqBody '[JSON] Bool :> Post '[JSON] Int
  :<|> "delete" :> Header "ponies" String :> Delete '[JSON] Int
  :<|> "raw" :> Raw
  :<|> NoEndpoint

type LinkableApi =
       "all" :> CaptureAll "names" String :> Get '[JSON] Int
  :<|> "get" :> Get '[JSON] Int


apiLink :: (IsElem endpoint TestApi, HasLink endpoint)
         => Proxy endpoint -> MkLink endpoint Link
apiLink = safeLink (Proxy :: Proxy TestApi)

-- | Convert a link to a URI and ensure that this maps to the given string
-- given string
shouldBeLink :: Link -> String -> Expectation
shouldBeLink link expected =
    toUrlPiece link `shouldBe` fromString expected

spec :: Spec
spec = describe "Servant.Utils.Links" $ do
    it "generates correct links for capture query params" $ do
        let l1 = Proxy :: Proxy ("hello" :> Capture "name" String :> Delete '[JSON] Int)
        apiLink l1 "hi" `shouldBeLink` "hello/hi"

        let l2 = Proxy :: Proxy ("hello" :> Capture "name" String
                                         :> QueryParam "capital" Bool
                                         :> Delete '[JSON] Int)
        apiLink l2 "bye" (Just True) `shouldBeLink` "hello/bye?capital=true"

        let l4 = Proxy :: Proxy ("hi" :> Capture "name" String
                                      :> QueryParam' '[Required] "capital" Bool
                                      :> Delete '[JSON] Int)
        apiLink l4 "privet" False `shouldBeLink` "hi/privet?capital=false"

    it "generates correct links for CaptureAll" $ do
        apiLink (Proxy :: Proxy ("all" :> CaptureAll "names" String :> Get '[JSON] Int))
          ["roads", "lead", "to", "rome"]
          `shouldBeLink` "all/roads/lead/to/rome"

    it "generates correct links for query flags" $ do
        let l1 = Proxy :: Proxy ("balls" :> QueryFlag "bouncy"
                                         :> QueryFlag "fast" :> Delete '[JSON] Int)
        apiLink l1 True True `shouldBeLink` "balls?bouncy&fast"
        apiLink l1 False True `shouldBeLink` "balls?fast"

    it "generates correct links for all of the verbs" $ do
        apiLink (Proxy :: Proxy ("get" :> Get '[JSON] Int)) `shouldBeLink` "get"
        apiLink (Proxy :: Proxy ("put" :> Put '[JSON] Int)) `shouldBeLink` "put"
        apiLink (Proxy :: Proxy ("post" :> Post '[JSON] Int)) `shouldBeLink` "post"
        apiLink (Proxy :: Proxy ("delete" :> Delete '[JSON] Int)) `shouldBeLink` "delete"
        apiLink (Proxy :: Proxy ("raw" :> Raw)) `shouldBeLink` "raw"

    it "can generate all links for an API that has only linkable endpoints" $ do
        let (allNames :<|> simple) = allLinks (Proxy :: Proxy LinkableApi)
        simple `shouldBeLink` "get"
        allNames ["Seneca", "Aurelius"] `shouldBeLink` "all/Seneca/Aurelius"

    it "can generate all links for ComprehensiveAPIWithoutRaw" $ do
        let (firstLink :<|> _) = allLinks comprehensiveAPIWithoutRaw
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
type WrongPath = "getTypo" :> Get '[JSON] Int
type WrongReturnType = "get" :> Get '[JSON] Bool
type WrongContentType = "get" :> Get '[OctetStream] Int
type WrongMethod = "get" :> Post '[JSON] Int
type NotALink = "hello" :> ReqBody '[JSON] Bool :> Get '[JSON] Bool
type AllGood = "get" :> Get '[JSON] Int
type NoEndpoint = "empty" :> EmptyAPI
