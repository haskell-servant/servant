{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.LinksSpec where

import           Data.Proxy
                 (Proxy (..))
import           Data.String
                 (fromString)
import           Test.Hspec
                 (Expectation, Spec, describe, it, shouldBe)

import           Servant.API
import           Servant.Links
import           Servant.Test.ComprehensiveAPI
                 (comprehensiveAPIWithoutRaw)

type TestApi =
  -- Capture and query params
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Delete '[JSON] NoContent
  :<|> "hi"    :> Capture "name" String :> QueryParam' '[Required] "capital" Bool :> Delete '[JSON] NoContent
  :<|> "all" :> CaptureAll "names" String :> Get '[JSON] NoContent

  -- Flags
  :<|> "balls" :> QueryFlag "bouncy" :> QueryFlag "fast" :> Delete '[JSON] NoContent

  -- Fragment
  :<|> "say" :> Fragment String :> Get '[JSON] NoContent

  -- UVerb
  :<|> "uverb-example" :> UVerb 'GET '[JSON] '[WithStatus 200 NoContent]

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

    it "generates correct links for CaptureAll" $ do
        apiLink (Proxy :: Proxy ("all" :> CaptureAll "names" String :> Get '[JSON] NoContent))
          ["roads", "lead", "to", "rome"]
          `shouldBeLink` "all/roads/lead/to/rome"

    it "generated correct links for UVerbs" $ do
      apiLink (Proxy :: Proxy ("uverb-example" :> UVerb 'GET '[JSON] '[WithStatus 200 NoContent]))
        `shouldBeLink` "uverb-example"

    it "generates correct links for query flags" $ do
        let l1 = Proxy :: Proxy ("balls" :> QueryFlag "bouncy"
                                         :> QueryFlag "fast" :> Delete '[JSON] NoContent)
        apiLink l1 True True `shouldBeLink` "balls?bouncy&fast"
        apiLink l1 False True `shouldBeLink` "balls?fast"

    it "generates correct link for fragment" $ do
        let l1 = Proxy :: Proxy ("say" :> Fragment String :> Get '[JSON] NoContent)
        apiLink l1 "something" `shouldBeLink` "say#something"

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

-- The doctests below aren't run on CI, setting that up is tricky.
-- They are run by makefile rule, however.

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
