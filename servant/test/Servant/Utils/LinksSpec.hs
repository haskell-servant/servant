{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Servant.Utils.LinksSpec where

import           Data.Proxy              (Proxy (..))
import           Test.Hspec              (Expectation, Spec, describe, it,
                                          shouldBe)

import           Servant.API

type TestApi =
  -- Capture and query params
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Delete '[JSON] NoContent
  :<|> "all" :> CaptureAll "names" String :> Get '[JSON] NoContent

  -- Flags
  :<|> "balls" :> QueryFlag "bouncy" :> QueryFlag "fast" :> Delete '[JSON] NoContent

  -- All of the verbs
  :<|> "get" :> Get '[JSON] NoContent
  :<|> "put" :> Put '[JSON] NoContent
  :<|> "post" :> ReqBody '[JSON] 'True :> Post '[JSON] NoContent
  :<|> "delete" :> Header "ponies" String :> Delete '[JSON] NoContent
  :<|> "raw" :> Raw


apiLink :: (IsElem endpoint TestApi, HasLink endpoint)
         => Proxy endpoint -> MkLink endpoint
apiLink = safeLink (Proxy :: Proxy TestApi)

-- | Convert a link to a URI and ensure that this maps to the given string
-- given string
shouldBeURI :: URI -> String -> Expectation
shouldBeURI link expected =
    show link `shouldBe` expected

spec :: Spec
spec = describe "Servant.Utils.Links" $ do
    it "generates correct links for capture query params" $ do
        let l1 = Proxy :: Proxy ("hello" :> Capture "name" String :> Delete '[JSON] NoContent)
        apiLink l1 "hi" `shouldBeURI` "hello/hi"

        let l2 = Proxy :: Proxy ("hello" :> Capture "name" String
                                         :> QueryParam "capital" Bool
                                         :> Delete '[JSON] NoContent)
        apiLink l2 "bye" (Just True) `shouldBeURI` "hello/bye?capital=true"

    it "generates correct links for CaptureAll" $ do
        apiLink (Proxy :: Proxy ("all" :> CaptureAll "names" String :> Get '[JSON] NoContent))
          ["roads", "lead", "to", "rome"]
          `shouldBeURI` "all/roads/lead/to/rome"

    it "generates correct links for query flags" $ do
        let l1 = Proxy :: Proxy ("balls" :> QueryFlag "bouncy"
                                         :> QueryFlag "fast" :> Delete '[JSON] NoContent)
        apiLink l1 True True `shouldBeURI` "balls?bouncy&fast"
        apiLink l1 False True `shouldBeURI` "balls?fast"

    it "generates correct links for all of the verbs" $ do
        apiLink (Proxy :: Proxy ("get" :> Get '[JSON] NoContent)) `shouldBeURI` "get"
        apiLink (Proxy :: Proxy ("put" :> Put '[JSON] NoContent)) `shouldBeURI` "put"
        apiLink (Proxy :: Proxy ("post" :> Post '[JSON] NoContent)) `shouldBeURI` "post"
        apiLink (Proxy :: Proxy ("delete" :> Delete '[JSON] NoContent)) `shouldBeURI` "delete"
        apiLink (Proxy :: Proxy ("raw" :> Raw)) `shouldBeURI` "raw"


-- |
-- Before https://github.com/CRogers/should-not-typecheck/issues/5 is fixed,
-- we'll just use doctest
--
-- >>> apiLink (Proxy :: Proxy WrongPath)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongReturnType)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongContentType)
-- ...
-- ...Could not deduce...
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
-- sanity check
-- >>> apiLink (Proxy :: Proxy AllGood)
-- get
type WrongPath = "getTypo" :> Get '[JSON] NoContent
type WrongReturnType = "get" :> Get '[JSON] Bool
type WrongContentType = "get" :> Get '[OctetStream] NoContent
type WrongMethod = "get" :> Post '[JSON] NoContent
type NotALink = "hello" :> ReqBody '[JSON] 'True :> Get '[JSON] Bool
type AllGood = "get" :> Get '[JSON] NoContent
