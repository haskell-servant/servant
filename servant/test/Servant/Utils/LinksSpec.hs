{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Servant.Utils.LinksSpec where

import Test.Hspec ( Spec, it, describe, shouldBe, Expectation )
import Data.Proxy ( Proxy(..) )

import Servant.API

type TestApi =
  -- Capture and query params
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Delete '[JSON] ()

  -- Flags
  :<|> "balls" :> QueryFlag "bouncy" :> QueryFlag "fast" :> Delete '[JSON] ()

  -- All of the verbs
  :<|> "get" :> Get '[JSON] ()
  :<|> "put" :> Put '[JSON] ()
  :<|> "post" :> ReqBody '[JSON] 'True :> Post '[JSON] ()
  :<|> "delete" :> Header "ponies" String :> Delete '[JSON] ()
  :<|> "raw" :> Raw

type TestLink = "hello" :> "hi" :> Get '[JSON] Bool
type TestLink2 = "greet" :> ReqBody '[JSON] [Int] :> Post '[PlainText] Bool
type TestLink3 = "parent" :> "child" :> Get '[JSON] String

type BadTestLink = "hallo" :> "hi" :> Get '[JSON] Bool
type BadTestLink2 = "greet" :> Get '[PlainText] Bool

type BadTestLink' = "hello" :> "hi" :> Get '[OctetStream] Bool
type BadTestLink'2 = "greet" :> Get '[OctetStream] Bool

type NotALink = "hello" :> Capture "x" Bool :> Get '[JSON] Bool
type NotALink2 = "hello" :> ReqBody '[JSON] 'True :> Get '[JSON] Bool

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
    it "Generates correct links for capture query params" $ do
        let l1 = Proxy :: Proxy ("hello" :> Capture "name" String :> Delete '[JSON] ())
        apiLink l1 "hi" `shouldBeURI` "hello/hi"

        let l2 = Proxy :: Proxy ("hello" :> Capture "name" String
                                         :> QueryParam "capital" Bool
                                         :> Delete '[JSON] ())
        apiLink l2 "bye" (Just True) `shouldBeURI` "hello/bye?capital=true"


    it "Generates correct links for query flags" $ do
        let l1 = Proxy :: Proxy ("balls" :> QueryFlag "bouncy"
                                         :> QueryFlag "fast" :> Delete '[JSON] ())
        apiLink l1 True True `shouldBeURI` "balls?bouncy&fast"
        apiLink l1 False True `shouldBeURI` "balls?fast"

    it "Generates correct links for all of the verbs" $ do
        apiLink (Proxy :: Proxy ("get" :> Get '[JSON] ())) `shouldBeURI` "get"
        apiLink (Proxy :: Proxy ("put" :> Put '[JSON] ())) `shouldBeURI` "put"
        apiLink (Proxy :: Proxy ("post" :> Post '[JSON] ())) `shouldBeURI` "post"
        apiLink (Proxy :: Proxy ("delete" :> Delete '[JSON] ())) `shouldBeURI` "delete"
        apiLink (Proxy :: Proxy ("raw" :> Raw)) `shouldBeURI` "raw"
