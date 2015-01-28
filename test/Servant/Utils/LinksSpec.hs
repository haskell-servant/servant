{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Utils.LinksSpec where

import Test.Hspec ( Spec, it, describe, shouldBe, Expectation )
import Data.Proxy ( Proxy(..) )

import Servant.API

type TestApi =
  -- Capture and query/matrix params
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Delete

  :<|> "parent" :> MatrixParams "name" String :> "child"
                :> MatrixParam "gender" String :> Get String

  -- Flags
  :<|> "ducks" :> MatrixFlag "yellow" :> MatrixFlag "loud" :> Delete
  :<|> "balls" :> QueryFlag "bouncy" :> QueryFlag "fast" :> Delete

  -- All of the verbs
  :<|> "get" :> Get ()
  :<|> "put" :> Put ()
  :<|> "post" :> ReqBody 'True :> Post ()
  :<|> "delete" :> Header "ponies" :> Delete
  :<|> "raw" :> Raw

type TestLink = "hello" :> "hi" :> Get Bool
type TestLink2 = "greet" :> Post Bool
type TestLink3 = "parent" :> "child" :> Get String

api :: Proxy TestApi
api = Proxy

-- | Convert a link to a URI and ensure that this maps to the given string
-- given string
shouldBeURI :: URI -> String -> Expectation
shouldBeURI link expected =
    show link `shouldBe` expected

spec :: Spec
spec = describe "Servant.Utils.Links" $ do
    it "Generates correct links for capture query and matrix params" $ do
        let l1 = Proxy :: Proxy ("hello" :> Capture "name" String :> Delete)
        safeLink l1 api "hi" `shouldBeURI` "hello/hi"

        let l2 = Proxy :: Proxy ("hello" :> Capture "name" String
                                         :> QueryParam "capital" Bool
                                         :> Delete)
        safeLink l2 api "bye" True `shouldBeURI` "hello/bye?capital=true"

        let l3 = Proxy :: Proxy ("parent" :> MatrixParams "name" String
                                          :> "child"
                                          :> MatrixParam "gender" String
                                          :> Get String)
        safeLink l3 api ["Hubert?x=;&", "Cumberdale"] "Edward?"
            `shouldBeURI` "parent;name[]=Hubert%3Fx%3D%3B%26;\
                           \name[]=Cumberdale/child;gender=Edward%3F"

    it "Generates correct links for query and matrix flags" $ do
        let l1 = Proxy :: Proxy ("balls" :> QueryFlag "bouncy"
                                         :> QueryFlag "fast" :> Delete)
        safeLink l1 api True True `shouldBeURI` "balls?bouncy&fast"
        safeLink l1 api False True `shouldBeURI` "balls?fast"

        let l2 = Proxy :: Proxy ("ducks" :> MatrixFlag "yellow"
                                         :> MatrixFlag "loud" :> Delete)
        safeLink l2 api True True `shouldBeURI` "ducks;yellow;loud"
        safeLink l2 api False True `shouldBeURI` "ducks;loud"

    it "Generates correct links for all of the verbs" $ do
        safeLink (Proxy :: Proxy ("get" :> Get ())) api `shouldBeURI` "get"
        safeLink (Proxy :: Proxy ("put" :> Put ())) api `shouldBeURI` "put"
        safeLink (Proxy :: Proxy ("post" :> Post ())) api `shouldBeURI` "post"
        safeLink (Proxy :: Proxy ("delete" :> Delete)) api `shouldBeURI` "delete"
        safeLink (Proxy :: Proxy ("raw" :> Raw)) api `shouldBeURI` "raw"
