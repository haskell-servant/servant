{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Servant.API.ElemSpec where

import Test.Hspec

import Servant.API
import Servant.API.Elem (IsElem, IsLink)
import Servant.Utils.ApiQuasiQuotingSpec ( (~>) )

type TestApi =
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Get Bool
  :<|> "greet" :> ReqBody 'True :> Post Bool

type TestLink = "hello" :> "hi" :> Get Bool
type TestLink2 = "greet" :> Post Bool

type BadTestLink = "hallo" :> "hi" :> Get Bool
type BadTestLink2 = "greet" :> Get Bool

type NotALink = "hello" :> Capture "x" Bool :> Get Bool
type NotALink2 = "hello" :> ReqBody 'True :> Get Bool

data Proxy x = Proxy
class ReflectT (x::Bool) where { reflected :: Proxy x -> Bool }
instance ReflectT 'True where { reflected _ = True }
instance ReflectT 'False where { reflected _ = False }

spec :: Spec
spec = describe "Servant.API.Elem" $ do
    isElem
    isLink

isElem :: Spec
isElem = describe "IsElem" $ do
    it "is True when the first argument is an url within the second" $ do
       reflected (Proxy::Proxy (IsElem TestLink TestApi)) ~> True
       reflected (Proxy::Proxy (IsElem TestLink2 TestApi)) ~> True
    it "is False when the first argument is not an url within the second" $ do
       reflected (Proxy::Proxy (IsElem BadTestLink TestApi)) ~> False
       reflected (Proxy::Proxy (IsElem BadTestLink2 TestApi)) ~> False

isLink :: Spec
isLink = describe "IsLink" $ do
    it "is True when all Subs are paths and the last is a method" $ do
        reflected (Proxy::Proxy (IsLink TestLink)) ~> True
        reflected (Proxy::Proxy (IsLink TestLink2)) ~> True
    it "is False of anything with captures" $ do
        reflected (Proxy::Proxy (IsLink NotALink)) ~> False
        reflected (Proxy::Proxy (IsLink NotALink2)) ~> False

