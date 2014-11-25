{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Proxy
import Servant
import Servant.JQuery

data Greet = Greet

type TestApi =
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> QueryParam "q" String :> Get Greet
  :<|> "greet" :> ReqBody Greet :> Post Greet
  :<|> "delete" :> Capture "greetid" String :> "haha" :> Delete

testApi :: Proxy TestApi
testApi = Proxy

getHello :<|> postGreet :<|> deleteGreet = jquery testApi

main :: IO ()
main =
  mapM_ printJS [ getHello "getHello"
                , postGreet "postGreet"
                , deleteGreet "deleteGreet"
                ]