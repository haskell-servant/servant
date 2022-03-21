{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Semigroup
import Data.Text (Text)
import           Servant hiding (NotSecure)
import           Servant.Server
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setLogger, setPort)

import qualified Data.Text as T

type MainAPI =
    "static" :> "1" :> Get '[JSON] Text
    :<|> "static" :> "2" :> Get '[JSON] Text
    :<|> "static" :> "3" :> Get '[JSON] Text
    :<|> "static" :> "4" :> Get '[JSON] Text
    :<|> "static" :> "5" :> Get '[JSON] Text
    :<|> "static" :> "6" :> Get '[JSON] Text
    :<|> "static" :> "7" :> Get '[JSON] Text
    :<|> "static" :> "8" :> Get '[JSON] Text
    :<|> "static" :> "9" :> Get '[JSON] Text
    :<|> "static" :> "10" :> Get '[JSON] Text
    :<|> "static" :> "11" :> Get '[JSON] Text
    :<|> "static" :> "12" :> Get '[JSON] Text
    :<|> "static" :> "13" :> Get '[JSON] Text
    :<|> "static" :> "14" :> Get '[JSON] Text
    :<|> "static" :> "15" :> Get '[JSON] Text
    :<|> "static" :> "16" :> Get '[JSON] Text
    :<|> "static" :> "17" :> Get '[JSON] Text
    :<|> "static" :> "18" :> Get '[JSON] Text
    :<|> "static" :> "19" :> Get '[JSON] Text
    :<|> "static" :> "20" :> Get '[JSON] Text
    :<|> "static" :> "21" :> Get '[JSON] Text
    :<|> "static" :> "22" :> Get '[JSON] Text
    :<|> "static" :> "23" :> Get '[JSON] Text
    :<|> "static" :> "24" :> Get '[JSON] Text
    :<|> "static" :> "25" :> Get '[JSON] Text
    :<|> "static" :> "26" :> Get '[JSON] Text
    :<|> "static" :> "27" :> Get '[JSON] Text
    :<|> "static" :> "28" :> Get '[JSON] Text
    :<|> "static" :> "29" :> Get '[JSON] Text
    :<|> "static" :> "30" :> Get '[JSON] Text
    :<|> "static" :> "31" :> Get '[JSON] Text
    :<|> "static" :> "32" :> Get '[JSON] Text
    :<|> "static" :> "33" :> Get '[JSON] Text
    :<|> "static" :> "34" :> Get '[JSON] Text
    :<|> "static" :> "35" :> Get '[JSON] Text
    :<|> "static" :> "36" :> Get '[JSON] Text
    :<|> "static" :> "37" :> Get '[JSON] Text
    :<|> "static" :> "38" :> Get '[JSON] Text
    :<|> "static" :> "39" :> Get '[JSON] Text
    :<|> "static" :> "40" :> Get '[JSON] Text
    :<|> "static" :> "41" :> Get '[JSON] Text
    :<|> "static" :> "42" :> Get '[JSON] Text
    :<|> "static" :> "43" :> Get '[JSON] Text
    :<|> "static" :> "44" :> Get '[JSON] Text
    :<|> "static" :> "45" :> Get '[JSON] Text
    :<|> "static" :> "46" :> Get '[JSON] Text
    :<|> "static" :> "47" :> Get '[JSON] Text
    :<|> "static" :> "48" :> Get '[JSON] Text
    :<|> "static" :> "49" :> Get '[JSON] Text
    :<|> "static" :> "50" :> Get '[JSON] Text
    :<|> "static" :> "51" :> Get '[JSON] Text
    :<|> "static" :> "52" :> Get '[JSON] Text
    :<|> "static" :> "53" :> Get '[JSON] Text
    :<|> "static" :> "54" :> Get '[JSON] Text
    :<|> "static" :> "55" :> Get '[JSON] Text
    :<|> "static" :> "56" :> Get '[JSON] Text
    :<|> "static" :> "57" :> Get '[JSON] Text
    :<|> "static" :> "58" :> Get '[JSON] Text
    :<|> "static" :> "59" :> Get '[JSON] Text
    :<|> "static" :> "60" :> Get '[JSON] Text
    :<|> "static" :> "61" :> Get '[JSON] Text
    :<|> "static" :> "62" :> Get '[JSON] Text
    :<|> "static" :> "63" :> Get '[JSON] Text
    :<|> "static" :> "64" :> Get '[JSON] Text

mainServer :: Server MainAPI
mainServer = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

foo = return "foo"

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer
