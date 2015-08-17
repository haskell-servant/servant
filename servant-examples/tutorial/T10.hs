{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module T10 where

import           Data.ByteString.Lazy    (ByteString)
import           Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Docs
import qualified T3

type DocsAPI = T3.API :<|> Raw

instance ToCapture (Capture "x" Int) where
  toCapture _ = DocCapture "x" "(integer) position on the x axis"

instance ToCapture (Capture "y" Int) where
  toCapture _ = DocCapture "y" "(integer) position on the y axis"

instance ToSample T3.Position T3.Position where
  toSample _ = Just (T3.Position 3 14)

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam "name"
                  ["Alp", "John Doe", "..."]
                  "Name of the person to say hello to."
                  Normal

instance ToSample T3.HelloMessage T3.HelloMessage where
  toSamples _ =
    [ ("When a value is provided for 'name'", T3.HelloMessage "Hello, Alp")
    , ("When 'name' is not specified", T3.HelloMessage "Hello, anonymous coward")
    ]

ci :: T3.ClientInfo
ci = T3.ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample T3.ClientInfo T3.ClientInfo where
  toSample _ = Just ci

instance ToSample T3.Email T3.Email where
  toSample _ = Just (T3.emailForClient ci)

api :: Proxy DocsAPI
api = Proxy

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] T3.api

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

server :: Server DocsAPI
server = T3.server :<|> serveDocs

  where serveDocs _ respond =
          respond $ responseLBS ok200 [plain] docsBS

        plain = ("Content-Type", "text/plain")

app :: Application
app = serve api server
