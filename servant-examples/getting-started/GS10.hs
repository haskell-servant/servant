{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GS10 where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Docs
import qualified GS3

type DocsAPI = GS3.API :<|> Raw

instance ToCapture (Capture "x" Int) where
  toCapture _ = DocCapture "x" "(integer) position on the x axis"

instance ToCapture (Capture "y" Int) where
  toCapture _ = DocCapture "y" "(integer) position on the y axis"

instance ToSample GS3.Position GS3.Position where
  toSample _ = Just (GS3.Position 3 14)

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam "name"
                  ["Alp", "John Doe", "..."]
                  "Name of the person to say hello to."
                  Normal

instance ToSample GS3.HelloMessage GS3.HelloMessage where
  toSamples _ =
    [ ("When a value is provided for 'name'", GS3.HelloMessage "Hello, Alp")
    , ("When 'name' is not specified", GS3.HelloMessage "Hello, anonymous coward")
    ]

ci :: GS3.ClientInfo
ci = GS3.ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample GS3.ClientInfo GS3.ClientInfo where
  toSample _ = Just ci

instance ToSample GS3.Email GS3.Email where
  toSample _ = Just (GS3.emailForClient ci)

api :: Proxy DocsAPI
api = Proxy

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] GS3.api

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

server :: Server DocsAPI
server = GS3.server :<|> serveDocs

  where serveDocs _ respond =
          respond $ responseLBS ok200 [plain] docsBS

        plain = ("Content-Type", "text/plain")

app :: Application
app = serve api server
