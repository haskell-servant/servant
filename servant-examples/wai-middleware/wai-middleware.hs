{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant

data Product = Product
  { name              :: Text
  , brand             :: Text
  , current_price_eur :: Double
  , available         :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Product

products :: [Product]
products = [p1, p2]

  where p1 = Product "Haskell laptop sticker"
                     "GHC Industries"
                     2.50
                     True

        p2 = Product "Foldable USB drive"
                     "Well-Typed"
                     13.99
                     False

type SimpleAPI = Get '[JSON] [Product]

simpleAPI :: Proxy SimpleAPI
simpleAPI = Proxy

server :: Server SimpleAPI
server = return products

-- logStdout :: Middleware
-- i.e, logStdout :: Application -> Application
-- serve :: Proxy api -> Server api -> Application
-- so applying a middleware is really as simple as
-- applying a function to the result of 'serve'
app :: Application
app = logStdout (serve simpleAPI server)

main :: IO ()
main = run 8080 app
