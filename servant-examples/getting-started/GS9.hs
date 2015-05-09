{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module GS9 where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Char
import Data.List
import GHC.Generics
import qualified Language.Javascript.JQuery as JQ
import Math.Probable
import Network.Wai
import Servant
import Servant.JQuery

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point

randomPoint :: MonadIO m => m Point
randomPoint = liftIO . mwc $ Point <$> d <*> d
  
  where d = doubleIn (-1, 1)

data Search a = Search
  { query   :: String
  , results :: [a]
  } deriving Generic

mkSearch :: String -> [a] -> Search a
mkSearch = Search

instance ToJSON a => ToJSON (Search a)

data Book = Book
  { author :: String
  , title  :: String
  , year   :: Int
  } deriving Generic

instance ToJSON Book

book :: String -> String -> Int -> Book
book = Book

books :: [Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
  , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
  , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
  , book "Graham Hutton" "Programming in Haskell" 2007
  , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
  , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]

searchBook :: Monad m => Maybe String -> m (Search Book)
searchBook Nothing  = return (mkSearch "" books)
searchBook (Just q) = return (mkSearch q books')

  where books' = filter (\b -> q' `isInfixOf` map toLower (author b)
                            || q' `isInfixOf` map toLower (title b)
                        )
                        books
        q' = map toLower q

type API = "point" :> Get '[JSON] Point
      :<|> "books" :> QueryParam "q" String :> Get '[JSON] (Search Book)

type API' = API :<|> Raw

api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: Server API
server = randomPoint
    :<|> searchBook

server' :: Server API'
server' = server
     :<|> serveDirectory "getting-started/gs9"

apiJS :: String
apiJS = jsForAPI api

writeJSFiles :: IO ()
writeJSFiles = do
  writeFile "getting-started/gs9/api.js" apiJS
  jq <- readFile =<< JQ.file
  writeFile "getting-started/gs9/jq.js" jq

app :: Application
app = serve api' server'
