{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant

type API =
  "species"
    :> ( Capture "species-name" Text
           :> ( Get '[JSON] Species
                  :<|> Delete '[JSON] ()
              )
           :<|> ReqBody '[JSON] Species :> Post '[JSON] ()
       )
    -- The plural of 'species' is unfortunately also 'species'
    :<|> "speciess" :> Get '[JSON] [Species]

api :: Proxy API
api = Proxy

data Species = Species
  { speciesName :: Text
  , speciesGenus :: Text
  }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data Genus = Genus
  { genusName :: Text
  , genusFamily :: Text
  }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

instance FromRow Genus

instance FromRow Species

server :: Connection -> Server API
server conn =
  ( ( \sname ->
        liftIO (lookupSpecies conn sname)
          :<|> liftIO (deleteSpecies conn sname)
    )
      :<|> (\species -> liftIO $ insertSpecies conn species)
  )
    :<|> (liftIO $ allSpecies conn)

lookupSpecies :: Connection -> Text -> IO Species
lookupSpecies conn name = do
  [s] <- query conn "SELECT * FROM species WHERE species_name = ?" (Only name)
  return s

deleteSpecies :: Connection -> Text -> IO ()
deleteSpecies conn name = do
  _ <- execute conn "DELETE FROM species WHERE species_name = ?" (Only name)
  return ()

insertSpecies :: Connection -> Species -> IO ()
insertSpecies conn Species{..} = do
  _ <- execute conn "INSERT INTO species (species_name, species_genus) VALUES (?)" (speciesName, speciesGenus)
  return ()

allSpecies :: Connection -> IO [Species]
allSpecies conn = do
  query_ conn "SELECT * FROM species"

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=servant-quickcheck"
  run 8090 (serve api $ server conn)
