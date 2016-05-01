#line 158 "Announcement.anansi"

#line 37 "Announcement.anansi"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Servant
import Data.Aeson
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import Data.Text (Text)
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)

type API
  = "species" :> (Capture "species-name" Text :> ( Get '[JSON] Species
                                              :<|> Delete '[JSON] ())
             :<|> ReqBody '[JSON] Species :> Post '[JSON] ())
  -- The plural of 'species' is unfortunately also 'species'
 :<|> "speciess" :> Get '[JSON] [Species]

api :: Proxy API
api = Proxy

data Species = Species
  { speciesName  :: Text
  , speciesGenus :: Text
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data Genus = Genus
  { genusName   :: Text
  , genusFamily :: Text
  } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance FromRow Genus
instance FromRow Species

server :: Connection -> Server API
server conn = ((\sname -> liftIO (lookupSpecies conn sname)
                     :<|> liftIO (deleteSpecies conn sname))
         :<|> (\species -> liftIO $ insertSpecies conn species))
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
