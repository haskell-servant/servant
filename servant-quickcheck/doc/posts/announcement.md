
# Announcing servant-quickcheck

Some time ago, we released `servant-mock`. The idea behind it is to use
`QuickCheck` to create a mock server that accords with a servant API. Not long
after, we started thinking about an analog that would, instead of mocking a
server, mock a client instead - i.e., generate random requests that conform to
an API description.

This is much closer to the traditional use of `QuickCheck`. The most obvious
use-case is checking that properties hold of an *entire* server rather than of
individual endpoints.

## `serverSatisfies`

There are a variety of best practices in writing web APIs that aren't always
obvious. As a running example, let's use a simple service that allows adding,
removing, and querying biological species. Our SQL schema is:


> **«schema.sql»**

>     
>     CREATE TABLE genus (
>         genus_name     text  PRIMARY KEY,
>         genus_family   text  NOT NULL
>     )
>     
>     CREATE TABLE species (
>         species_name    text  PRIMARY KEY,
>         species_genus   text  NOT NULL REFERENCES genus (genus_name)
>     )


And our actual application:


> **«Main.hs»**

>     {-# LANGUAGE DataKinds #-}
>     {-# LANGUAGE DeriveAnyClass #-}
>     {-# LANGUAGE DeriveGeneric #-}
>     {-# LANGUAGE TypeOperators #-}
>     {-# LANGUAGE OverloadedStrings #-}
>     {-# LANGUAGE RecordWildCards #-}
>     import Servant
>     import Data.Aeson
>     import Database.PostgreSQL.Simple
>     import GHC.Generics (Generic)
>     import Data.Text (Text)
>     import Network.Wai.Handler.Warp
>     import Control.Monad.IO.Class (liftIO)
>     
>     type API
>       = "species" :> (Capture "species-name" Text :> ( Get '[JSON] Species
>                                                   :<|> Delete '[JSON] ())
>                  :<|> ReqBody '[JSON] Species :> Post '[JSON] ()
>                  :<|> "count" :> Get '[JSON] Int)
>     
>     api :: Proxy API
>     api = Proxy
>     
>     data Species = Species
>       { speciesName  :: Text
>       , speciesGenus :: Text
>       } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
>     
>     data Genus = Genus
>       { genusName   :: Text
>       , genusFamily :: Text
>       } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
>     
>     instance FromRow Genus
>     instance FromRow Species
>     
>     server :: Connection -> Server API
>     server conn = (\sname -> liftIO (lookupSpecies conn sname)
>                         :<|> liftIO (deleteSpecies conn sname))
>              :<|> (\species -> liftIO $ insertSpecies conn species)
>              :<|> (liftIO $ countSpecies conn)
>     
>     lookupSpecies :: Connection -> Text -> IO Species
>     lookupSpecies conn name = do
>       [s] <- query conn "SELECT * FROM species WHERE species_name == ?" (Only name)
>       return s
>     
>     deleteSpecies :: Connection -> Text -> IO ()
>     deleteSpecies conn name = do
>       _ <- execute conn "DELETE FROM species WHERE species_name == ?" (Only name)
>       return ()
>     
>     insertSpecies :: Connection -> Species -> IO ()
>     insertSpecies conn Species{..} = do
>       _ <- execute conn "INSERT INTO species (species_name, species_genus) VALUES (?)" (speciesName, speciesGenus)
>       return ()
>     
>     countSpecies :: Connection -> IO Int
>     countSpecies conn = do
>       [Only count] <- query_ conn "SELECT count(*) FROM species"
>       return count
>     
>     main :: IO ()
>     main = do
>       conn <- connectPostgreSQL ""
>       run 8090 (serve api $ server conn)



> **» Main.hs**

>     «Main.hs»



> **» schema.sql**

>     «schema.sql»

