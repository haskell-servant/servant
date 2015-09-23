{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module T4 where

import Data.Aeson
import Data.Foldable (foldMap)
import GHC.Generics
import Lucid
import Network.Wai
import Servant
import Servant.HTML.Lucid

data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  } deriving Generic -- for the JSON instance

-- JSON serialization
instance ToJSON Person

-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)
      td_ (toHtml . show $ age person)

  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml ps = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"
      th_ "age"

    foldMap toHtml ps

  toHtmlRaw = toHtml

persons :: [Person]
persons =
  [ Person "Isaac"  "Newton"   372
  , Person "Albert" "Einstein" 136
  ]

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server :: Server PersonAPI
server = return persons

app :: Application
app = serve personAPI server
