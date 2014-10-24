{-# LANGUAGE OverloadedStrings #-}
module Servant.Text where

import Data.Text

class FromText a where
  fromText :: Text -> Maybe a

class ToText a where
  toText :: a -> Text

instance FromText Text where
  fromText = Just

instance ToText Text where
  toText = id

instance FromText Bool where
  fromText "true"  = Just True
  fromText "false" = Just False
  fromText _       = Nothing

instance ToText Bool where
  toText True  = "true"
  toText False = "false"