{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Servant.Text where

import Data.String.Conversions
import Data.Text

class FromText a where
  fromText :: Text -> Maybe a

class ToText a where
  toText :: a -> Text

instance FromText Text where
  fromText = Just

instance ToText Text where
  toText = id

instance FromText String where
  fromText = Just . cs

instance ToText String where
  toText = cs

instance FromText Bool where
  fromText "true"  = Just True
  fromText "false" = Just False
  fromText _       = Nothing

instance ToText Bool where
  toText True  = "true"
  toText False = "false"
