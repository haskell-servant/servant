{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Servant.Utils.Text where

import Data.String.Conversions
import Data.Int
import Data.Text
import Data.Text.Read
import Data.Word

-- | For getting values from url captures, get parameters
class FromText a where
  fromText :: Text -> Maybe a

-- | For putting values in paths, get parameters
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

instance FromText Int where
  fromText = runReader (signed decimal)

instance ToText Int where
  toText = cs . show

instance FromText Int8 where
  fromText = runReader (signed decimal)

instance ToText Int8 where
  toText = cs . show

instance FromText Int16 where
  fromText = runReader (signed decimal)

instance ToText Int16 where
  toText = cs . show

instance FromText Int32 where
  fromText = runReader (signed decimal)

instance ToText Int32 where
  toText = cs . show

instance FromText Int64 where
  fromText = runReader (signed decimal)

instance ToText Int64 where
  toText = cs . show

instance FromText Word where
  fromText = runReader decimal

instance ToText Word where
  toText = cs . show

instance FromText Word8 where
  fromText = runReader decimal

instance ToText Word8 where
  toText = cs . show

instance FromText Word16 where
  fromText = runReader decimal

instance ToText Word16 where
  toText = cs . show

instance FromText Word32 where
  fromText = runReader decimal

instance ToText Word32 where
  toText = cs . show

instance FromText Word64 where
  fromText = runReader decimal

instance ToText Word64 where
  toText = cs . show

instance FromText Integer where
  fromText = runReader decimal

instance ToText Integer where
  toText = cs . show

instance FromText Double where
  fromText = runReader rational

instance ToText Double where
  toText = cs . show

instance FromText Float where
  fromText = runReader rational

instance ToText Float where
  toText = cs . show

runReader :: Reader a -> Text -> Maybe a
runReader reader t = either (const Nothing) (Just . fst) $ reader t
