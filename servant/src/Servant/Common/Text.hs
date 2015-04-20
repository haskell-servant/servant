{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Servant.Common.Text
  ( FromText(..)
  , ToText(..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative     ((<$>))
#endif
import           Data.Int                (Int16, Int32, Int64, Int8)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Text.Read          (Reader, decimal, rational, signed)
import           Data.Word               (Word16, Word32, Word64, Word8
#if !MIN_VERSION_base(4,8,0)
        , Word
#endif
        )

-- | For getting values from url captures and query string parameters
-- Instances should obey:
-- > fromText (toText a) == Just a
class FromText a where
  fromText :: Text -> Maybe a

-- | For putting values in paths and query string parameters
-- Instances should obey:
-- > fromText (toText a) == Just a
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

-- |
-- >>> fromText ("true"::Text) :: Maybe Bool
-- Just True
-- >>> fromText ("false"::Text) :: Maybe Bool
-- Just False
-- >>> fromText ("anything else"::Text) :: Maybe Bool
-- Nothing
instance FromText Bool where
  fromText "true"  = Just True
  fromText "false" = Just False
  fromText _       = Nothing

-- |
-- >>> toText True
-- "true"
-- >>> toText False
-- "false"
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
  fromText = runReader (signed decimal)

instance ToText Integer where
  toText = cs . show

instance FromText Double where
  fromText x = fromRational <$> runReader rational x

instance ToText Double where
  toText = cs . show

instance FromText Float where
  -- Double is more practically accurate due to weird rounding when using
  -- rational. We convert to double and then convert to Float.
  fromText x = fromRational <$> runReader rational x

instance ToText Float where
  toText = cs . show

runReader :: Reader a -> Text -> Maybe a
runReader reader t = either (const Nothing) (Just . fst) $ reader t
