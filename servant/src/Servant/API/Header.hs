{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Header (
  Header(..),
) where

import           Data.ByteString (ByteString)
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (Symbol)
-- | Extract the given header's value as a value of type @a@.
--
-- Example:
--
-- >>> newtype Referer = Referer Text deriving (Eq, Show)
-- >>>
-- >>>            -- GET /view-my-referer
-- >>> type MyApi = "view-my-referer" :> Header "from" Referer :> Get '[JSON] Referer
data Header (sym :: Symbol) a = Header a
                              | MissingHeader
                              | UndecodableHeader ByteString
    deriving (Typeable, Eq, Show, Functor)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
