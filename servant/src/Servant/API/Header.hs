{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Header (
    Header, Header',
    ) where

import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (Symbol)
import           Servant.API.Modifiers

-- | Extract the given header's value as a value of type @a@.
-- I.e. header sent by client, parsed by server.
--
-- Example:
--
-- >>> newtype Referer = Referer Text deriving (Eq, Show)
-- >>>
-- >>>            -- GET /view-my-referer
-- >>> type MyApi = "view-my-referer" :> Header "from" Referer :> Get '[JSON] Referer
type Header = Header' '[Optional, Strict]

data Header' (mods :: [*]) (sym :: Symbol) (a :: *)
    deriving Typeable

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
