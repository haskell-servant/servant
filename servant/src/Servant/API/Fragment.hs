{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Fragment (Fragment, Fragment') where

import           Data.Typeable
                 (Typeable)
import           Servant.API.Modifiers

-- | Lookup the value associated to the URI fragment
-- and try to extract it as a value of type @a@.
-- Lenient by default.
--
-- Example:
--
-- >>> -- /post#TRACKING
-- >>> type MyApi = "post" :> Fragment Text :> Get '[JSON] Tracking
type Fragment a = Fragment' '[Lenient] a

-- | 'Fragment' which can be 'Lenient' or 'Strict'.
data Fragment' (mods :: [*]) (a :: *)
    deriving Typeable

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Tracking
-- >>> instance ToJSON Tracking where { toJSON = undefined }
