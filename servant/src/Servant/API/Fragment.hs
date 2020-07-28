{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Fragment (Fragment) where

import           Data.Typeable
                 (Typeable)

-- | Lookup the value associated to the URI fragment
-- and try to extract it as a value of type @a@.
--
-- Example:
--
-- >>> -- /post#TRACKING
-- >>> type MyApi = "post" :> Fragment Text :> Get '[JSON] Tracking
data Fragment (a :: *)
    deriving Typeable

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Tracking
-- >>> instance ToJSON Tracking where { toJSON = undefined }
