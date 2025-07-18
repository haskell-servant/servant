{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_HADDOCK not-home #-}

module Servant.API.Fragment (Fragment) where

import Data.Kind (Type)
import Data.Typeable (Typeable)

-- | Document the URI fragment in API. Useful in combination with 'Link'.
--
-- Example:
--
-- >>> -- /post#TRACKING
-- >>> type MyApi = "post" :> Fragment Text :> Get '[JSON] Tracking
data Fragment (a :: Type)
  deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Tracking
-- >>> instance ToJSON Tracking where { toJSON = undefined }
