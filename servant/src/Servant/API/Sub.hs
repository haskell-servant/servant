{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Sub ((:>)) where

import           Data.Typeable
                 (Typeable)

-- | The contained API (second argument) can be found under @("/" ++ path)@
-- (path being the first argument).
--
-- Example:
--
-- >>> -- GET /hello/world
-- >>> -- returning a JSON encoded World value
-- >>> type MyApi = "hello" :> "world" :> Get '[JSON] World
data (path :: k) :> (a :: *)
    deriving (Typeable)
infixr 4 :>

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data World
-- >>> instance ToJSON World where { toJSON = undefined }
