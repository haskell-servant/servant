{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Empty(EmptyAPI(..)) where

import           Data.Typeable    (Typeable)
import           Prelude ()
import           Prelude.Compat

-- | An empty API: one which serves nothing.
data EmptyAPI = EmptyAPI deriving (Typeable, Eq, Show, Bounded)
