{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Empty(EmptyAPI(..)) where

import           Data.Typeable
                 (Typeable)
import           Prelude ()
import           Prelude.Compat

-- | An empty API: one which serves nothing. Morally speaking, this should be
-- the unit of ':<|>'. Implementors of interpretations of API types should
-- treat 'EmptyAPI' as close to the unit as possible.
data EmptyAPI = EmptyAPI deriving (Typeable, Eq, Show, Bounded, Enum)
