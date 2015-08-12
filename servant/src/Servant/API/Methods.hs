{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}

module Servant.API.Methods where

import           Data.Typeable (Typeable)
import           GHC.TypeLits (Nat, Symbol)

data HttpMethod (m :: Symbol) (s :: Nat) (contentTypes :: [*]) a 
    deriving Typeable
