{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_HADDOCK not-home    #-}

module Servant.API.Methods where

import           Data.Typeable (Typeable)
import           GHC.TypeLits (Nat, Symbol)

data HttpMethod (m :: Symbol) (contentTypes :: [*]) a 
    deriving Typeable

type family DefaultStatusCode (m :: Symbol) :: Nat
