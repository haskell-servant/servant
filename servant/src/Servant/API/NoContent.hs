{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
module Servant.API.NoContent
    ( module Servant.API.NoContent
    ) where

import           GHC.Generics
                 (Generic)
import           GHC.TypeLits
                 (Nat)

-- | A type for responses without content-body.
data NoContent (statusCode :: Nat) = NoContent
  deriving (Show, Eq, Read, Generic)
