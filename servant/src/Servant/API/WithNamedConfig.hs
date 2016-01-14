{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Servant.API.WithNamedConfig where

import GHC.TypeLits

data WithNamedConfig (name :: Symbol) (subConfig :: [*]) subApi
