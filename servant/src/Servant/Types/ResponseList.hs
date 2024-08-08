{-# LANGUAGE DeriveTraversable #-}

module Servant.Types.ResponseList where

import Network.HTTP.Types (Status, Header)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Data.Data (Typeable)

data InternalResponse a = InternalResponse
  { statusCode :: Status
  , headers :: Seq Header
  , responseBody :: a
  } deriving stock (Eq, Show, Generic, Typeable, Functor, Foldable, Traversable)
