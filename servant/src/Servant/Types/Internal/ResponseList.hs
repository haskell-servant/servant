{-# LANGUAGE DeriveTraversable #-}

-- | This module offers other servant libraries a minimalistic HTTP response type.
--
-- It is purely an internal API and SHOULD NOT be used by end-users of Servant.
module Servant.Types.Internal.ResponseList where

import Network.HTTP.Types (Status, Header)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Data.Data (Typeable)

data InternalResponse a = InternalResponse
  { statusCode :: Status
  , headers :: Seq Header
  , responseBody :: a
  } deriving stock (Eq, Show, Generic, Typeable, Functor, Foldable, Traversable)
