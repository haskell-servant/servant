{-# LANGUAGE DeriveTraversable #-}

-- | This module offers other servant libraries a minimalistic HTTP response type.
--
-- It is purely an internal API and SHOULD NOT be used by end-users of Servant.
module Servant.Types.Internal.Response where

import Data.Data (Typeable)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Network.HTTP.Types (Header, Status)

data InternalResponse a = InternalResponse
  { statusCode :: Status
  , headers :: Seq Header
  , responseBody :: a
  }
  deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable, Typeable)
