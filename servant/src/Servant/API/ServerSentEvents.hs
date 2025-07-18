{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

-- | Server-sent events
--
-- See <https://www.w3.org/TR/2009/WD-eventsource-20090421/>.
module Servant.API.ServerSentEvents
  ( ServerSentEvents'
  , ServerSentEvents
  , EventKind (..)
  )
where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import Network.HTTP.Types (StdMethod (GET))

-- | Determines the shape of events you may receive (i.e. the @a@ in
-- 'ServerSentEvents\'')
data EventKind
  = -- | 'EventMessage' or 'Event' 'ByteString'
    RawEvent
  | -- | Anything that implements 'FromJSON'
    JsonEvent

-- | Server-sent events (SSE)
--
-- See <https://www.w3.org/TR/2009/WD-eventsource-20090421/>.
data ServerSentEvents' (method :: k) (status :: Nat) (kind :: EventKind) (a :: Type)
  deriving (Generic, Typeable)

type ServerSentEvents = ServerSentEvents' 'GET 200
