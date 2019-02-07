{-# LANGUAGE DeriveFunctor #-}
module Servant.Client.Core.Internal.ClientF where

import           Servant.Client.Core.Internal.Request
import           Servant.Client.Core.Internal.Response
import           Servant.Client.Core.Internal.ClientError

data ClientF a
    = RunRequest Request (Response -> a)
    | StreamingRequest Request (StreamingResponse -> a)
    | Throw ServantError
  deriving (Functor)
