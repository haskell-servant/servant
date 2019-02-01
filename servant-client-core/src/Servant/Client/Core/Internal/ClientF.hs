{-# LANGUAGE DeriveFunctor #-}
module Servant.Client.Core.Internal.ClientF where

import           Servant.Client.Core.Internal.Request

data ClientF a
    = RunRequest Request (Response -> a)
    | StreamingRequest Request (StreamingResponse -> a)
    | Throw ServantError
  deriving (Functor)
