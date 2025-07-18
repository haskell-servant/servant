{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Types for possible backends to run client-side `Request` queries
module Servant.Client.Core.RunClient
  ( RunClient (..)
  , runRequest
  , RunStreamingClient (..)
  , ClientF (..)
  )
where

import Control.Monad.Free (Free (..), liftF)
import Network.HTTP.Types.Status (Status)
import Prelude.Compat
import Prelude ()

import Servant.Client.Core.ClientError
import Servant.Client.Core.Request
import Servant.Client.Core.Response

class Monad m => RunClient m where
  -- | How to make a request, with an optional list of status codes to not throw exceptions
  -- for (default: [200..299]).
  runRequestAcceptStatus :: Maybe [Status] -> Request -> m Response

  throwClientError :: ClientError -> m a

-- | How to make a request.
runRequest :: RunClient m => Request -> m Response
runRequest = runRequestAcceptStatus Nothing

class RunClient m => RunStreamingClient m where
  withStreamingRequest :: Request -> (StreamingResponse -> IO a) -> m a

-------------------------------------------------------------------------------
-- Free
-------------------------------------------------------------------------------

-- | 'ClientF' cannot stream.
--
-- Compare to 'RunClient'.
data ClientF a
  = RunRequest Request (Response -> a)
  | Throw ClientError
  deriving (Functor)

-- TODO: honour the accept-status argument.
instance ClientF ~ f => RunClient (Free f) where
  runRequestAcceptStatus _ req = liftF (RunRequest req id)
  throwClientError = liftF . Throw
