{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types for possible backends to run client-side `Request` queries
module Servant.Client.Core.RunClient (
    RunClient (..),
    RunStreamingClient (..),
    ClientF (..),
    ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Free
                 (Free (..), liftF)

import           Servant.Client.Core.ClientError
import           Servant.Client.Core.Request
import           Servant.Client.Core.Response

class Monad m => RunClient m where
  -- | How to make a request.
  runRequest :: Request -> m Response
  throwServantError :: ServantError -> m a

class RunClient m =>  RunStreamingClient m where
    withStreamingRequest :: Request -> (StreamingResponse -> IO a) ->  m a

-------------------------------------------------------------------------------
-- Free
-------------------------------------------------------------------------------

-- | 'ClientF' cannot stream.
--
-- Compare to 'RunClient'.
data ClientF a
    = RunRequest Request (Response -> a)
    | Throw ServantError
  deriving (Functor)

instance ClientF ~ f => RunClient (Free f) where
    runRequest req  = liftF (RunRequest req id)
    throwServantError = liftF . Throw
