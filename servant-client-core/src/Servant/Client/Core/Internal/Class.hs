{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-| Types for possible backends to run client-side `Request` queries -}
module Servant.Client.Core.Internal.Class where

import           Control.Monad.Error.Class            (MonadError)
import           Servant.Client.Core.Internal.Request (Request, Response,
                                                       ServantError)

class (MonadError ServantError m) => RunClient m where
  runRequest :: Request -> m Response
