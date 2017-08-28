{-# LANGUAGE MultiParamTypeClasses #-}
{-| Types for possible backends to run client-side `Req` queries -}
module Servant.Client.Class
  (RunClient(..))
where

import Data.Proxy
import Network.HTTP.Types
import Servant.API
import Servant.Common.Req

class (Monad m) => RunClient m ct result where
  runRequest :: MimeUnrender ct result
             => Proxy ct
             -> Method -> Req -> m result
