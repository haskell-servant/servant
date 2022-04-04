{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Servant.API.RedirectOf where

import Data.Proxy (Proxy(..))
import Servant.Links

data RedirectOf api where
   RedirectOf
     :: (IsElem endpoint api, HasLink endpoint)
     => Proxy endpoint -> (forall a. MkLink endpoint a -> a) -> RedirectOf api
