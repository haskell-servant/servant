{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GADTs #-}
module Servant.Client.Free (
    client,
    ClientF (..),
    module Servant.Client.Core.Reexport,
    ) where

import Data.Proxy (Proxy (..))
import Control.Monad.Free
import Servant.Client.Core
import Servant.Client.Core.Reexport
import Servant.Client.Core.Internal.ClientF

client :: HasClient (Free ClientF) api => Proxy api -> Client (Free ClientF) api
client api = api `clientIn` (Proxy :: Proxy (Free ClientF))
