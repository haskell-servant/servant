{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Client.Free (
    client,
    ClientF (..),
    module Servant.Client.Core.Reexport,
    ) where

import           Control.Monad.Free
import           Data.Proxy
                 (Proxy (..))
import           Servant.Client.Core
import           Servant.Client.Core.Reexport
import           Servant.Client.Core.RunClient

client :: HasClient (Free ClientF) api => Proxy api -> Client (Free ClientF) api
client api = api `clientIn` (Proxy :: Proxy (Free ClientF))
