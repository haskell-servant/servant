{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Servant.API.Sub where

import Data.Proxy
import GHC.TypeLits

-- | The contained API (second argument) can be found under @("/" ++ path)@
-- (path being the first argument).
--
-- Example:
--
-- > -- GET /hello/world
-- > -- returning a JSON encoded World value
-- > type MyApi = "hello" :> "world" :> Get World
data (path :: k) :> a = Proxy path :> a
infixr 9 :>
