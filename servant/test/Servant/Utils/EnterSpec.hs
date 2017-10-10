{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Servant.Utils.EnterSpec where

import Test.Hspec (Spec)

import Servant.API
import Servant.Utils.Enter

-------------------------------------------------------------------------------
-- https://github.com/haskell-servant/servant/issues/734
-------------------------------------------------------------------------------

-- This didn't fail if executed in GHCi; cannot have as a doctest.

data App a

f :: App :~> App
f = NT id

server :: App Int :<|> (String -> App Bool)
server = undefined

server' :: App Int :<|> (String -> App Bool)
server' = enter f server

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = return ()
