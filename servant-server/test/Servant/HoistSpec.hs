{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Servant.HoistSpec where

import           Data.Aeson
                 (ToJSON, encode)
import           Test.Hspec
                 (Spec)

import           Servant

-------------------------------------------------------------------------------
-- https://github.com/haskell-servant/servant/issues/734
-------------------------------------------------------------------------------

-- This didn't fail if executed in GHCi; cannot have as a doctest.

newtype App a = App a

type API = Get '[JSON] Int
    :<|> ReqBody '[JSON] String :> Get '[JSON] Bool

api :: Proxy API
api = Proxy

server :: App Int :<|> (String -> App Bool)
server = undefined

-- Natural transformation still seems to need an explicit type.
f :: App a -> App a
f = id

server' :: App Int :<|> (String -> App Bool)
server' = hoistServer api f server

instance ToJSON a => MimeRender JSON a where mimeRender _ = encode

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = return ()
