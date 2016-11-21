{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Client.Generic
  ( ClientLike(..)
  , genericMkClient
  ) where

import Generics.SOP   (Generic, I(..), NP(..), NS(Z), Rep, SOP(..), to)
import Servant.API    ((:<|>)(..))
import Servant.Client (ClientM)

-- | This class allows us to match client structure with client functions
-- produced with 'client' without explicit pattern-matching.
--
-- The client structure needs a 'Generics.SOP.Generic' instance.
--
-- Example:
--
-- type API
--     = "foo" :> Capture "x" Int :> Get '[JSON] Int
--  :<|> "bar" :> QueryParam "a" Char :> QueryParam "b" String :> Post '[JSON] [Int]
--  :<|> Captre "nested" Int :> NestedAPI
--
-- type NestedAPI
--     = Get '[JSON] String
--  :<|> "baz" :> QueryParam "c" Char :> Post '[JSON] ()
--
-- data APIClient = APIClient
--   { getFoo         :: Int -> Manager -> BaseUrl -> ClientM Int
--   , postBar        :: Maybe Char -> Maybe String -> Manager -> BaseUrl -> ClientM [Int]
--   , mkNestedClient :: Int -> NestedClient
--   } deriving GHC.Generic
--
-- instance Generic.SOP.Generic APIClient
-- instance (Client API ~ client) => ClientLike client APIClient
--
-- data NestedClient = NestedClient
--  { getString :: Manager -> BaseUrl -> ClientM String
--  , postBaz   :: Maybe Char -> Manager -> BaseUrl -> ClientM ()
--  } deriving GHC.Generic
--
-- instance Generic.SOP.Generic
-- instance (Client NestedAPI ~ client) => ClientLike client NestedAPI
--
-- mkAPIClient :: APIClient
-- mkAPIClient = mkClient (client (Proxy :: Proxy API))
class ClientLike client custom where
  mkClient :: client -> custom
  default mkClient :: (Generic custom, GClientLikeP client xs, SOP I '[xs] ~ Rep custom)
    => client -> custom
  mkClient = genericMkClient

instance ClientLike client custom
      => ClientLike (a -> client) (a -> custom) where
  mkClient c = mkClient . c

instance ClientLike (ClientM a) (ClientM a) where
  mkClient = id

-- | This class is used to match client functions to the
-- representation of client structure type as sum of products
-- and basically does all the internal job to build this structure.
class GClientLikeP client xs where
  gMkClientP :: client -> NP I xs

instance (GClientLikeP b (y ': xs), ClientLike a x)
      => GClientLikeP (a :<|> b) (x ': y ': xs) where
  gMkClientP (a :<|> b) = I (mkClient a) :* gMkClientP b

instance ClientLike a x => GClientLikeP a '[x] where
  gMkClientP a = I (mkClient a) :* Nil

-- | Generate client structure from client type.
genericMkClient :: (Generic custom, GClientLikeP client xs, SOP I '[xs] ~ Rep custom)
  => client -> custom
genericMkClient = to . SOP . Z . gMkClientP

