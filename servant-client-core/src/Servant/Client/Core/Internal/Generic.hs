{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlapping-compat.h"

module Servant.Client.Core.Internal.Generic where

import Generics.SOP   (Code, Generic, I(..), NP(..), NS(Z), SOP(..), to)
import Servant.API    ((:<|>)(..))

-- | This class allows us to match client structure with client functions
-- produced with 'client' without explicit pattern-matching.
--
-- The client structure needs a 'Generics.SOP.Generic' instance.
--
-- Example:
--
-- > type API
-- >     = "foo" :> Capture "x" Int :> Get '[JSON] Int
-- >  :<|> "bar" :> QueryParam "a" Char :> QueryParam "b" String :> Post '[JSON] [Int]
-- >  :<|> Capture "nested" Int :> NestedAPI
-- >
-- > type NestedAPI
-- >     = Get '[JSON] String
-- >  :<|> "baz" :> QueryParam "c" Char :> Post '[JSON] ()
-- >
-- > data APIClient = APIClient
-- >   { getFoo         :: Int -> ClientM Int
-- >   , postBar        :: Maybe Char -> Maybe String -> ClientM [Int]
-- >   , mkNestedClient :: Int -> NestedClient
-- >   } deriving GHC.Generic
-- >
-- > instance Generics.SOP.Generic APIClient
-- > instance (Client API ~ client) => ClientLike client APIClient
-- >
-- > data NestedClient = NestedClient
-- >  { getString :: ClientM String
-- >  , postBaz   :: Maybe Char -> ClientM ()
-- >  } deriving GHC.Generic
-- >
-- > instance Generics.SOP.Generic NestedClient
-- > instance (Client NestedAPI ~ client) => ClientLike client NestedClient
-- >
-- > mkAPIClient :: APIClient
-- > mkAPIClient = mkClient (client (Proxy :: Proxy API))
--
-- By default, left-nested alternatives are expanded:
--
-- > type API1
-- >     = "foo" :> Capture "x" Int :> Get '[JSON] Int
-- >  :<|> "bar" :> QueryParam "a" Char :> Post '[JSON] String
-- >
-- > type API2
-- >     = "baz" :> QueryParam "c" Char :> Post '[JSON] ()
-- >
-- > type API = API1 :<|> API2
-- >
-- > data APIClient = APIClient
-- >   { getFoo  :: Int -> ClientM Int
-- >   , postBar :: Maybe Char -> ClientM String
-- >   , postBaz :: Maybe Char -> ClientM ()
-- >   } deriving GHC.Generic
-- >
-- > instance Generics.SOP.Generic APIClient
-- > instance (Client API ~ client) => ClientLike client APIClient
-- >
-- > mkAPIClient :: APIClient
-- > mkAPIClient = mkClient (client (Proxy :: Proxy API))
--
-- If you want to define client for @API1@ as a separate data structure,
-- you can use 'genericMkClientP':
--
-- > data APIClient1 = APIClient1
-- >   { getFoo  :: Int -> ClientM Int
-- >   , postBar :: Maybe Char -> ClientM String
-- >   } deriving GHC.Generic
-- >
-- > instance Generics.SOP.Generic APIClient1
-- > instance (Client API1 ~ client) => ClientLike client APIClient1
-- >
-- > data APIClient = APIClient
-- >   { mkAPIClient1 :: APIClient1
-- >   , postBaz      :: Maybe Char -> ClientM ()
-- >   } deriving GHC.Generic
-- >
-- > instance Generics.SOP.Generic APIClient
-- > instance (Client API ~ client) => ClientLike client APIClient where
-- >   mkClient = genericMkClientP
-- >
-- > mkAPIClient :: APIClient
-- > mkAPIClient = mkClient (client (Proxy :: Proxy API))
class ClientLike client custom where
  mkClient :: client -> custom
  default mkClient :: (Generic custom, Code custom ~ '[xs], GClientList client '[], GClientLikeL (ClientList client '[]) xs)
    => client -> custom
  mkClient = genericMkClientL

instance ClientLike client custom
      => ClientLike (a -> client) (a -> custom) where
  mkClient c = mkClient . c

-- | Match client structure with client functions, regarding left-nested API clients
-- as separate data structures.
class GClientLikeP client xs where
  gMkClientP :: client -> NP I xs

instance (GClientLikeP b (y ': xs), ClientLike a x)
      => GClientLikeP (a :<|> b) (x ': y ': xs) where
  gMkClientP (a :<|> b) = I (mkClient a) :* gMkClientP b

instance ClientLike a x => GClientLikeP a '[x] where
  gMkClientP a = I (mkClient a) :* Nil

-- | Match client structure with client functions, expanding left-nested API clients
-- in the same structure.
class GClientLikeL (xs :: [*]) (ys :: [*]) where
  gMkClientL :: NP I xs -> NP I ys

instance GClientLikeL '[] '[] where
  gMkClientL Nil = Nil

instance (ClientLike x y, GClientLikeL xs ys) => GClientLikeL (x ': xs) (y ': ys) where
  gMkClientL (I x :* xs) = I (mkClient x) :* gMkClientL xs

type family ClientList (client :: *) (acc :: [*]) :: [*] where
  ClientList (a :<|> b) acc = ClientList a (ClientList b acc)
  ClientList a acc = a ': acc

class GClientList client (acc :: [*]) where
  gClientList :: client -> NP I acc -> NP I (ClientList client acc)

instance (GClientList b acc, GClientList a (ClientList b acc))
  => GClientList (a :<|> b) acc where
  gClientList (a :<|> b) acc = gClientList a (gClientList b acc)

instance OVERLAPPABLE_ (ClientList client acc ~ (client ': acc))
  => GClientList client acc where
  gClientList c acc = I c :* acc

-- | Generate client structure from client type, expanding left-nested API (done by default).
genericMkClientL :: (Generic custom, Code custom ~ '[xs], GClientList client '[], GClientLikeL (ClientList client '[]) xs)
  => client -> custom
genericMkClientL = to . SOP . Z . gMkClientL . flip gClientList Nil

-- | Generate client structure from client type, regarding left-nested API clients as separate data structures.
genericMkClientP :: (Generic custom, Code custom ~ '[xs], GClientLikeP client xs)
  => client -> custom
genericMkClientP = to . SOP . Z . gMkClientP

