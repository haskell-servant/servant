{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Data (Proxy (..), Typeable)
import Data.OpenApi (OpenApi, ToParamSchema, ToSchema)
import Data.OpenApi.Internal.Utils (encodePretty)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Servant (GenericMode ((:-)), Get, JSON, NamedRoutes, (:>))
import Servant.Named ()
import Servant.OpenApi (HasOpenApi (..))
import Servant.Record (RecordParam, UnRecordParam)
import Servant.Server.Named ()
import Servant.Server.Record ()
import Servant.Symbols (DropPrefix, Eval, Exp)

-- | Instance of 'HasOpenAPI' for any 'RecordParam'
instance HasOpenApi (UnRecordParam mod (RecordParam mod a :> api)) => HasOpenApi (RecordParam mod a :> api) where
  toOpenApi :: Proxy (RecordParam mod a :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnRecordParam mod (RecordParam mod a :> api)))

-- | A label for dropping the prefix of a symbol
data DropPrefixExp :: Symbol -> Exp Symbol

type instance Eval (DropPrefixExp sym) = DropPrefix sym

-- | A label for keeping the prefix of a symbol
data KeepPrefixExp :: Symbol -> Exp Symbol

type instance Eval (KeepPrefixExp sym) = sym

-- | Query parameters as a record
newtype Params = Params {_get_user :: Maybe String} deriving (Show, Generic, Typeable, ToJSON, ToSchema)

-- | User id
newtype UserId = UserId Integer deriving (Show, Generic, Typeable, ToJSON, ToSchema, ToParamSchema)

-- | API as a record. Prefixes of query parameters are dropped
newtype GetUser1 routes = GetUser1 {get :: routes :- "get" :> RecordParam DropPrefixExp Params :> Get '[JSON] Int} deriving (Generic)

-- | API as a type synonym
type APIDrop = NamedRoutes GetUser1

-- | API as a record. Prefixes of query parameters are kept
newtype GetUser2 routes = GetUser2 {get :: routes :- "get" :> RecordParam KeepPrefixExp Params :> Get '[JSON] Int} deriving (Generic)

-- | API as a type synonym
type APIKeep = NamedRoutes GetUser2

-- | 'OpenApi' specification for 'APIDrop'
specDrop :: OpenApi
specDrop = toOpenApi (Proxy :: Proxy APIDrop)

-- | 'OpenApi' specification for 'APIKeep'
specKeep :: OpenApi
specKeep = toOpenApi (Proxy :: Proxy APIKeep)

main :: IO ()
main = do
  putStrLn "\n---\nQuery parameters without prefixes\n---\n"
  BSL8.putStrLn $ encodePretty specDrop
  putStrLn "\n---\nQuery parameters with prefixes\n---\n"
  BSL8.putStrLn $ encodePretty specKeep
