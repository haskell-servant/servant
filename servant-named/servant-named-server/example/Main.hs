{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Aeson (ToJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Data (Proxy (..), Typeable)
import Data.OpenApi (OpenApi, ToParamSchema, ToSchema)
import Data.OpenApi.Internal.Utils (encodePretty)
import GHC.Generics (Generic)
import Servant (GenericMode ((:-)), Get, JSON, NamedRoutes, (:>))
import Servant.Named ()
import Servant.OpenApi (HasOpenApi (..))
import Servant.Record (RecordParam, UnRecordParam)
import Servant.Server.Named ()
import Servant.Server.Record ()
import Servant.Symbols (DropWhile, DropWhileNot, Exp, Modify, Symbol)

type family ModDropPrefix (sym :: Symbol) :: Symbol where
  ModDropPrefix sym = DropWhile "_" (DropWhileNot "_" (DropWhile "_" sym))

data ModifyPrefix1 :: Symbol -> Exp Symbol
data ModifyPrefix2 :: Symbol -> Exp Symbol

instance HasOpenApi (UnRecordParam mkExp (RecordParam mkExp a :> api)) => HasOpenApi (RecordParam mkExp a :> api) where
  toOpenApi :: Proxy (RecordParam mkExp a :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnRecordParam mkExp (RecordParam mkExp a :> api)))

newtype Params = Params {_get_user :: Maybe String} deriving (Show, Generic, Typeable, ToJSON, ToSchema)

newtype UserId = UserId Integer deriving (Show, Generic, Typeable, ToJSON, ToSchema, ToParamSchema)

type instance Modify (ModifyPrefix1 sym) = ModDropPrefix sym
newtype GetUser1 routes = GetUser1 {get :: routes :- "get" :> RecordParam ModifyPrefix1 Params :> Get '[JSON] Int} deriving (Generic)
type API1 = NamedRoutes GetUser1

newtype GetUser2 routes = GetUser2 {get :: routes :- "get" :> RecordParam ModifyPrefix2 Params :> Get '[JSON] Int} deriving (Generic)
type API2 = NamedRoutes GetUser2
type instance Modify (ModifyPrefix2 sym) = sym

spec1 :: OpenApi
spec1 = toOpenApi (Proxy :: Proxy API1)

spec2 :: OpenApi
spec2 = toOpenApi (Proxy :: Proxy API2)

main :: IO ()
main = do
  putStrLn "\n---\nQuery parameters without prefixes\n---\n"
  BSL8.putStrLn $ encodePretty spec1
  putStrLn "\n---\nQuery parameters with prefixes\n---\n"
  BSL8.putStrLn $ encodePretty spec2
