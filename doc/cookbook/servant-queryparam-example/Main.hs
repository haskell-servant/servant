{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Data (Proxy (..), Typeable)
import Data.Maybe (maybeToList)
import Data.OpenApi (OpenApi, ToParamSchema, ToSchema)
import Data.OpenApi.Internal.Utils (encodePretty)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, Context (..), GenericMode ((:-)), Get, Handler (..), JSON, NamedRoutes, ServerError, (:>))
import Servant.OpenApi (HasOpenApi (..))
import Servant.OpenApi.Record ()
import Servant.Record (RecordParam)
import Servant.Server.Generic (genericServe, genericServeTWithContext)
import Servant.Server.Record ()
import Servant.Symbols (DropPrefix, Eval, Exp)

-- | A label for dropping the prefix of a symbol
data DropPrefixExp :: Symbol -> Exp Symbol

type instance Eval (DropPrefixExp sym) = DropPrefix sym

-- | A label for keeping the prefix of a symbol
data KeepPrefixExp :: Symbol -> Exp Symbol

type instance Eval (KeepPrefixExp sym) = sym

-- | Query parameters as a record
data Params = Params {_get_user :: Maybe String, _get_users :: [String], _get_oneUser :: String, _get_userFlag :: Bool} deriving (Show, Generic, Typeable, ToJSON, ToSchema)

-- | User id
newtype UserId = UserId Integer deriving (Show, Generic, Typeable, ToJSON, ToSchema, ToParamSchema)

-- | API as a record. Prefixes of query parameters are dropped
newtype UserAPI1 routes = UserAPI1 {get :: routes :- "get" :> RecordParam DropPrefixExp Params :> Get '[JSON] [String]} deriving (Generic)

-- | API as a type synonym
type APIDrop = NamedRoutes UserAPI1

-- | API as a record. Prefixes of query parameters are kept
newtype UserAPI2 routes = UserAPI2 {get :: routes :- "get" :> RecordParam KeepPrefixExp Params :> Get '[JSON] [String]} deriving (Generic)

-- | API as a type synonym
type APIKeep = NamedRoutes UserAPI2

-- | 'OpenApi' specification for 'APIDrop'
specDrop :: OpenApi
specDrop = toOpenApi (Proxy :: Proxy APIDrop)

-- | 'OpenApi' specification for 'APIKeep'
specKeep :: OpenApi
specKeep = toOpenApi (Proxy :: Proxy APIKeep)

server :: Application
server =
  genericServeTWithContext
    (\x -> Servant.Handler (ExceptT $ Right <$> liftIO x))
    UserAPI1
      { get = \Params{..} -> pure $ maybeToList _get_user <> _get_users <> [_get_oneUser] <> [if _get_userFlag then "true" else "false"]
      }
    EmptyContext

main :: IO ()
main = do
  putStrLn "\n---\nQuery parameters without prefixes\n---\n"
  BSL8.putStrLn $ encodePretty specDrop
  putStrLn "\n---\nQuery parameters with prefixes\n---\n"
  BSL8.putStrLn $ encodePretty specKeep

  putStrLn "Starting the server..."
  putStrLn "\nTry running\n"
  putStrLn "curl -v \"localhost:8080/get?user=1&users=2&users=3&oneUser=4\""
  Warp.run 8080 server