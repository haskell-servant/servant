{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | This module contains the `RouterEnv env` type and associated functions.
-- `RouterEnv env` encapsulates the `env` type (as in `Router env a`),
-- which contains a tuple-encoded list of url pieces parsed from the incoming request.
-- The encapsulation makes it possible to pass more information throughout
-- the routing process, and ultimately to the computation of the `Delayed env c`
-- associated with each request.
-- The type and functions have been designed to be extensible: it should remain easy
-- to add a new field to the record and manipulate it.
--
-- @since 0.20
--
module Servant.Server.Internal.RouterEnv where

import           Data.Text
                 (Text)
import qualified Data.Text                                  as T
import           Data.Typeable
                 (TypeRep)
import           Network.HTTP.Types.Header
                 (HeaderName)

data RouterEnv env = RouterEnv
  { routedPath :: [PathPiece]
  , shouldReturnRoutedPath :: Bool
  , routerEnv :: env
  }
  deriving Functor

emptyEnv :: a -> RouterEnv a
emptyEnv v = RouterEnv [] False v

enableRoutingHeaders :: RouterEnv env -> RouterEnv env
enableRoutingHeaders env = env { shouldReturnRoutedPath = True }

routedPathRepr :: RouterEnv env -> Text
routedPathRepr RouterEnv{routedPath = path} =
    "/" <> T.intercalate "/" (map go $ reverse path)
  where
    go (StaticPiece p) = p
    go (CapturePiece p) = toCaptureTags p

data PathPiece
  = StaticPiece Text
  | CapturePiece [CaptureHint]

appendPathPiece :: PathPiece -> RouterEnv a -> RouterEnv a
appendPathPiece p env@RouterEnv{..} = env { routedPath = p:routedPath }

data CaptureHint = CaptureHint
  { captureName :: Text
  , captureType :: TypeRep
  }
  deriving (Show, Eq)

toCaptureTag :: CaptureHint -> Text
toCaptureTag hint = captureName hint <> "::" <> (T.pack . show) (captureType hint)

toCaptureTags :: [CaptureHint] -> Text
toCaptureTags hints = "<" <> T.intercalate "|" (map toCaptureTag hints) <> ">"

hRoutedPathHeader :: HeaderName
hRoutedPathHeader = "Servant-Routed-Path"
