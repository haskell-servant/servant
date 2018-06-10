{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE NullaryTypeClasses #-}
#endif

-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign.Internal where

import Prelude ()
import Prelude.Compat

import           Control.Lens (makePrisms, makeLenses, Getter, (&), (<>~), (%~),
                               (.~))
import           Data.Data (Data)
import           Data.Proxy
import           Data.Semigroup (Semigroup)
import           Data.String
import           Data.Text
import           Data.Typeable (Typeable)
import           Data.Text.Encoding (decodeUtf8)
import           GHC.TypeLits
import qualified Network.HTTP.Types as HTTP
import           Servant.API
import           Servant.API.TypeLevel
import           Servant.API.Modifiers (RequiredArgument)

newtype FunctionName = FunctionName { unFunctionName :: [Text] }
  deriving (Data, Show, Eq, Semigroup, Monoid, Typeable)

makePrisms ''FunctionName

newtype PathSegment = PathSegment { unPathSegment :: Text }
  deriving (Data, Show, Eq, IsString, Semigroup, Monoid, Typeable)

makePrisms ''PathSegment

data Arg f = Arg
  { _argName :: PathSegment
  , _argType :: f }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''Arg

argPath :: Getter (Arg f) Text
argPath = argName . _PathSegment

data SegmentType f
  = Static PathSegment
    -- ^ a static path segment. like "/foo"
  | Cap (Arg f)
    -- ^ a capture. like "/:userid"
  deriving (Data, Eq, Show, Typeable)

makePrisms ''SegmentType

newtype Segment f = Segment { unSegment :: SegmentType f }
  deriving (Data, Eq, Show, Typeable)

makePrisms ''Segment

isCapture :: Segment f -> Bool
isCapture (Segment (Cap _)) = True
isCapture                _  = False

captureArg :: Segment f -> Arg f
captureArg (Segment (Cap s)) = s
captureArg                 _ = error "captureArg called on non capture"

type Path f = [Segment f]

data ArgType
  = Normal
  | Flag
  | List
  deriving (Data, Eq, Show, Typeable)

makePrisms ''ArgType

data QueryArg f = QueryArg
  { _queryArgName :: Arg f
  , _queryArgType :: ArgType
  }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''QueryArg

data HeaderArg f = HeaderArg
  { _headerArg :: Arg f }
  | ReplaceHeaderArg
  { _headerArg     :: Arg f
  , _headerPattern :: Text
  }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''HeaderArg

makePrisms ''HeaderArg

data Url f = Url
  { _path     :: Path f
  , _queryStr :: [QueryArg f]
  }
  deriving (Data, Eq, Show, Typeable)

defUrl :: Url f
defUrl = Url [] []

makeLenses ''Url

data Req f = Req
  { _reqUrl        :: Url f
  , _reqMethod     :: HTTP.Method
  , _reqHeaders    :: [HeaderArg f]
  , _reqBody       :: Maybe f
  , _reqReturnType :: Maybe f
  , _reqFuncName   :: FunctionName
  }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''Req

defReq :: Req ftype
defReq = Req defUrl "GET" [] Nothing Nothing (FunctionName [])

-- | 'HasForeignType' maps Haskell types with types in the target
-- language of your backend. For example, let's say you're
-- implementing a backend to some language __X__, and you want
-- a Text representation of each input/output type mentioned in the API:
--
-- > -- First you need to create a dummy type to parametrize your
-- > -- instances.
-- > data LangX
-- >
-- > -- Otherwise you define instances for the types you need
-- > instance HasForeignType LangX Text Int where
-- >    typeFor _ _ _ = "intX"
-- >
-- > -- Or for example in case of lists
-- > instance HasForeignType LangX Text a => HasForeignType LangX Text [a] where
-- >    typeFor lang type _ = "listX of " <> typeFor lang ftype (Proxy :: Proxy a)
--
-- Finally to generate list of information about all the endpoints for
-- an API you create a function of a form:
--
-- > getEndpoints :: (HasForeign LangX Text api, GenerateList Text (Foreign Text api))
-- >              => Proxy api -> [Req Text]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy Text) api
--
-- > -- If language __X__ is dynamically typed then you can use
-- > -- a predefined NoTypes parameter with the NoContent output type:
--
-- > getEndpoints :: (HasForeign NoTypes NoContent api, GenerateList Text (Foreign NoContent api))
-- >              => Proxy api -> [Req NoContent]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) api
-- >
--
class HasForeignType lang ftype a where
  typeFor :: Proxy lang -> Proxy ftype -> Proxy a -> ftype

data NoTypes

instance HasForeignType NoTypes NoContent ftype where
  typeFor _ _ _ = NoContent

class HasForeign lang ftype (api :: *) where
  type Foreign ftype api :: *
  foreignFor :: Proxy lang -> Proxy ftype -> Proxy api -> Req ftype -> Foreign ftype api

instance (HasForeign lang ftype a, HasForeign lang ftype b)
  => HasForeign lang ftype (a :<|> b) where
  type Foreign ftype (a :<|> b) = Foreign ftype a :<|> Foreign ftype b

  foreignFor lang ftype Proxy req =
         foreignFor lang ftype (Proxy :: Proxy a) req
    :<|> foreignFor lang ftype (Proxy :: Proxy b) req

data EmptyForeignAPI = EmptyForeignAPI

instance HasForeign lang ftype EmptyAPI where
  type Foreign ftype EmptyAPI = EmptyForeignAPI

  foreignFor Proxy Proxy Proxy _ = EmptyForeignAPI

instance (KnownSymbol sym, HasForeignType lang ftype t, HasForeign lang ftype api)
  => HasForeign lang ftype (Capture' mods sym t :> api) where
  type Foreign ftype (Capture' mods sym t :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy (Proxy :: Proxy api) $
      req & reqUrl . path <>~ [Segment (Cap arg)]
          & reqFuncName . _FunctionName %~ (++ ["by", str])
    where
      str   = pack . symbolVal $ (Proxy :: Proxy sym)
      ftype = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy t)
      arg   = Arg
        { _argName = PathSegment str
        , _argType = ftype }

instance (KnownSymbol sym, HasForeignType lang ftype [t], HasForeign lang ftype sublayout)
  => HasForeign lang ftype (CaptureAll sym t :> sublayout) where
  type Foreign ftype (CaptureAll sym t :> sublayout) = Foreign ftype sublayout

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy (Proxy :: Proxy sublayout) $
      req & reqUrl . path <>~ [Segment (Cap arg)]
          & reqFuncName . _FunctionName %~ (++ ["by", str])
    where
      str   = pack . symbolVal $ (Proxy :: Proxy sym)
      ftype = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy [t])
      arg   = Arg
        { _argName = PathSegment str
        , _argType = ftype }

instance (Elem JSON list, HasForeignType lang ftype a, ReflectMethod method)
  => HasForeign lang ftype (Verb method status list a) where
  type Foreign ftype (Verb method status list a) = Req ftype

  foreignFor lang Proxy Proxy req =
    req & reqFuncName . _FunctionName %~ (methodLC :)
        & reqMethod .~ method
        & reqReturnType .~ Just retType
    where
      retType  = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a)
      method   = reflectMethod (Proxy :: Proxy method)
      methodLC = toLower $ decodeUtf8 method

instance (KnownSymbol sym, HasForeignType lang ftype (RequiredArgument mods a), HasForeign lang ftype api)
  => HasForeign lang ftype (Header' mods sym a :> api) where
  type Foreign ftype (Header' mods sym a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
    where
      hname = pack . symbolVal $ (Proxy :: Proxy sym)
      arg   = Arg
        { _argName = PathSegment hname
        , _argType  = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy (RequiredArgument mods a)) }
      subP  = Proxy :: Proxy api

instance (KnownSymbol sym, HasForeignType lang ftype (RequiredArgument mods a), HasForeign lang ftype api)
  => HasForeign lang ftype (QueryParam' mods sym a :> api) where
  type Foreign ftype (QueryParam' mods sym a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy req =
    foreignFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy api) $
      req & reqUrl.queryStr <>~ [QueryArg arg Normal]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      arg = Arg
        { _argName = PathSegment str
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy (RequiredArgument mods a)) }

instance
  (KnownSymbol sym, HasForeignType lang ftype [a], HasForeign lang ftype api)
  => HasForeign lang ftype (QueryParams sym a :> api) where
  type Foreign ftype (QueryParams sym a :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy req =
    foreignFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy api) $
      req & reqUrl.queryStr <>~ [QueryArg arg List]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      arg = Arg
        { _argName = PathSegment str
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy [a]) }

instance
  (KnownSymbol sym, HasForeignType lang ftype Bool, HasForeign lang ftype api)
  => HasForeign lang ftype (QueryFlag sym :> api) where
  type Foreign ftype (QueryFlag sym :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) $
      req & reqUrl.queryStr <>~ [QueryArg arg Flag]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      arg = Arg
        { _argName = PathSegment str
        , _argType = typeFor lang ftype (Proxy :: Proxy Bool) }

instance HasForeign lang ftype Raw where
  type Foreign ftype Raw = HTTP.Method -> Req ftype

  foreignFor _ Proxy Proxy req method =
    req & reqFuncName . _FunctionName %~ ((toLower $ decodeUtf8 method) :)
        & reqMethod .~ method

instance (Elem JSON list, HasForeignType lang ftype a, HasForeign lang ftype api)
      => HasForeign lang ftype (ReqBody' mods list a :> api) where
  type Foreign ftype (ReqBody' mods list a :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) $
      req & reqBody .~ (Just $ typeFor lang ftype (Proxy :: Proxy a))

instance (KnownSymbol path, HasForeign lang ftype api)
      => HasForeign lang ftype (path :> api) where
  type Foreign ftype (path :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) $
      req & reqUrl . path <>~ [Segment (Static (PathSegment str))]
          & reqFuncName . _FunctionName %~ (++ [str])
    where
      str = pack . symbolVal $ (Proxy :: Proxy path)

instance HasForeign lang ftype api
  => HasForeign lang ftype (RemoteHost :> api) where
  type Foreign ftype (RemoteHost :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

instance HasForeign lang ftype api
  => HasForeign lang ftype (IsSecure :> api) where
  type Foreign ftype (IsSecure :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

instance HasForeign lang ftype api => HasForeign lang ftype (Vault :> api) where
  type Foreign ftype (Vault :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

instance HasForeign lang ftype api =>
  HasForeign lang ftype (WithNamedContext name context api) where

  type Foreign ftype (WithNamedContext name context api) = Foreign ftype api

  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy api)

instance HasForeign lang ftype api
  => HasForeign lang ftype (HttpVersion :> api) where
  type Foreign ftype (HttpVersion :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

instance HasForeign lang ftype api
  => HasForeign lang ftype (Summary desc :> api) where
  type Foreign ftype (Summary desc :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

instance HasForeign lang ftype api
  => HasForeign lang ftype (Description desc :> api) where
  type Foreign ftype (Description desc :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

-- | Utility class used by 'listFromAPI' which computes
--   the data needed to generate a function for each endpoint
--   and hands it all back in a list.
class GenerateList ftype reqs where
  generateList :: reqs -> [Req ftype]

instance GenerateList ftype EmptyForeignAPI where
  generateList _ = []

instance GenerateList ftype (Req ftype) where
  generateList r = [r]

instance (GenerateList ftype start, GenerateList ftype rest)
  => GenerateList ftype (start :<|> rest) where
  generateList (start :<|> rest) = (generateList start) ++ (generateList rest)

-- | Generate the necessary data for codegen as a list, each 'Req'
--   describing one endpoint from your API type.
listFromAPI
  :: (HasForeign lang ftype api, GenerateList ftype (Foreign ftype api))
  => Proxy lang
  -> Proxy ftype
  -> Proxy api
  -> [Req ftype]
listFromAPI lang ftype p = generateList (foreignFor lang ftype p defReq)
