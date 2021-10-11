{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Servant.Foreign.Internal where

import           Prelude ()
import           Prelude.Compat

import           Control.Lens
                 (Getter, makeLenses, makePrisms, (%~), (&), (.~), (<>~))
import           Data.Data
                 (Data)
import           Data.Proxy
import           Data.String
import           Data.Text
import           Data.Text.Encoding
                 (decodeUtf8)
import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
import qualified Network.HTTP.Types    as HTTP
import           Servant.API
import           Servant.API.Modifiers
                 (RequiredArgument)
import           Servant.API.TypeLevel

-- | Canonical name of the endpoint, can be used to generate a function name.
--
-- You can use the functions in "Servant.Foreign.Inflections", like 'Servant.Foreign.Inflections.camelCase' to transform to `Text`.
newtype FunctionName = FunctionName { unFunctionName :: [Text] }
  deriving (Data, Show, Eq, Semigroup, Monoid, Typeable)

makePrisms ''FunctionName

-- | See documentation of 'Arg'
newtype PathSegment = PathSegment { unPathSegment :: Text }
  deriving (Data, Show, Eq, IsString, Semigroup, Monoid, Typeable)

makePrisms ''PathSegment

-- | Maps a name to the foreign type that belongs to the annotated value.
--
-- Used for header args, query args, and capture args.
data Arg ftype = Arg
  { _argName :: PathSegment
  -- ^ The name to be captured.
  --
  -- Only for capture args it really denotes a path segment.
  , _argType :: ftype
  -- ^ Foreign type the associated value will have
  }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''Arg

argPath :: Getter (Arg ftype) Text
argPath = argName . _PathSegment

data SegmentType ftype
  = Static PathSegment
    -- ^ Static path segment.
    --
    -- @"foo\/bar\/baz"@
    --
    -- contains the static segments @"foo"@, @"bar"@ and @"baz"@.
  | Cap (Arg ftype)
    -- ^ A capture.
    --
    -- @"user\/{userid}\/name"@
    --
    -- would capture the arg @userid@ with type @ftype@.
  deriving (Data, Eq, Show, Typeable)

makePrisms ''SegmentType

-- | A part of the Url’s path.
newtype Segment ftype = Segment { unSegment :: SegmentType ftype }
  deriving (Data, Eq, Show, Typeable)

makePrisms ''Segment

-- | Whether a segment is a 'Cap'.
isCapture :: Segment ftype -> Bool
isCapture (Segment (Cap _)) = True
isCapture                _  = False

-- | Crashing Arg extraction from segment, TODO: remove
captureArg :: Segment ftype -> Arg ftype
captureArg (Segment (Cap s)) = s
captureArg                 _ = error "captureArg called on non capture"

-- TODO: remove, unnecessary indirection
type Path ftype = [Segment ftype]

-- | Type of a 'QueryArg'.
data ArgType
  = Normal
  | Flag
  | List
  deriving (Data, Eq, Show, Typeable)

makePrisms ''ArgType

-- | Url Query argument.
--
-- Urls can contain query arguments, which is a list of key-value pairs.
-- In a typical url, query arguments look like this:
--
-- @?foo=bar&alist[]=el1&alist[]=el2&aflag@
--
-- Each pair can be
--
-- * @?foo=bar@: a plain key-val pair, either optional or required ('QueryParam')
-- * @?aflag@: a flag (no value, implicitly Bool with default `false` if it’s missing) ('QueryFlag')
-- * @?alist[]=el1&alist[]=el2@: list of values ('QueryParams')
--
-- @_queryArgType@ will be set accordingly.
--
-- For the plain key-val pairs ('QueryParam'), @_queryArgName@’s @ftype@ will be wrapped in a @Maybe@ if the argument is optional.
data QueryArg ftype = QueryArg
  { _queryArgName :: Arg ftype
  -- ^ Name and foreign type of the argument. Will be wrapped in `Maybe` if the query is optional and in a `[]` if the query is a list
  , _queryArgType :: ArgType
  -- ^ one of normal/plain, list or flag
  }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''QueryArg

data HeaderArg ftype =
  -- | The name of the header and the foreign type of its value.
  HeaderArg
  { _headerArg :: Arg ftype }
  -- | Unused, will never be set.
  --
  -- TODO: remove
  | ReplaceHeaderArg
  { _headerArg     :: Arg ftype
  , _headerPattern :: Text
  }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''HeaderArg

makePrisms ''HeaderArg

-- | Full endpoint url, with all captures and parameters
data Url ftype = Url
  { _path     :: Path ftype
  -- ^ Url path, list of either static segments or captures
  --
  -- @"foo\/{id}\/bar"@
  , _queryStr :: [QueryArg ftype]
  -- ^ List of query args
  --
  -- @"?foo=bar&a=b"@
  , _frag     :: Maybe ftype
  -- ^ Url fragment.
  --
  -- Not sent to the HTTP server, so only useful for frontend matters (e.g. inter-page linking).
  --
  -- @#fragmentText@
  }
  deriving (Data, Eq, Show, Typeable)

defUrl :: Url ftype
defUrl = Url [] [] Nothing

makeLenses ''Url

-- | See documentation of '_reqBodyContentType'
data ReqBodyContentType = ReqBodyJSON | ReqBodyMultipart
  deriving (Data, Eq, Show, Read)

-- | Full description of an endpoint in your API, generated by 'listFromAPI'. It should give you all the information needed to generate foreign language bindings.
--
-- Every field containing @ftype@ will use the foreign type mapping specified via 'HasForeignType' (see its docstring on how to set that up).
--
-- See https://docs.servant.dev/en/stable/tutorial/ApiType.html for accessible documentation of the possible content of an endpoint.
data Req ftype = Req
  { _reqUrl             :: Url ftype
  -- ^ Full list of URL segments, including captures
  , _reqMethod          :: HTTP.Method
  -- ^ @\"GET\"@\/@\"POST\"@\/@\"PUT\"@\/…
  , _reqHeaders         :: [HeaderArg ftype]
  -- ^ Headers required by this endpoint, with their type
  , _reqBody            :: Maybe ftype
  -- ^ Foreign type of the expected request body ('ReqBody'), if any
  , _reqReturnType      :: Maybe ftype
  -- ^ The foreign type of the response, if any
  , _reqFuncName        :: FunctionName
  -- ^ The URL segments rendered in a way that they can be easily concatenated into a canonical function name
  , _reqBodyContentType :: ReqBodyContentType
  -- ^ The content type the request body is transferred as.
  --
  -- This is a severe limitation of @servant-foreign@ currently,
  -- as we only allow the content type to be `JSON`
  -- no user-defined content types. ('ReqBodyMultipart' is not
  -- actually implemented.)
  --
  -- Thus, any routes looking like this will work:
  --
  -- @"foo" :> Get '[JSON] Foo@
  --
  -- while routes like
  --
  -- @"foo" :> Get '[MyFancyContentType] Foo@
  --
  -- will fail with an error like
  --
  -- @• JSON expected in list '[MyFancyContentType]@
  }
  deriving (Data, Eq, Show, Typeable)

makeLenses ''Req

defReq :: Req ftype
defReq = Req defUrl "GET" [] Nothing Nothing (FunctionName []) ReqBodyJSON

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
-- >    typeFor lang ftype _ = "listX of " <> typeFor lang ftype (Proxy :: Proxy a)
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

-- | The language definition without any foreign types. It can be used for dynamic languages which do not /do/ type annotations.
data NoTypes

-- | Use if the foreign language does not have any types.
instance HasForeignType NoTypes NoContent a where
  typeFor _ _ _ = NoContent

-- | Implementation of the Servant framework types.
--
-- Relevant instances: Everything containing 'HasForeignType'.
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

instance (HasForeignType lang ftype NoContent, ReflectMethod method)
  => HasForeign lang ftype (NoContentVerb method) where
  type Foreign ftype (NoContentVerb method) = Req ftype

  foreignFor lang Proxy Proxy req =
    req & reqFuncName . _FunctionName %~ (methodLC :)
        & reqMethod .~ method
        & reqReturnType .~ Just retType
    where
      retType  = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy NoContent)
      method   = reflectMethod (Proxy :: Proxy method)
      methodLC = toLower $ decodeUtf8 method

-- | TODO: doesn't taking framing into account.
instance (ct ~ JSON, HasForeignType lang ftype a, ReflectMethod method)
  => HasForeign lang ftype (Stream method status framing ct a) where
  type Foreign ftype (Stream method status framing ct a) = Req ftype

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

instance
  (HasForeignType lang ftype (Maybe a), HasForeign lang ftype api)
  => HasForeign lang ftype (Fragment a :> api) where
  type Foreign ftype (Fragment a :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy req =
    foreignFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy api) $
      req & reqUrl . frag .~ Just argT
    where
      argT = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy (Maybe a))

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

instance
    ( HasForeign lang ftype api
    ) =>  HasForeign lang ftype (StreamBody' mods framing ctype a :> api)
  where
    type Foreign ftype (StreamBody' mods framing ctype a :> api) = Foreign ftype api

    foreignFor _lang Proxy Proxy _req = error "HasForeign @StreamBody"

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
