{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE NullaryTypeClasses #-}
#endif

-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign.Internal where

import           Control.Lens (makePrisms, makeLenses, Getter, (&), (<>~), (%~),
                               (.~))
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid
#endif
import           Data.Proxy
import           Data.String
import           Data.Text
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Exts (Constraint)
import           GHC.TypeLits
import qualified Network.HTTP.Types as HTTP
import           Prelude hiding (concat)
import           Servant.API


newtype FunctionName = FunctionName { unFunctionName :: [Text] }
  deriving (Show, Eq, Monoid)

makePrisms ''FunctionName

newtype PathSegment = PathSegment { unPathSegment :: Text }
  deriving (Show, Eq, IsString, Monoid)

makePrisms ''PathSegment

data Arg f = Arg
  { _argName :: PathSegment
  , _argType :: f }

deriving instance Eq f => Eq (Arg f)
deriving instance Show f => Show (Arg f)

makeLenses ''Arg

argPath :: Getter (Arg f) Text
argPath = argName . _PathSegment

data SegmentType f
  = Static PathSegment
    -- ^ a static path segment. like "/foo"
  | Cap (Arg f)
    -- ^ a capture. like "/:userid"

deriving instance Eq f => Eq (SegmentType f)
deriving instance Show f => Show (SegmentType f)

makePrisms ''SegmentType

newtype Segment f = Segment { unSegment :: SegmentType f }

deriving instance Eq f => Eq (Segment f)
deriving instance Show f => Show (Segment f)

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
  deriving (Eq, Show)

makePrisms ''ArgType

data QueryArg f = QueryArg
  { _queryArgName :: Arg f
  , _queryArgType :: ArgType
  }

deriving instance Eq f => Eq (QueryArg f)
deriving instance Show f => Show (QueryArg f)

makeLenses ''QueryArg

data HeaderArg f = HeaderArg
  { _headerArg :: Arg f }
  | ReplaceHeaderArg
  { _headerArg     :: Arg f
  , _headerPattern :: Text
  }

deriving instance Eq f => Eq (HeaderArg f)
deriving instance Show f => Show (HeaderArg f)

makeLenses ''HeaderArg

makePrisms ''HeaderArg

data Url f = Url
  { _path     :: Path f
  , _queryStr :: [QueryArg f]
  }

deriving instance Eq f => Eq (Url f)
deriving instance Show f => Show (Url f)

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

deriving instance Eq f => Eq (Req f)
deriving instance Show f => Show (Req f)

makeLenses ''Req

defReq :: Req ftype
defReq = Req defUrl "GET" [] Nothing Nothing (FunctionName [])

-- | To be used exclusively as a "negative" return type/constraint
-- by @'Elem`@ type family.
class NotFound

type family Elem (a :: *) (ls::[*]) :: Constraint where
  Elem a '[]         = NotFound
  Elem a (a ': list) = ()
  Elem a (b ': list) = Elem a list

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
-- > -- a predefined NoTypes parameter with the () output type:
--
-- > getEndpoints :: (HasForeign NoTypes () api, GenerateList Text (Foreign () api))
-- >              => Proxy api -> [Req ()]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy ()) api
-- >
--
class HasForeignType lang ftype a where
  typeFor :: Proxy lang -> Proxy ftype -> Proxy a -> ftype

data NoTypes

instance HasForeignType NoTypes () ftype where
  typeFor _ _ _ = ()

class HasForeign lang ftype (layout :: *) where
  type Foreign ftype layout :: *
  foreignFor :: Proxy lang -> Proxy ftype -> Proxy layout -> Req ftype -> Foreign ftype layout

instance (HasForeign lang ftype a, HasForeign lang ftype b)
  => HasForeign lang ftype (a :<|> b) where
  type Foreign ftype (a :<|> b) = Foreign ftype a :<|> Foreign ftype b

  foreignFor lang ftype Proxy req =
         foreignFor lang ftype (Proxy :: Proxy a) req
    :<|> foreignFor lang ftype (Proxy :: Proxy b) req

instance (KnownSymbol sym, HasForeignType lang ftype t, HasForeign lang ftype sublayout)
  => HasForeign lang ftype (Capture sym t :> sublayout) where
  type Foreign ftype (Capture sym a :> sublayout) = Foreign ftype sublayout

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy (Proxy :: Proxy sublayout) $
      req & reqUrl . path <>~ [Segment (Cap arg)]
          & reqFuncName . _FunctionName %~ (++ ["by", str])
    where
      str   = pack . symbolVal $ (Proxy :: Proxy sym)
      ftype = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy t)
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

instance (KnownSymbol sym, HasForeignType lang ftype a, HasForeign lang ftype sublayout)
  => HasForeign lang ftype (Header sym a :> sublayout) where
  type Foreign ftype (Header sym a :> sublayout) = Foreign ftype sublayout

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
    where
      hname = pack . symbolVal $ (Proxy :: Proxy sym)
      arg   = Arg
        { _argName = PathSegment hname
        , _argType  = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a) }
      subP  = Proxy :: Proxy sublayout

instance (KnownSymbol sym, HasForeignType lang ftype a, HasForeign lang ftype sublayout)
  => HasForeign lang ftype (QueryParam sym a :> sublayout) where
  type Foreign ftype (QueryParam sym a :> sublayout) = Foreign ftype sublayout

  foreignFor lang Proxy Proxy req =
    foreignFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg arg Normal]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      arg = Arg
        { _argName = PathSegment str
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a) }

instance
  (KnownSymbol sym, HasForeignType lang ftype [a], HasForeign lang ftype sublayout)
  => HasForeign lang ftype (QueryParams sym a :> sublayout) where
  type Foreign ftype (QueryParams sym a :> sublayout) = Foreign ftype sublayout
  foreignFor lang Proxy Proxy req =
    foreignFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg arg List]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      arg = Arg
        { _argName = PathSegment str
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy [a]) }

instance
  (KnownSymbol sym, HasForeignType lang ftype Bool, HasForeign lang ftype sublayout)
  => HasForeign lang ftype (QueryFlag sym :> sublayout) where
  type Foreign ftype (QueryFlag sym :> sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy sublayout) $
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

instance (Elem JSON list, HasForeignType lang ftype a, HasForeign lang ftype sublayout)
      => HasForeign lang ftype (ReqBody list a :> sublayout) where
  type Foreign ftype (ReqBody list a :> sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy sublayout) $
      req & reqBody .~ (Just $ typeFor lang ftype (Proxy :: Proxy a))

instance (KnownSymbol path, HasForeign lang ftype sublayout)
      => HasForeign lang ftype (path :> sublayout) where
  type Foreign ftype (path :> sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy sublayout) $
      req & reqUrl . path <>~ [Segment (Static (PathSegment str))]
          & reqFuncName . _FunctionName %~ (++ [str])
    where
      str =
        Data.Text.map (\c -> if c == '.' then '_' else c)
          . pack . symbolVal $ (Proxy :: Proxy path)

instance HasForeign lang ftype sublayout
  => HasForeign lang ftype (RemoteHost :> sublayout) where
  type Foreign ftype (RemoteHost :> sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy sublayout) req

instance HasForeign lang ftype sublayout
  => HasForeign lang ftype (IsSecure :> sublayout) where
  type Foreign ftype (IsSecure :> sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy sublayout) req

instance HasForeign lang ftype sublayout => HasForeign lang ftype (Vault :> sublayout) where
  type Foreign ftype (Vault :> sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy sublayout) req

instance HasForeign lang ftype sublayout =>
  HasForeign lang ftype (WithNamedContext name context sublayout) where

  type Foreign ftype (WithNamedContext name context sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy sublayout)

instance HasForeign lang ftype sublayout
  => HasForeign lang ftype (HttpVersion :> sublayout) where
  type Foreign ftype (HttpVersion :> sublayout) = Foreign ftype sublayout

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy sublayout) req

-- | Utility class used by 'listFromAPI' which computes
--   the data needed to generate a function for each endpoint
--   and hands it all back in a list.
class GenerateList ftype reqs where
  generateList :: reqs -> [Req ftype]

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
