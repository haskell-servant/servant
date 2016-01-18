{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE NullaryTypeClasses    #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}

-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign.Internal where

import           Control.Lens          (makeLenses, (%~), (&), (.~), (<>~))
import qualified Data.Char             as C
import           Data.Proxy
import           Data.Text
import           Data.Text.Encoding    (decodeUtf8)
import           GHC.Exts              (Constraint)
import           GHC.TypeLits
import qualified Network.HTTP.Types    as HTTP
import           Prelude               hiding (concat)
import           Servant.API

-- | Function name builder that simply concat each part together
concatCase :: FunctionName -> Text
concatCase = concat

-- | Function name builder using the snake_case convention.
-- each part is separated by a single underscore character.
snakeCase :: FunctionName -> Text
snakeCase = intercalate "_"

-- | Function name builder using the CamelCase convention.
-- each part begins with an upper case character.
camelCase :: FunctionName -> Text
camelCase = camelCase' . Prelude.map (replace "-" "")
  where camelCase' []     = ""
        camelCase' (p:ps) = concat $ p : Prelude.map capitalize ps
        capitalize ""   = ""
        capitalize name = C.toUpper (Data.Text.head name) `cons` Data.Text.tail name

type ForeignType = Text
type Arg = (Text, ForeignType)

newtype Segment = Segment { _segment :: SegmentType  }
  deriving (Eq, Show)

data SegmentType = Static Text  -- ^ a static path segment. like "/foo"
                 | Cap Arg        -- ^ a capture. like "/:userid"
  deriving (Eq, Show)

type Path = [Segment]

data ArgType =
    Normal
  | Flag
  | List
  deriving (Eq, Show)

data QueryArg = QueryArg
  { _argName :: Arg
  , _argType :: ArgType
  } deriving (Eq, Show)

data HeaderArg = HeaderArg
    { headerArg :: Arg
    }
  | ReplaceHeaderArg
    { headerArg :: Arg
    , headerPattern :: Text
    } deriving (Eq, Show)


data Url = Url
  { _path     :: Path
  , _queryStr :: [QueryArg]
  } deriving (Eq, Show)

defUrl :: Url
defUrl = Url [] []

type FunctionName = [Text]

data Req = Req
  { _reqUrl        :: Url
  , _reqMethod     :: HTTP.Method
  , _reqHeaders    :: [HeaderArg]
  , _reqBody       :: Maybe ForeignType
  , _reqReturnType :: ForeignType
  , _funcName      :: FunctionName
  } deriving (Eq, Show)

makeLenses ''QueryArg
makeLenses ''Segment
makeLenses ''Url
makeLenses ''Req

isCapture :: Segment -> Bool
isCapture (Segment (Cap _)) = True
isCapture                _  = False

captureArg :: Segment -> Arg
captureArg (Segment (Cap s)) = s
captureArg                 _ = error "captureArg called on non capture"

defReq :: Req
defReq = Req defUrl "GET" [] Nothing "" []

-- | To be used exclusively as a "negative" return type/constraint
-- by @'Elem`@ type family.
class NotFound

type family Elem (a :: *) (ls::[*]) :: Constraint where
  Elem a '[]         = NotFound
  Elem a (a ': list) = ()
  Elem a (b ': list) = Elem a list

-- | 'HasForeignType' maps Haskell types with types in the target
-- language of your backend. For example, let's say you're
-- implementing a backend to some language __X__:
--
-- > -- First you need to create a dummy type to parametrize your
-- > -- instances.
-- > data LangX
-- >
-- > -- Otherwise you define instances for the types you need
-- > instance HasForeignType LangX Int where
-- >    typeFor _ _ = "intX"
-- >
-- > -- Or for example in case of lists
-- > instance HasForeignType LangX a => HasForeignType LangX [a] where
-- >    typeFor lang _ = "listX of " <> typeFor lang (Proxy :: Proxy a)
--
-- Finally to generate list of information about all the endpoints for
-- an API you create a function of a form:
--
-- > getEndpoints :: (HasForeign LangX api, GenerateList (Foreign api))
-- >              => Proxy api -> [Req]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy LangX) api
--
-- > -- If language __X__ is dynamically typed then you can use
-- > -- a predefined NoTypes parameter
-- > getEndpoints :: (HasForeign NoTypes api, GenerateList (Foreign api))
-- >              => Proxy api -> [Req]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy NoTypes) api
-- >
--
class HasForeignType lang a where
    typeFor :: Proxy lang -> Proxy a -> ForeignType

data NoTypes

instance HasForeignType NoTypes a where
    typeFor _ _ = empty

class HasForeign lang (layout :: *) where
  type Foreign layout :: *
  foreignFor :: Proxy lang -> Proxy layout -> Req -> Foreign layout

instance (HasForeign lang a, HasForeign lang b)
      => HasForeign lang (a :<|> b) where
  type Foreign (a :<|> b) = Foreign a :<|> Foreign b

  foreignFor lang Proxy req =
         foreignFor lang (Proxy :: Proxy a) req
    :<|> foreignFor lang (Proxy :: Proxy b) req

instance (KnownSymbol sym, HasForeignType lang a, HasForeign lang sublayout)
      => HasForeign lang (Capture sym a :> sublayout) where
  type Foreign (Capture sym a :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Segment (Cap arg)]
          & funcName %~ (++ ["by", str])

    where
    str = pack . symbolVal $ (Proxy :: Proxy sym)
    arg = (str, typeFor lang (Proxy :: Proxy a))

instance (Elem JSON list, HasForeignType lang a, ReflectMethod method)
      => HasForeign lang (Verb method status list a) where
  type Foreign (Verb method status list a) = Req

  foreignFor lang Proxy req =
    req & funcName  %~ (methodLC :)
        & reqMethod .~ method
        & reqReturnType .~ retType
    where
    retType  = typeFor lang (Proxy :: Proxy a)
    method   = reflectMethod (Proxy :: Proxy method)
    methodLC = toLower $ decodeUtf8 method

instance (KnownSymbol sym, HasForeignType lang a, HasForeign lang sublayout)
      => HasForeign lang (Header sym a :> sublayout) where
  type Foreign (Header sym a :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang subP $ req
        & reqHeaders <>~ [HeaderArg arg]

    where
    hname = pack . symbolVal $ (Proxy :: Proxy sym)
    arg = (hname, typeFor lang (Proxy :: Proxy a))
    subP = Proxy :: Proxy sublayout

instance (KnownSymbol sym, HasForeignType lang a, HasForeign lang sublayout)
      => HasForeign lang (QueryParam sym a :> sublayout) where
  type Foreign (QueryParam sym a :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg arg Normal]

    where
    str = pack . symbolVal $ (Proxy :: Proxy sym)
    arg = (str, typeFor lang (Proxy :: Proxy a))

instance (KnownSymbol sym, HasForeignType lang [a], HasForeign lang sublayout)
      => HasForeign lang (QueryParams sym a :> sublayout) where
  type Foreign (QueryParams sym a :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg arg List]

    where
    str = pack . symbolVal $ (Proxy :: Proxy sym)
    arg = (str, typeFor lang (Proxy :: Proxy [a]))

instance (KnownSymbol sym, HasForeignType lang a, a ~ Bool, HasForeign lang sublayout)
      => HasForeign lang (QueryFlag sym :> sublayout) where
  type Foreign (QueryFlag sym :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg arg Flag]

    where
    str = pack . symbolVal $ (Proxy :: Proxy sym)
    arg = (str, typeFor lang (Proxy :: Proxy a))

instance HasForeign lang Raw where
  type Foreign Raw = HTTP.Method -> Req

  foreignFor _ Proxy req method =
    req & funcName %~ ((toLower $ decodeUtf8 method) :)
        & reqMethod .~ method

instance (Elem JSON list, HasForeignType lang a, HasForeign lang sublayout)
      => HasForeign lang (ReqBody list a :> sublayout) where
  type Foreign (ReqBody list a :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqBody .~ (Just $ typeFor lang (Proxy :: Proxy a))

instance (KnownSymbol path, HasForeign lang sublayout)
      => HasForeign lang (path :> sublayout) where
  type Foreign (path :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Segment (Static str)]
          & funcName %~ (++ [str])

    where
    str = Data.Text.map (\c -> if c == '.' then '_' else c)
        . pack . symbolVal $ (Proxy :: Proxy path)

instance HasForeign lang sublayout => HasForeign lang (RemoteHost :> sublayout) where
  type Foreign (RemoteHost :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) req

instance HasForeign lang sublayout => HasForeign lang (IsSecure :> sublayout) where
  type Foreign (IsSecure :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) req

instance HasForeign lang sublayout => HasForeign lang (Vault :> sublayout) where
  type Foreign (Vault :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) req

instance HasForeign lang sublayout =>
  HasForeign lang (WithNamedConfig name config sublayout) where

  type Foreign (WithNamedConfig name config sublayout) = Foreign sublayout

  foreignFor lang Proxy = foreignFor lang (Proxy :: Proxy sublayout)

instance HasForeign lang sublayout => HasForeign lang (HttpVersion :> sublayout) where
  type Foreign (HttpVersion :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) req

-- | Utility class used by 'listFromAPI' which computes
--   the data needed to generate a function for each endpoint
--   and hands it all back in a list.
class GenerateList reqs where
  generateList :: reqs -> [Req]

instance GenerateList Req where
  generateList r = [r]

instance (GenerateList start, GenerateList rest) => GenerateList (start :<|> rest) where
  generateList (start :<|> rest) = (generateList start) ++ (generateList rest)

-- | Generate the necessary data for codegen as a list, each 'Req'
--   describing one endpoint from your API type.
listFromAPI :: (HasForeign lang api, GenerateList (Foreign api)) => Proxy lang -> Proxy api -> [Req]
listFromAPI lang p = generateList (foreignFor lang p defReq)
