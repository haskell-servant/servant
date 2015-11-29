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

-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign.Internal where

import           Control.Lens (makeLenses, (%~), (&), (.~), (<>~))
import qualified Data.Char    as C
import           Data.Proxy
import           Data.Text
import           GHC.Exts     (Constraint)
import           GHC.TypeLits
import           Prelude      hiding (concat)
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
type Method = Text

data Req = Req
  { _reqUrl        :: Url
  , _reqMethod     :: Method
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

class HasForeignType lang a where
    typeFor :: Proxy lang -> Proxy a -> ForeignType

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

instance (Elem JSON list, HasForeignType lang a)
      => HasForeign lang (Delete list a) where
  type Foreign (Delete list a) = Req

  foreignFor lang Proxy req =
    req & funcName      %~ ("delete" :)
        & reqMethod     .~ "DELETE"
        & reqReturnType .~ retType
    where
    retType = typeFor lang (Proxy :: Proxy a)

instance (Elem JSON list, HasForeignType lang a)
      => HasForeign lang (Get list a) where
  type Foreign (Get list a) = Req

  foreignFor lang Proxy req =
    req & funcName  %~ ("get" :)
        & reqMethod .~ "GET"
        & reqReturnType .~ retType
    where
    retType = typeFor lang (Proxy :: Proxy a)

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

instance (Elem JSON list, HasForeignType lang a)
      => HasForeign lang (Post list a) where
  type Foreign (Post list a) = Req

  foreignFor lang Proxy req =
    req & funcName  %~ ("post" :)
        & reqMethod .~ "POST"
        & reqReturnType .~ retType
    where
    retType = typeFor lang (Proxy :: Proxy a)

instance (Elem JSON list, HasForeignType lang a)
      => HasForeign lang (Put list a) where
  type Foreign (Put list a) = Req

  foreignFor lang Proxy req =
    req & funcName  %~ ("put" :)
        & reqMethod .~ "PUT"
        & reqReturnType .~ retType
    where
    retType = typeFor lang (Proxy :: Proxy a)

instance (KnownSymbol sym, HasForeignType lang a, HasForeign lang sublayout)
      => HasForeign lang (QueryParam sym a :> sublayout) where
  type Foreign (QueryParam sym a :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg arg Normal]

    where
    str = pack . symbolVal $ (Proxy :: Proxy sym)
    arg = (str, typeFor lang (Proxy :: Proxy a))

instance (KnownSymbol sym, HasForeignType lang a, HasForeign lang sublayout)
      => HasForeign lang (QueryParams sym a :> sublayout) where
  type Foreign (QueryParams sym a :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg arg List]

    where
    str = pack . symbolVal $ (Proxy :: Proxy sym)
    arg = (str, typeFor lang (Proxy :: Proxy a))

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
  type Foreign Raw = Method -> Req

  foreignFor _ Proxy req method =
    req & funcName %~ ((toLower method) :)
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

instance HasForeign lang sublayout => HasForeign lang (HttpVersion :> sublayout) where
  type Foreign (HttpVersion :> sublayout) = Foreign sublayout

  foreignFor lang Proxy req =
    foreignFor lang (Proxy :: Proxy sublayout) req
