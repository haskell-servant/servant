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

-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign
  ( HasForeign(..)
  , Segment(..)
  , SegmentType(..)
  , MatrixArg
  , FunctionName
  , QueryArg(..)
  , HeaderArg(..)
  , ArgType(..)
  , Req
  , captureArg
  , defReq
  , concatCase
  , snakeCase
  , camelCase
  -- lenses
  , argType
  , argName
  , isCapture
  , funcName
  , path
  , reqUrl
  , reqBody
  , reqHeaders
  , reqMethod
  , segment
  , queryStr
  -- re-exports
  , module Servant.API
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import           Control.Lens                  (makeLenses, (%~), (&), (.~),
                                                (<>~), _last)
import Data.Char (toLower, toUpper)
import Data.List
import Data.Proxy
import GHC.Exts (Constraint)
import GHC.TypeLits
import Servant.API

-- | Function name builder that simply concat each part together
concatCase :: FunctionName -> String
concatCase = concat

-- | Function name builder using the snake_case convention.
-- each part is separated by a single underscore character.
snakeCase :: FunctionName -> String
snakeCase = intercalate "_"

-- | Function name builder using the CamelCase convention.
-- each part begins with an upper case character.
camelCase :: FunctionName -> String
camelCase [] = ""
camelCase (p:ps) = concat $ p : camelCase' ps
   where camelCase' [] = []
         camelCase' (r:rs) = capitalize r : camelCase' rs
         capitalize [] = []
         capitalize (x:xs) = toUpper x : xs

type Arg = String

data Segment = Segment { _segment :: SegmentType, _matrix :: [MatrixArg] }
  deriving (Eq, Show)

data SegmentType = Static String  -- ^ a static path segment. like "/foo"
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
    { headerArgName :: String
    }
  | ReplaceHeaderArg
    { headerArgName :: String
    , headerPattern :: String
    } deriving (Eq, Show)


type MatrixArg = QueryArg

data Url = Url
  { _path     :: Path
  , _queryStr :: [QueryArg]
  } deriving (Eq, Show)

defUrl :: Url
defUrl = Url [] []

type FunctionName = [String]
type Method = String

data Req = Req
  { _reqUrl     :: Url
  , _reqMethod  :: Method
  , _reqHeaders :: [HeaderArg]
  , _reqBody    :: Bool
  , _funcName   :: FunctionName
  } deriving (Eq, Show)

makeLenses ''QueryArg
makeLenses ''Segment
makeLenses ''Url
makeLenses ''Req

isCapture :: Segment -> Bool
isCapture (Segment (Cap _) _) = True
isCapture                  _  = False

captureArg :: Segment -> Arg
captureArg (Segment (Cap s) _) = s
captureArg                  _  = error "captureArg called on non capture"

defReq :: Req
defReq = Req defUrl "GET" [] False []

-- | To be used exclusively as a "negative" return type/constraint
-- by @'Elem`@ type family.
class NotFound

type family Elem (a :: *) (ls::[*]) :: Constraint where
  Elem a '[]         = NotFound
  Elem a (a ': list) = ()
  Elem a (b ': list) = Elem a list

class HasForeign (layout :: *) where
  type Foreign layout :: *
  foreignFor :: Proxy layout -> Req -> Foreign layout

instance (HasForeign a, HasForeign b)
      => HasForeign (a :<|> b) where
  type Foreign (a :<|> b) = Foreign a :<|> Foreign b

  foreignFor Proxy req =
         foreignFor (Proxy :: Proxy a) req
    :<|> foreignFor (Proxy :: Proxy b) req

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (Capture sym a :> sublayout) where
  type Foreign (Capture sym a :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Segment (Cap str) []]
          & funcName %~ (++ ["by", str])

    where str = symbolVal (Proxy :: Proxy sym)

instance Elem JSON list => HasForeign (Delete list a) where
  type Foreign (Delete list a) = Req

  foreignFor Proxy req =
    req & funcName  %~ ("delete" :)
        & reqMethod .~ "DELETE"

instance Elem JSON list => HasForeign (Get list a) where
  type Foreign (Get list a) = Req

  foreignFor Proxy req =
    req & funcName  %~ ("get" :)
        & reqMethod .~ "GET"

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (Header sym a :> sublayout) where
  type Foreign (Header sym a :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor subP (req & reqHeaders <>~ [HeaderArg hname])

    where hname = symbolVal (Proxy :: Proxy sym)
          subP = Proxy :: Proxy sublayout

instance Elem JSON list => HasForeign (Post list a) where
  type Foreign (Post list a) = Req

  foreignFor Proxy req =
    req & funcName  %~ ("post" :)
        & reqMethod .~ "POST"

instance Elem JSON list => HasForeign (Put list a) where
  type Foreign (Put list a) = Req

  foreignFor Proxy req =
    req & funcName  %~ ("put" :)
        & reqMethod .~ "PUT"

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (QueryParam sym a :> sublayout) where
  type Foreign (QueryParam sym a :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Normal]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (QueryParams sym a :> sublayout) where
  type Foreign (QueryParams sym a :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str List]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (QueryFlag sym :> sublayout) where
  type Foreign (QueryFlag sym :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Flag]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (MatrixParam sym a :> sublayout) where
  type Foreign (MatrixParam sym a :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path._last.matrix <>~ [QueryArg strArg Normal]

    where str = symbolVal (Proxy :: Proxy sym)
          strArg = str ++ "Value"

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (MatrixParams sym a :> sublayout) where
  type Foreign (MatrixParams sym a :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path._last.matrix  <>~ [QueryArg str List]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasForeign sublayout)
      => HasForeign (MatrixFlag sym :> sublayout) where
  type Foreign (MatrixFlag sym :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path._last.matrix  <>~ [QueryArg str Flag]

    where str = symbolVal (Proxy :: Proxy sym)

instance HasForeign (Raw m a) where
  type Foreign (Raw m a) = Method -> Req

  foreignFor Proxy req method =
    req & funcName %~ ((toLower <$> method) :)
        & reqMethod .~ method

instance (Elem JSON list, HasForeign sublayout) => HasForeign (ReqBody list a :> sublayout) where
  type Foreign (ReqBody list a :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqBody .~ True

instance (KnownSymbol path, HasForeign sublayout)
      => HasForeign (path :> sublayout) where
  type Foreign (path :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Segment (Static str) []]
          & funcName %~ (++ [str])

    where str = map (\c -> if c == '.' then '_' else c) $ symbolVal (Proxy :: Proxy path)

instance HasForeign sublayout => HasForeign (RemoteHost :> sublayout) where
  type Foreign (RemoteHost :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) req

instance HasForeign sublayout => HasForeign (IsSecure :> sublayout) where
  type Foreign (IsSecure :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) req

instance HasForeign sublayout => HasForeign (Vault :> sublayout) where
  type Foreign (Vault :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) req

instance HasForeign sublayout => HasForeign (HttpVersion :> sublayout) where
  type Foreign (HttpVersion :> sublayout) = Foreign sublayout

  foreignFor Proxy req =
    foreignFor (Proxy :: Proxy sublayout) req
