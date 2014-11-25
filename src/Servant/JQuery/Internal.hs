{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.JQuery.Internal where

import Control.Lens
import Data.Proxy
import GHC.TypeLits
import Servant.API

type Arg = String

data Segment = Static String -- ^ a static path segment. like "/foo"
             | Cap Arg       -- ^ a capture. like "/:userid"
  deriving (Eq, Show)


isCapture :: Segment -> Bool
isCapture (Cap _) = True
isCapture      _  = False

captureArg :: Segment -> Arg
captureArg (Cap s) = s
captureArg      _  = error "captureArg called on non capture"

jsSegments :: [Segment] -> String
jsSegments []  = ""
jsSegments [x] = "/" ++ segmentToStr x False
jsSegments (x:xs) = "/" ++ segmentToStr x True ++ jsSegments xs

segmentToStr :: Segment -> Bool -> String
segmentToStr (Static s) notTheEnd =
  if notTheEnd then s else s ++ "'"
segmentToStr (Cap s)    notTheEnd =
  "' + " ++ s ++ if notTheEnd then " + '" else ""

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

data Url = Url
  { _path     :: Path
  , _queryStr :: [QueryArg]
  } deriving (Eq, Show)

defUrl :: Url
defUrl = Url [] []

type FunctionName = String
type Method = String

data AjaxReq = AjaxReq
  { _reqUrl    :: Url
  , _reqMethod :: Method
  , _reqBody   :: Bool
  , _funcName  :: FunctionName
  } deriving (Eq, Show)

makeLenses ''QueryArg
makeLenses ''Url
makeLenses ''AjaxReq

jsParams :: [QueryArg] -> String
jsParams []  = ""
jsParams [x] = paramToStr x False
jsParams (x:xs) = paramToStr x True ++ "&" ++ jsParams xs

paramToStr :: QueryArg -> Bool -> String
paramToStr qarg notTheEnd =
  case qarg ^. argType of
    Normal -> name
           ++ "=' + encodeURIComponent("
           ++ name
           ++ if notTheEnd then ") + '" else ")"

    Flag   -> name ++ "="

    List   -> name
           ++ "[]=' + encodeURIComponent("
           ++ name
           ++ if notTheEnd then ") + '" else ")"

  where name = qarg ^. argName

defReq :: AjaxReq
defReq = AjaxReq defUrl "GET" False ""

class HasJQ layout where
  type JQ layout :: *
  jqueryFor :: Proxy layout -> AjaxReq -> JQ layout

instance (HasJQ a, HasJQ b)
      => HasJQ (a :<|> b) where
  type JQ (a :<|> b) = JQ a :<|> JQ b

  jqueryFor Proxy req =
         jqueryFor (Proxy :: Proxy a) req
    :<|> jqueryFor (Proxy :: Proxy b) req

instance (KnownSymbol sym, HasJQ sublayout)
      => HasJQ (Capture sym a :> sublayout) where
  type JQ (Capture sym a :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Cap str]

    where str = symbolVal (Proxy :: Proxy sym)

instance HasJQ Delete where
  type JQ Delete = FunctionName -> AjaxReq

  jqueryFor Proxy req fName =
    req & funcName  .~ fName
        & reqMethod .~ "DELETE"

instance HasJQ (Get a) where
  type JQ (Get a) = FunctionName -> AjaxReq

  jqueryFor Proxy req fName =
    req & funcName  .~ fName
        & reqMethod .~ "GET"

instance HasJQ (Post a) where
  type JQ (Post a) = FunctionName -> AjaxReq

  jqueryFor Proxy req fName =
    req & funcName  .~ fName
        & reqMethod .~ "POST"

instance HasJQ (Put a) where
  type JQ (Put a) = FunctionName -> AjaxReq

  jqueryFor Proxy req fName =
    req & funcName  .~ fName
        & reqMethod .~ "PUT"

instance (KnownSymbol sym, HasJQ sublayout)
      => HasJQ (QueryParam sym a :> sublayout) where
  type JQ (QueryParam sym a :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Normal]

    where str = symbolVal (Proxy :: Proxy sym)
          strArg = str ++ "Value"

instance (KnownSymbol sym, HasJQ sublayout)
      => HasJQ (QueryParams sym a :> sublayout) where
  type JQ (QueryParams sym a :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str List]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasJQ sublayout)
      => HasJQ (QueryFlag sym :> sublayout) where
  type JQ (QueryFlag sym :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Flag]

    where str = symbolVal (Proxy :: Proxy sym)

instance HasJQ Raw where
  type JQ Raw = Method -> FunctionName -> AjaxReq

  jqueryFor Proxy req method fName =
    req & reqMethod .~ method
        & funcName  .~ fName

instance HasJQ sublayout => HasJQ (ReqBody a :> sublayout) where
  type JQ (ReqBody a :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqBody .~ True

instance (KnownSymbol path, HasJQ sublayout)
      => HasJQ (path :> sublayout) where
  type JQ (path :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Static str]

    where str = symbolVal (Proxy :: Proxy path)
