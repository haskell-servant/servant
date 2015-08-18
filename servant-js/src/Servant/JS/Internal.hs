{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.JS.Internal where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Lens
import Data.Char (toLower, toUpper)
import qualified Data.CharSet as Set
import qualified Data.CharSet.Unicode.Category as Set
import Data.List
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import GHC.Exts (Constraint)
import GHC.TypeLits
import Servant.API

-- | this structure is used by JavaScriptGenerator implementations to let you
-- customize the output
data CommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder :: FunctionName -> String  -- ^ function generating function names
  , requestBody :: String                -- ^ name used when a user want to send the request body (to let you redefine it)
  , successCallback :: String            -- ^ name of the callback parameter when the request was successful
  , errorCallback :: String              -- ^ name of the callback parameter when the request reported an error
  , moduleName :: String                 -- ^ namespace on which we define the js function (empty mean local var)
  , urlPrefix :: String                  -- ^ a prefix we should add to the Url in the JS codegen
  }

-- | Default options.
--
-- @
-- > defCommonGeneratorOptions = CommonGeneratorOptions
-- >   { functionNameBuilder = camelCase
-- >   , requestBody = "body"
-- >   , successCallback = "onSuccess"
-- >   , errorCallback = "onError"
-- >   , moduleName = ""
-- >   , urlPrefix = ""
-- >   }
-- @
defCommonGeneratorOptions :: CommonGeneratorOptions
defCommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder = camelCase
  , requestBody = "body"
  , successCallback = "onSuccess"
  , errorCallback = "onError"
  , moduleName = ""
  , urlPrefix = ""
  }

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

-- A 'JavascriptGenerator' just takes the data found in the API type
-- for each endpoint and generates Javascript code in a String. Several
-- generators are available in this package.
type JavaScriptGenerator = [AjaxReq] -> String

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
    } deriving (Eq)

instance Show HeaderArg where
    show (HeaderArg n)          = toValidFunctionName ("header" <> n)
    show (ReplaceHeaderArg n p)
        | pn `isPrefixOf` p = pv <> " + \"" <> rp <> "\""
        | pn `isSuffixOf` p = "\"" <> rp <> "\" + " <> pv
        | pn `isInfixOf` p  = "\"" <> (replace pn ("\" + " <> pv <> " + \"") p)
                                   <> "\""
        | otherwise         = p
      where
        pv = toValidFunctionName ("header" <> n)
        pn = "{" <> n <> "}"
        rp = replace pn "" p
        -- Use replace method from Data.Text
        replace old new = T.unpack .
            T.replace (T.pack old) (T.pack new) .
            T.pack

-- | Attempts to reduce the function name provided to that allowed by JS.
-- https://mathiasbynens.be/notes/javascript-identifiers
-- Couldn't work out how to handle zero-width characters.
-- @TODO: specify better default function name, or throw error?
toValidFunctionName :: String -> String
toValidFunctionName (x:xs) = [setFirstChar x] <> filter remainder xs
  where
    setFirstChar c = if firstChar c
        then c
        else '_'
    firstChar c = prefixOK c || any (Set.member c) firstLetterOK
    remainder c = prefixOK c || any (Set.member c) remainderOK
    -- Valid prefixes
    prefixOK c = c `elem` ['$','_']
    -- Unicode character sets
    firstLetterOK = [ Set.lowercaseLetter
                    , Set.uppercaseLetter
                    , Set.titlecaseLetter
                    , Set.modifierLetter
                    , Set.otherLetter
                    , Set.letterNumber ]
    remainderOK   = firstLetterOK <> [ Set.nonSpacingMark
                                     , Set.spacingCombiningMark
                                     , Set.decimalNumber
                                     , Set.connectorPunctuation ]
toValidFunctionName [] = "_"

type MatrixArg = QueryArg

data Url = Url
  { _path     :: Path
  , _queryStr :: [QueryArg]
  } deriving (Eq, Show)

defUrl :: Url
defUrl = Url [] []

type FunctionName = [String]
type Method = String

data AjaxReq = AjaxReq
  { _reqUrl     :: Url
  , _reqMethod  :: Method
  , _reqHeaders :: [HeaderArg]
  , _reqBody    :: Bool
  , _funcName   :: FunctionName
  } deriving (Eq, Show)

makeLenses ''QueryArg
makeLenses ''Segment
makeLenses ''Url
makeLenses ''AjaxReq

isCapture :: Segment -> Bool
isCapture (Segment (Cap _) _) = True
isCapture                  _  = False

hasMatrixArgs :: Segment -> Bool
hasMatrixArgs (Segment _ (_:_)) = True
hasMatrixArgs                _  = False

hasArgs :: Segment -> Bool
hasArgs s = isCapture s || hasMatrixArgs s

matrixArgs :: Segment -> [MatrixArg]
matrixArgs (Segment _ ms) = ms

captureArg :: Segment -> Arg
captureArg (Segment (Cap s) _) = s
captureArg                  _  = error "captureArg called on non capture"

jsSegments :: [Segment] -> String
jsSegments []  = ""
jsSegments [x] = "/" ++ segmentToStr x False
jsSegments (x:xs) = "/" ++ segmentToStr x True ++ jsSegments xs

segmentToStr :: Segment -> Bool -> String
segmentToStr (Segment st ms) notTheEnd =
  segmentTypeToStr st ++ jsMParams ms ++ if notTheEnd then "" else "'"

segmentTypeToStr :: SegmentType -> String
segmentTypeToStr (Static s) = s
segmentTypeToStr (Cap s)    = "' + encodeURIComponent(" ++ s ++ ") + '"

jsGParams :: String -> [QueryArg] -> String
jsGParams _ []  = ""
jsGParams _ [x] = paramToStr x False
jsGParams s (x:xs) = paramToStr x True ++ s ++ jsGParams s xs

jsParams :: [QueryArg] -> String
jsParams = jsGParams "&"

jsMParams :: [MatrixArg] -> String
jsMParams [] = ""
jsMParams xs = ";" ++ jsGParams ";" xs

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
defReq = AjaxReq defUrl "GET" [] False []

type family Elem (a :: *) (ls::[*]) :: Constraint where
  Elem a '[] = 'False ~ 'True
  Elem a (a ': list) = ()
  Elem a (b ': list) = Elem a list

class HasJS (layout :: *) where
  type JS layout :: *
  javascriptFor :: Proxy layout -> AjaxReq -> JS layout

instance (HasJS a, HasJS b)
      => HasJS (a :<|> b) where
  type JS (a :<|> b) = JS a :<|> JS b

  javascriptFor Proxy req =
         javascriptFor (Proxy :: Proxy a) req
    :<|> javascriptFor (Proxy :: Proxy b) req

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (Capture sym a :> sublayout) where
  type JS (Capture sym a :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Segment (Cap str) []]
          & funcName %~ (++ ["by", str])

    where str = symbolVal (Proxy :: Proxy sym)

instance Elem JSON list => HasJS (Delete list a) where
  type JS (Delete list a) = AjaxReq

  javascriptFor Proxy req =
    req & funcName  %~ ("delete" :)
        & reqMethod .~ "DELETE"

instance Elem JSON list => HasJS (Get list a) where
  type JS (Get list a) = AjaxReq

  javascriptFor Proxy req =
    req & funcName  %~ ("get" :)
        & reqMethod .~ "GET"

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (Header sym a :> sublayout) where
  type JS (Header sym a :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor subP (req & reqHeaders <>~ [HeaderArg hname])

    where hname = symbolVal (Proxy :: Proxy sym)
          subP = Proxy :: Proxy sublayout

instance Elem JSON list => HasJS (Post list a) where
  type JS (Post list a) = AjaxReq

  javascriptFor Proxy req =
    req & funcName  %~ ("post" :)
        & reqMethod .~ "POST"

instance Elem JSON list => HasJS (Put list a) where
  type JS (Put list a) = AjaxReq

  javascriptFor Proxy req =
    req & funcName  %~ ("put" :)
        & reqMethod .~ "PUT"

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (QueryParam sym a :> sublayout) where
  type JS (QueryParam sym a :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Normal]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (QueryParams sym a :> sublayout) where
  type JS (QueryParams sym a :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str List]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (QueryFlag sym :> sublayout) where
  type JS (QueryFlag sym :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Flag]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (MatrixParam sym a :> sublayout) where
  type JS (MatrixParam sym a :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path._last.matrix <>~ [QueryArg strArg Normal]

    where str = symbolVal (Proxy :: Proxy sym)
          strArg = str ++ "Value"

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (MatrixParams sym a :> sublayout) where
  type JS (MatrixParams sym a :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path._last.matrix  <>~ [QueryArg str List]

    where str = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasJS sublayout)
      => HasJS (MatrixFlag sym :> sublayout) where
  type JS (MatrixFlag sym :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path._last.matrix  <>~ [QueryArg str Flag]

    where str = symbolVal (Proxy :: Proxy sym)

instance HasJS Raw where
  type JS Raw = Method -> AjaxReq

  javascriptFor Proxy req method =
    req & funcName %~ ((toLower <$> method) :)
        & reqMethod .~ method

instance (Elem JSON list, HasJS sublayout) => HasJS (ReqBody list a :> sublayout) where
  type JS (ReqBody list a :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqBody .~ True

instance (KnownSymbol path, HasJS sublayout)
      => HasJS (path :> sublayout) where
  type JS (path :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) $
      req & reqUrl.path <>~ [Segment (Static str) []]
          & funcName %~ (++ [str])

    where str = map (\c -> if c == '.' then '_' else c) $ symbolVal (Proxy :: Proxy path)

instance HasJS sublayout => HasJS (RemoteHost :> sublayout) where
  type JS (RemoteHost :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) req

instance HasJS sublayout => HasJS (IsSecure :> sublayout) where
  type JS (IsSecure :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) req

instance HasJS sublayout => HasJS (Vault :> sublayout) where
  type JS (Vault :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) req

instance HasJS sublayout => HasJS (HttpVersion :> sublayout) where
  type JS (HttpVersion :> sublayout) = JS sublayout

  javascriptFor Proxy req =
    javascriptFor (Proxy :: Proxy sublayout) req
