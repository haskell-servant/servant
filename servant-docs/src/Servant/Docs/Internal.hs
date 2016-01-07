{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

#include "overlapping-compat.h"
module Servant.Docs.Internal where

import           Control.Applicative
import           Control.Arrow              (second)
import           Control.Lens               (makeLenses, mapped, over, traversed, view, (%~),
                                             (&), (.~), (<>~), (^.), (|>))
import qualified Control.Monad.Omega        as Omega
import           Data.ByteString.Conversion (ToByteString, toByteString)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.CaseInsensitive       as CI
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord                   (comparing)
import           Data.Proxy                 (Proxy(Proxy))
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text, unpack)
import           GHC.Exts                   (Constraint)
import           GHC.Generics
import           GHC.TypeLits
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Utils.Links

import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Network.HTTP.Media         as M
import qualified Network.HTTP.Types         as HTTP

-- | An 'Endpoint' type that holds the 'path' and the 'method'.
--
-- Gets used as the key in the 'API' hashmap. Modify 'defEndpoint'
-- or any 'Endpoint' value you want using the 'path' and 'method'
-- lenses to tweak.
--
-- @
-- λ> 'defEndpoint'
-- GET /
-- λ> 'defEndpoint' & 'path' '<>~' ["foo"]
-- GET /foo
-- λ> 'defEndpoint' & 'path' '<>~' ["foo"] & 'method' '.~' 'HTTP.methodPost'
-- POST /foo
-- @
data Endpoint = Endpoint
  { _path   :: [String]      -- type collected
  , _method :: HTTP.Method   -- type collected
  } deriving (Eq, Ord, Generic)

instance Show Endpoint where
  show (Endpoint p m) =
    show m ++ " " ++ showPath p

-- |
-- Render a path as a '/'-delimited string
--
showPath :: [String] -> String
showPath [] = "/"
showPath ps = concatMap ('/' :) ps

-- | An 'Endpoint' whose path is `"/"` and whose method is @GET@
--
-- Here's how you can modify it:
--
-- @
-- λ> 'defEndpoint'
-- GET /
-- λ> 'defEndpoint' & 'path' '<>~' ["foo"]
-- GET /foo
-- λ> 'defEndpoint' & 'path' '<>~' ["foo"] & 'method' '.~' 'HTTP.methodPost'
-- POST /foo
-- @
defEndpoint :: Endpoint
defEndpoint = Endpoint [] HTTP.methodGet

instance Hashable Endpoint

-- | Our API documentation type, a product of top-level information and a good
-- old hashmap from 'Endpoint' to 'Action'
data API = API
  { _apiIntros    :: [DocIntro]
  , _apiEndpoints :: HashMap Endpoint Action
  } deriving (Eq, Show)

instance Monoid API where
    API a1 b1 `mappend` API a2 b2 = API (a1 <> a2) (b1 <> b2)
    mempty = API mempty mempty

-- | An empty 'API'
emptyAPI :: API
emptyAPI = mempty

-- | A type to represent captures. Holds the name of the capture
--   and a description.
--
-- Write a 'ToCapture' instance for your captured types.
data DocCapture = DocCapture
  { _capSymbol :: String -- type supplied
  , _capDesc   :: String -- user supplied
  } deriving (Eq, Ord, Show)

-- | A type to represent a /GET/ parameter from the Query String. Holds its name,
--   the possible values (leave empty if there isn't a finite number of them),
--   and a description of how it influences the output or behavior.
--
-- Write a 'ToParam' instance for your GET parameter types
data DocQueryParam = DocQueryParam
  { _paramName   :: String   -- type supplied
  , _paramValues :: [String] -- user supplied
  , _paramDesc   :: String   -- user supplied
  , _paramKind   :: ParamKind
  } deriving (Eq, Ord, Show)

-- | An introductory paragraph for your documentation. You can pass these to
-- 'docsWithIntros'.
data DocIntro = DocIntro
  { _introTitle :: String   -- ^ Appears above the intro blob
  , _introBody  :: [String] -- ^ Each String is a paragraph.
  } deriving (Eq, Show)

-- | A type to represent Authentication information about an endpoint.
data DocAuthentication = DocAuthentication
  { _authIntro        :: String
  , _authDataRequired :: String
  } deriving (Eq, Ord, Show)

instance Ord DocIntro where
    compare = comparing _introTitle

-- | A type to represent extra notes that may be attached to an 'Action'.
--
-- This is intended to be used when writing your own HasDocs instances to
-- add extra sections to your endpoint's documentation.
data DocNote = DocNote
  { _noteTitle :: String
  , _noteBody  :: [String]
  } deriving (Eq, Ord, Show)

-- | Type of extra information that a user may wish to "union" with their
-- documentation.
--
-- These are intended to be built using extraInfo.
-- Multiple ExtraInfo may be combined with the monoid instance.
newtype ExtraInfo layout = ExtraInfo (HashMap Endpoint Action)
instance Monoid (ExtraInfo a) where
    mempty = ExtraInfo mempty
    ExtraInfo a `mappend` ExtraInfo b =
        ExtraInfo $ HM.unionWith combineAction a b

-- | Documentation options.
data DocOptions = DocOptions
  { _maxSamples :: Int    -- ^ Maximum samples allowed.
  } deriving (Show)

-- | Default documentation options.
defaultDocOptions :: DocOptions
defaultDocOptions = DocOptions
  { _maxSamples = 5 }

-- | Type of GET parameter:
--
-- - Normal corresponds to @QueryParam@, i.e your usual GET parameter
-- - List corresponds to @QueryParams@, i.e GET parameters with multiple values
-- - Flag corresponds to @QueryFlag@, i.e a value-less GET parameter
data ParamKind = Normal | List | Flag
  deriving (Eq, Ord, Show)

-- | A type to represent an HTTP response. Has an 'Int' status, a list of
-- possible 'MediaType's, and a list of example 'ByteString' response bodies.
-- Tweak 'defResponse' using the 'respStatus', 'respTypes' and 'respBody'
-- lenses if you want.
--
-- If you want to respond with a non-empty response body, you'll most likely
-- want to write a 'ToSample' instance for the type that'll be represented
-- as encoded data in the response.
--
-- Can be tweaked with three lenses.
--
-- > λ> defResponse
-- > Response {_respStatus = 200, _respTypes = [], _respBody = []}
-- > λ> defResponse & respStatus .~ 204 & respBody .~ [("If everything goes well", "{ \"status\": \"ok\" }")]
-- > Response {_respStatus = 204, _respTypes = [], _respBody = [("If everything goes well", "{ \"status\": \"ok\" }")]}
data Response = Response
  { _respStatus  :: Int
  , _respTypes   :: [M.MediaType]
  , _respBody    :: [(Text, M.MediaType, ByteString)]
  , _respHeaders :: [HTTP.Header]
  } deriving (Eq, Ord, Show)

-- | Default response: status code 200, no response body.
--
-- Can be tweaked with two lenses.
--
-- > λ> defResponse
-- > Response {_respStatus = 200, _respBody = Nothing}
-- > λ> defResponse & respStatus .~ 204 & respBody .~ Just "[]"
-- > Response {_respStatus = 204, _respBody = Just "[]"}
defResponse :: Response
defResponse = Response
  { _respStatus  = 200
  , _respTypes   = []
  , _respBody    = []
  , _respHeaders = []
  }

-- | A datatype that represents everything that can happen
-- at an endpoint, with its lenses:
--
-- - List of captures ('captures')
-- - List of GET parameters ('params')
-- - What the request body should look like, if any is requested ('rqbody')
-- - What the response should be if everything goes well ('response')
--
-- You can tweak an 'Action' (like the default 'defAction') with these lenses
-- to transform an action and add some information to it.
data Action = Action
  { _authInfo :: [DocAuthentication]         -- user supplied info
  , _captures :: [DocCapture]                -- type collected + user supplied info
  , _headers  :: [Text]                      -- type collected
  , _params   :: [DocQueryParam]             -- type collected + user supplied info
  , _notes    :: [DocNote]                   -- user supplied
  , _mxParams :: [(String, [DocQueryParam])] -- type collected + user supplied info
  , _rqtypes  :: [M.MediaType]               -- type collected
  , _rqbody   :: [(M.MediaType, ByteString)] -- user supplied
  , _response :: Response                    -- user supplied
  } deriving (Eq, Ord, Show)

-- | Combine two Actions, we can't make a monoid as merging Response breaks the
-- laws.
--
-- As such, we invent a non-commutative, left associative operation
-- 'combineAction' to mush two together taking the response, body and content
-- types from the very left.
combineAction :: Action -> Action -> Action
Action a c h p n m ts body resp `combineAction` Action a' c' h' p' n' m' _ _ _ =
        Action (a <> a') (c <> c') (h <> h') (p <> p') (n <> n') (m <> m') ts body resp

-- Default 'Action'. Has no 'captures', no GET 'params', expects
-- no request body ('rqbody') and the typical response is 'defResponse'.
--
-- Tweakable with lenses.
--
-- > λ> defAction
-- > Action {_captures = [], _headers = [], _params = [], _mxParams = [], _rqbody = Nothing, _response = Response {_respStatus = 200, _respBody = Nothing}}
-- > λ> defAction & response.respStatus .~ 201
-- > Action {_captures = [], _headers = [], _params = [], _mxParams = [], _rqbody = Nothing, _response = Response {_respStatus = 201, _respBody = Nothing}}
defAction :: Action
defAction =
  Action []
         []
         []
         []
         []
         []
         []
         []
         defResponse

-- | Create an API that's comprised of a single endpoint.
--   'API' is a 'Monoid', so combine multiple endpoints with
--   'mappend' or '<>'.
single :: Endpoint -> Action -> API
single e a = API mempty (HM.singleton e a)

-- gimme some lenses
makeLenses ''DocAuthentication
makeLenses ''DocOptions
makeLenses ''API
makeLenses ''Endpoint
makeLenses ''DocCapture
makeLenses ''DocQueryParam
makeLenses ''DocIntro
makeLenses ''DocNote
makeLenses ''Response
makeLenses ''Action

-- | Generate the docs for a given API that implements 'HasDocs'. This is the
-- default way to create documentation.
--
-- prop> docs == docsWithOptions defaultDocOptions
docs :: HasDocs layout => Proxy layout -> API
docs p = docsWithOptions p defaultDocOptions

-- | Generate the docs for a given API that implements 'HasDocs'.
docsWithOptions :: HasDocs layout => Proxy layout -> DocOptions -> API
docsWithOptions p = docsFor p (defEndpoint, defAction)

-- | Closed type family, check if endpoint is exactly within API.

-- We aren't sure what affects how an Endpoint is built up, so we require an
-- exact match.
type family IsIn (endpoint :: *) (api :: *) :: Constraint where
    IsIn e (sa :<|> sb)                = Or (IsIn e sa) (IsIn e sb)
    IsIn (e :> sa) (e :> sb)           = IsIn sa sb
    IsIn e e                           = ()

-- | Create an 'ExtraInfo' that is garunteed to be within the given API layout.
--
-- The safety here is to ensure that you only add custom documentation to an
-- endpoint that actually exists within your API.
--
-- > extra :: ExtraInfo TestApi
-- > extra =
-- >     extraInfo (Proxy :: Proxy ("greet" :> Capture "greetid" Text :> Delete)) $
-- >              defAction & headers <>~ ["unicorns"]
-- >                        & notes   <>~ [ DocNote "Title" ["This is some text"]
-- >                                      , DocNote "Second secton" ["And some more"]
-- >                                      ]

extraInfo :: (IsIn endpoint layout, HasLink endpoint, HasDocs endpoint)
          => Proxy endpoint -> Action -> ExtraInfo layout
extraInfo p action =
    let api = docsFor p (defEndpoint, defAction) defaultDocOptions
    -- Assume one endpoint, HasLink constraint means that we should only ever
    -- point at one endpoint.
    in ExtraInfo $ api ^. apiEndpoints & traversed .~ action

-- | Generate documentation given some extra introductions (in the form of
-- 'DocInfo') and some extra endpoint documentation (in the form of
-- 'ExtraInfo'.
--
-- The extra introductions will be prepended to the top of the documentation,
-- before the specific endpoint documentation. The extra endpoint documentation
-- will be "unioned" with the automatically generated endpoint documentation.
--
-- You are expected to build up the ExtraInfo with the Monoid instance and
-- 'extraInfo'.
--
-- If you only want to add an introduction, use 'docsWithIntros'.
docsWith :: HasDocs layout => DocOptions -> [DocIntro] -> ExtraInfo layout -> Proxy layout -> API
docsWith opts intros (ExtraInfo endpoints) p =
    docsWithOptions p opts
      & apiIntros <>~ intros
      & apiEndpoints %~ HM.unionWith (flip combineAction) endpoints


-- | Generate the docs for a given API that implements 'HasDocs' with with any
-- number of introduction(s)
docsWithIntros :: HasDocs layout => [DocIntro] -> Proxy layout -> API
docsWithIntros intros = docsWith defaultDocOptions intros mempty

-- | The class that abstracts away the impact of API combinators
--   on documentation generation.
class HasDocs layout where
  docsFor :: Proxy layout -> (Endpoint, Action) -> DocOptions -> API

-- | The class that lets us display a sample input or output in the supported
-- content-types when generating documentation for endpoints that either:
--
-- - expect a request body, or
-- - return a non empty response body
--
-- Example of an instance:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.Aeson
-- > import Data.Text
-- > import GHC.Generics
-- >
-- > data Greet = Greet { _msg :: Text }
-- >   deriving (Generic, Show)
-- >
-- > instance FromJSON Greet
-- > instance ToJSON Greet
-- >
-- > instance ToSample Greet where
-- >   toSamples _ = singleSample g
-- >
-- >     where g = Greet "Hello, haskeller!"
--
-- You can also instantiate this class using 'toSamples' instead of
-- 'toSample': it lets you specify different responses along with
-- some context (as 'Text') that explains when you're supposed to
-- get the corresponding response.
class ToSample a where
  toSamples :: Proxy a -> [(Text, a)]
  default toSamples :: (Generic a, GToSample (Rep a)) => Proxy a -> [(Text, a)]
  toSamples = defaultSamples

-- | Sample input or output (if there is at least one).
toSample :: forall a. ToSample a => Proxy a -> Maybe a
toSample _ = snd <$> listToMaybe (toSamples (Proxy :: Proxy a))

-- | No samples.
noSamples :: [(Text, a)]
noSamples = empty

-- | Single sample without description.
singleSample :: a -> [(Text, a)]
singleSample x = [("", x)]

-- | Samples without documentation.
samples :: [a] -> [(Text, a)]
samples = map ("",)

-- | Default sample Generic-based inputs/outputs.
defaultSamples :: forall a. (Generic a, GToSample (Rep a)) => Proxy a -> [(Text, a)]
defaultSamples _ = Omega.runOmega $ second to <$> gtoSamples (Proxy :: Proxy (Rep a))

-- | @'ToSample'@ for Generics.
--
-- The use of @'Omega'@ allows for more productive sample generation.
class GToSample t where
  gtoSamples :: proxy t -> Omega.Omega (Text, t x)

instance GToSample U1 where
  gtoSamples _ = Omega.each (singleSample U1)

instance GToSample V1 where
  gtoSamples _ = empty

instance (GToSample p, GToSample q) => GToSample (p :*: q) where
  gtoSamples _ = render <$> ps <*> qs
    where
      ps = gtoSamples (Proxy :: Proxy p)
      qs = gtoSamples (Proxy :: Proxy q)
      render (ta, a) (tb, b)
        | T.null ta || T.null tb = (ta <> tb, a :*: b)
        | otherwise              = (ta <> ", " <> tb, a :*: b)

instance (GToSample p, GToSample q) => GToSample (p :+: q) where
  gtoSamples _ = lefts <|> rights
    where
      lefts  = second L1 <$> gtoSamples (Proxy :: Proxy p)
      rights = second R1 <$> gtoSamples (Proxy :: Proxy q)

instance ToSample a => GToSample (K1 i a) where
  gtoSamples _ = second K1 <$> Omega.each (toSamples (Proxy :: Proxy a))

instance (GToSample f) => GToSample (M1 i a f) where
  gtoSamples _ = second M1 <$> gtoSamples (Proxy :: Proxy f)


class AllHeaderSamples ls where
    allHeaderToSample :: Proxy ls -> [HTTP.Header]

instance AllHeaderSamples '[] where
    allHeaderToSample _  = []

instance (ToByteString l, AllHeaderSamples ls, ToSample l, KnownSymbol h)
    => AllHeaderSamples (Header h l ': ls) where
    allHeaderToSample _ = mkHeader (toSample (Proxy :: Proxy l)) :
                          allHeaderToSample (Proxy :: Proxy ls)
      where headerName = CI.mk . cs $ symbolVal (Proxy :: Proxy h)
            mkHeader (Just x) = (headerName, cs $ toByteString x)
            mkHeader Nothing  = (headerName, "<no header sample provided>")

-- | Synthesise a sample value of a type, encoded in the specified media types.
sampleByteString
    :: forall ct cts a. (ToSample a, AllMimeRender (ct ': cts) a)
    => Proxy (ct ': cts)
    -> Proxy a
    -> [(M.MediaType, ByteString)]
sampleByteString ctypes@Proxy Proxy =
    maybe [] (allMimeRender ctypes) $ toSample (Proxy :: Proxy a)

-- | Synthesise a list of sample values of a particular type, encoded in the
-- specified media types.
sampleByteStrings
    :: forall ct cts a. (ToSample a, AllMimeRender (ct ': cts) a)
    => Proxy (ct ': cts)
    -> Proxy a
    -> [(Text, M.MediaType, ByteString)]
sampleByteStrings ctypes@Proxy Proxy =
    let samples' = toSamples (Proxy :: Proxy a)
        enc (t, s) = uncurry (t,,) <$> allMimeRender ctypes s
    in concatMap enc samples'

-- | The class that helps us automatically get documentation
--   for GET parameters.
--
-- Example of an instance:
--
-- > instance ToParam (QueryParam "capital" Bool) where
-- >   toParam _ =
-- >     DocQueryParam "capital"
-- >                   ["true", "false"]
-- >                   "Get the greeting message in uppercase (true) or not (false). Default is false."
class ToParam t where
  toParam :: Proxy t -> DocQueryParam

-- | The class that helps us automatically get documentation
--   for URL captures.
--
-- Example of an instance:
--
-- > instance ToCapture (Capture "name" Text) where
-- >   toCapture _ = DocCapture "name" "name of the person to greet"
class ToCapture c where
  toCapture :: Proxy c -> DocCapture

-- | The class that helps us get documentation for authenticated endpoints
class ToAuthInfo a where
      toAuthInfo :: Proxy a -> DocAuthentication

-- | Generate documentation in Markdown format for
--   the given 'API'.
markdown :: API -> String
markdown api = unlines $
       introsStr (api ^. apiIntros)
    ++ (concatMap (uncurry printEndpoint) . sort . HM.toList $ api ^. apiEndpoints)

  where printEndpoint :: Endpoint -> Action -> [String]
        printEndpoint endpoint action =
          str :
          "" :
          notesStr (action ^. notes) ++
          authStr (action ^. authInfo) ++
          capturesStr (action ^. captures) ++
          headersStr (action ^. headers) ++
          paramsStr (action ^. params) ++
          rqbodyStr (action ^. rqtypes) (action ^. rqbody) ++
          responseStr (action ^. response) ++
          []

          where str = "## " ++ BSC.unpack (endpoint^.method)
                    ++ " " ++ showPath (endpoint^.path)

        introsStr :: [DocIntro] -> [String]
        introsStr = concatMap introStr

        introStr :: DocIntro -> [String]
        introStr i =
            ("## " ++ i ^. introTitle) :
            "" :
            intersperse "" (i ^. introBody) ++
            "" :
            []

        notesStr :: [DocNote] -> [String]
        notesStr = concatMap noteStr

        noteStr :: DocNote -> [String]
        noteStr nt =
            ("#### " ++ nt ^. noteTitle) :
            "" :
            intersperse "" (nt ^. noteBody) ++
            "" :
            []


        authStr :: [DocAuthentication] -> [String]
        authStr auths =
          let authIntros = mapped %~ view authIntro $ auths
              clientInfos = mapped %~ view authDataRequired $ auths
          in "#### Authentication":
              "":
              unlines authIntros :
              "":
              "Clients must supply the following data" :
              unlines clientInfos :
              "" :
              []

        capturesStr :: [DocCapture] -> [String]
        capturesStr [] = []
        capturesStr l =
          "#### Captures:" :
          "" :
          map captureStr l ++
          "" :
          []

        captureStr cap =
          "- *" ++ (cap ^. capSymbol) ++ "*: " ++ (cap ^. capDesc)

        headersStr :: [Text] -> [String]
        headersStr [] = []
        headersStr l = [""] ++ map headerStr l ++ [""]

          where headerStr hname = "- This endpoint is sensitive to the value of the **"
                               ++ unpack hname ++ "** HTTP header."

        paramsStr :: [DocQueryParam] -> [String]
        paramsStr [] = []
        paramsStr l =
          "#### GET Parameters:" :
          "" :
          map paramStr l ++
          "" :
          []

        paramStr param = unlines $
          ("- " ++ param ^. paramName) :
          (if (not (null values) || param ^. paramKind /= Flag)
            then ["     - **Values**: *" ++ intercalate ", " values ++ "*"]
            else []) ++
          ("     - **Description**: " ++ param ^. paramDesc) :
          (if (param ^. paramKind == List)
            then ["     - This parameter is a **list**. All GET parameters with the name "
                  ++ param ^. paramName ++ "[] will forward their values in a list to the handler."]
            else []) ++
          (if (param ^. paramKind == Flag)
            then ["     - This parameter is a **flag**. This means no value is expected to be associated to this parameter."]
            else []) ++
          []

          where values = param ^. paramValues

        rqbodyStr :: [M.MediaType] -> [(M.MediaType, ByteString)]-> [String]
        rqbodyStr [] [] = []
        rqbodyStr types s =
            ["#### Request:", ""]
            <> formatTypes types
            <> concatMap formatBody s

        formatTypes [] = []
        formatTypes ts = ["- Supported content types are:", ""]
            <> map (\t -> "    - `" <> show t <> "`") ts
            <> [""]

        formatBody (m, b) =
          "- Example: `" <> cs (show m) <> "`" :
          contentStr m b

        markdownForType mime_type =
            case (M.mainType mime_type, M.subType mime_type) of
                ("text", "html") -> "html"
                ("application", "xml") -> "xml"
                ("application", "json") -> "javascript"
                ("application", "javascript") -> "javascript"
                ("text", "css") -> "css"
                (_, _) -> ""

        contentStr mime_type body =
          "" :
          "```" <> markdownForType mime_type :
          cs body :
          "```" :
          "" :
          []

        responseStr :: Response -> [String]
        responseStr resp =
          "#### Response:" :
          "" :
          ("- Status code " ++ show (resp ^. respStatus)) :
          ("- Headers: " ++ show (resp ^. respHeaders)) :
          "" :
          formatTypes (resp ^. respTypes) ++
          bodies

          where bodies = case resp ^. respBody of
                  []        -> ["- No response body\n"]
                  [("", t, r)] -> "- Response body as below." : contentStr t r
                  xs        ->
                    concatMap (\(ctx, t, r) -> ("- " <> T.unpack ctx) : contentStr t r) xs

-- * Instances

-- | The generated docs for @a ':<|>' b@ just appends the docs
--   for @a@ with the docs for @b@.
instance OVERLAPPABLE_
         (HasDocs layout1, HasDocs layout2)
      => HasDocs (layout1 :<|> layout2) where

  docsFor Proxy (ep, action) = docsFor p1 (ep, action) <> docsFor p2 (ep, action)

    where p1 :: Proxy layout1
          p1 = Proxy

          p2 :: Proxy layout2
          p2 = Proxy

-- | @"books" :> 'Capture' "isbn" Text@ will appear as
-- @/books/:isbn@ in the docs.
instance (KnownSymbol sym, ToCapture (Capture sym a), HasDocs sublayout)
      => HasDocs (Capture sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action')

    where sublayoutP = Proxy :: Proxy sublayout
          captureP = Proxy :: Proxy (Capture sym a)

          action' = over captures (|> toCapture captureP) action
          endpoint' = over path (\p -> p ++ [":" ++ symbolVal symP]) endpoint
          symP = Proxy :: Proxy sym


instance OVERLAPPABLE_
        (ToSample a, AllMimeRender (ct ': cts) a, KnownNat status
        , ReflectMethod method)
    => HasDocs (Verb method status (ct ': cts) a) where
  docsFor Proxy (endpoint, action) DocOptions{..} =
    single endpoint' action'

    where endpoint' = endpoint & method .~ method'
          action' = action & response.respBody .~ take _maxSamples (sampleByteStrings t p)
                           & response.respTypes .~ allMime t
                           & response.respStatus .~ status
          t = Proxy :: Proxy (ct ': cts)
          method' = reflectMethod (Proxy :: Proxy method)
          status = fromInteger $ natVal (Proxy :: Proxy status)
          p = Proxy :: Proxy a

instance OVERLAPPING_
        (ToSample a, AllMimeRender (ct ': cts) a, KnownNat status
        , ReflectMethod method, AllHeaderSamples ls, GetHeaders (HList ls))
    => HasDocs (Verb method status (ct ': cts) (Headers ls a)) where
  docsFor Proxy (endpoint, action) DocOptions{..} =
    single endpoint' action'

    where endpoint' = endpoint & method .~ method'
          action' = action & response.respBody .~ take _maxSamples (sampleByteStrings t p)
                           & response.respTypes .~ allMime t
                           & response.respStatus .~ status
                           & response.respHeaders .~ hdrs
          t = Proxy :: Proxy (ct ': cts)
          hdrs = allHeaderToSample (Proxy :: Proxy ls)
          method' = reflectMethod (Proxy :: Proxy method)
          status = fromInteger $ natVal (Proxy :: Proxy status)
          p = Proxy :: Proxy a

instance (KnownSymbol sym, HasDocs sublayout)
      => HasDocs (Header sym a :> sublayout) where
  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          action' = over headers (|> headername) action
          headername = T.pack $ symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToParam (QueryParam sym a), HasDocs sublayout)
      => HasDocs (QueryParam sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          paramP = Proxy :: Proxy (QueryParam sym a)
          action' = over params (|> toParam paramP) action

instance (KnownSymbol sym, ToParam (QueryParams sym a), HasDocs sublayout)
      => HasDocs (QueryParams sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          paramP = Proxy :: Proxy (QueryParams sym a)
          action' = over params (|> toParam paramP) action


instance (KnownSymbol sym, ToParam (QueryFlag sym), HasDocs sublayout)
      => HasDocs (QueryFlag sym :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          paramP = Proxy :: Proxy (QueryFlag sym)
          action' = over params (|> toParam paramP) action


instance HasDocs Raw where
  docsFor _proxy (endpoint, action) _ =
    single endpoint action

-- TODO: We use 'AllMimeRender' here because we need to be able to show the
-- example data. However, there's no reason to believe that the instances of
-- 'AllMimeUnrender' and 'AllMimeRender' actually agree (or to suppose that
-- both are even defined) for any particular type.
instance (ToSample a, AllMimeRender (ct ': cts) a, HasDocs sublayout)
      => HasDocs (ReqBody (ct ': cts) a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          action' = action & rqbody .~ sampleByteString t p
                           & rqtypes .~ allMime t
          t = Proxy :: Proxy (ct ': cts)
          p = Proxy :: Proxy a

instance (KnownSymbol path, HasDocs sublayout) => HasDocs (path :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action)

    where sublayoutP = Proxy :: Proxy sublayout
          endpoint' = endpoint & path <>~ [symbolVal pa]
          pa = Proxy :: Proxy path

instance HasDocs sublayout => HasDocs (RemoteHost :> sublayout) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy sublayout) ep

instance HasDocs sublayout => HasDocs (IsSecure :> sublayout) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy sublayout) ep

instance HasDocs sublayout => HasDocs (HttpVersion :> sublayout) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy sublayout) ep

instance HasDocs sublayout => HasDocs (Vault :> sublayout) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy sublayout) ep

instance HasDocs sublayout => HasDocs (WithNamedConfig name config sublayout) where
  docsFor Proxy = docsFor (Proxy :: Proxy sublayout)

instance (ToAuthInfo (BasicAuth realm), HasDocs sublayout) => HasDocs (BasicAuth realm :> sublayout) where
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy sublayout) (endpoint, action')
      where
        authProxy = Proxy :: Proxy (BasicAuth realm)
        action' = over authInfo (|> toAuthInfo authProxy) action

instance (ToAuthInfo (AuthProtect tag), HasDocs sublayout) => HasDocs (AuthProtect tag :> sublayout) where
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy sublayout) (endpoint, action')
      where
        authProxy = Proxy :: Proxy (AuthProtect tag)
        action' = over authInfo (|> toAuthInfo authProxy) action

-- ToSample instances for simple types
instance ToSample ()
instance ToSample Bool
instance ToSample Ordering

-- polymorphic ToSample instances
instance (ToSample a, ToSample b) => ToSample (a, b)
instance (ToSample a, ToSample b, ToSample c) => ToSample (a, b, c)
instance (ToSample a, ToSample b, ToSample c, ToSample d) => ToSample (a, b, c, d)
instance (ToSample a, ToSample b, ToSample c, ToSample d, ToSample e) => ToSample (a, b, c, d, e)
instance (ToSample a, ToSample b, ToSample c, ToSample d, ToSample e, ToSample f) => ToSample (a, b, c, d, e, f)
instance (ToSample a, ToSample b, ToSample c, ToSample d, ToSample e, ToSample f, ToSample g) => ToSample (a, b, c, d, e, f, g)

instance ToSample a => ToSample (Maybe a)
instance (ToSample a, ToSample b) => ToSample (Either a b)
instance ToSample a => ToSample [a]

-- ToSample instances for Control.Applicative types
instance ToSample a => ToSample (Const a b)
instance ToSample a => ToSample (ZipList a)

-- ToSample instances for Data.Monoid newtypes
instance ToSample All
instance ToSample Any
instance ToSample a => ToSample (Sum a)
instance ToSample a => ToSample (Product a)
instance ToSample a => ToSample (First a)
instance ToSample a => ToSample (Last a)
instance ToSample a => ToSample (Dual a)
