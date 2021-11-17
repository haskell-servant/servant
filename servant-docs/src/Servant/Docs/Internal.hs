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

module Servant.Docs.Internal where

import           Prelude ()
import           Prelude.Compat

import           Control.Applicative
import           Control.Arrow
                 (second)
import           Control.Lens
                 (makeLenses, mapped, each, over, set, to, toListOf, traversed, view,
                 _1, (%~), (&), (.~), (<>~), (^.), (|>))
import qualified Data.ByteString.Char8      as BSC
import           Data.ByteString.Lazy.Char8
                 (ByteString)
import qualified Data.CaseInsensitive       as CI
import           Data.Foldable
                 (fold, toList)
import           Data.Hashable
                 (Hashable)
import           Data.HashMap.Strict
                 (HashMap)
import           Data.List.Compat
                 (intercalate, intersperse, sort)
import           Data.List.NonEmpty
                 (NonEmpty ((:|)), groupWith)
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.Monoid
                 (All (..), Any (..), Dual (..), First (..), Last (..),
                 Product (..), Sum (..))
import           Data.Ord
                 (comparing)
import           Data.Proxy
                 (Proxy (Proxy))
import           Data.String.Conversions
                 (cs)
import           Data.Text
                 (Text, unpack)
import           GHC.Generics
                 (Generic, Rep, K1(K1), M1(M1), U1(U1), V1,
                 (:*:)((:*:)), (:+:)(L1, R1))
import qualified GHC.Generics               as G
import           GHC.TypeLits
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.API.TypeLevel

import qualified Data.Universe.Helpers      as U

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
-- >>> defEndpoint
-- "GET" /
--
-- >>> defEndpoint & path <>~ ["foo"]
-- "GET" /foo
--
-- >>> defEndpoint & path <>~ ["foo"] & method .~ HTTP.methodPost
-- "POST" /foo
--
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
-- >>> defEndpoint
-- "GET" /
--
-- >>> defEndpoint & path <>~ ["foo"]
-- "GET" /foo
--
-- >>> defEndpoint & path <>~ ["foo"] & method .~ HTTP.methodPost
-- "POST" /foo
--
defEndpoint :: Endpoint
defEndpoint = Endpoint [] HTTP.methodGet

instance Hashable Endpoint

-- | Our API documentation type, a product of top-level information and a good
-- old hashmap from 'Endpoint' to 'Action'
data API = API
  { _apiIntros    :: [DocIntro]
  , _apiEndpoints :: HashMap Endpoint Action
  } deriving (Eq, Show)

instance Semigroup API where
    (<>) = mappend

instance Monoid API where
    API a1 b1 `mappend` API a2 b2 = API (a1 `mappend` a2)
                                        (HM.unionWith combineAction b1 b2)
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

-- | A type to represent a /GET/ (or other possible 'HTTP.Method')
--   parameter from the Query String. Holds its name, the possible
--   values (leave empty if there isn't a finite number of them), and
--   a description of how it influences the output or behavior.
--
-- Write a 'ToParam' instance for your GET parameter types
data DocQueryParam = DocQueryParam
  { _paramName   :: String   -- type supplied
  , _paramValues :: [String] -- user supplied
  , _paramDesc   :: String   -- user supplied
  , _paramKind   :: ParamKind
  } deriving (Eq, Ord, Show)

-- | A type to represent fragment. Holds the name of the fragment and its description.
--
-- Write a 'ToFragment' instance for your fragment types.
data DocFragment = DocFragment
  { _fragSymbol :: String -- type supplied
  , _fragDesc   :: String -- user supplied
  } deriving (Eq, Ord, Show)

-- | There should be at most one 'Fragment' per API endpoint.
-- So here we are keeping the first occurrence.
combineFragment :: Maybe DocFragment -> Maybe DocFragment -> Maybe DocFragment
Nothing `combineFragment` mdocFragment = mdocFragment
Just docFragment `combineFragment` _ = Just docFragment

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
newtype ExtraInfo api = ExtraInfo (HashMap Endpoint Action)
instance Semigroup (ExtraInfo a) where
    (<>) = mappend
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

-- | Type of GET (or other 'HTTP.Method') parameter:
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
-- Can be tweaked with four lenses.
--
-- >>> defResponse
-- Response {_respStatus = 200, _respTypes = [], _respBody = [], _respHeaders = []}
--
-- >>> defResponse & respStatus .~ 204 & respBody .~ [("If everything goes well", "application/json", "{ \"status\": \"ok\" }")]
-- Response {_respStatus = 204, _respTypes = [], _respBody = [("If everything goes well",application/json,"{ \"status\": \"ok\" }")], _respHeaders = []}
--
data Response = Response
  { _respStatus  :: Int
  , _respTypes   :: [M.MediaType]
  , _respBody    :: [(Text, M.MediaType, ByteString)]
  , _respHeaders :: [HTTP.Header]
  } deriving (Eq, Ord, Show)

-- | Combine two Responses, we can't make a monoid because merging Status breaks
-- the laws.
--
-- As such, we invent a non-commutative, left associative operation
-- 'combineResponse' to mush two together taking the status from the very left.
combineResponse :: Response -> Response -> Response
Response s ts bs hs `combineResponse` Response _ ts' bs' hs'
  = Response s (ts <> ts') (bs <> bs') (hs <> hs')

-- | Default response: status code 200, no response body.
--
-- Can be tweaked with four lenses.
--
-- >>> defResponse
-- Response {_respStatus = 200, _respTypes = [], _respBody = [], _respHeaders = []}
--
-- >>> defResponse & respStatus .~ 204
-- Response {_respStatus = 204, _respTypes = [], _respBody = [], _respHeaders = []}
--
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
-- - List of GET (or other 'HTTP.Method') parameters ('params')
-- - What the request body should look like, if any is requested ('rqbody')
-- - What the response should be if everything goes well ('response')
--
-- You can tweak an 'Action' (like the default 'defAction') with these lenses
-- to transform an action and add some information to it.
data Action = Action
  { _authInfo :: [DocAuthentication]         -- user supplied info
  , _captures :: [DocCapture]                -- type collected + user supplied info
  , _headers  :: [HTTP.Header]               -- type collected
  , _params   :: [DocQueryParam]             -- type collected + user supplied info
  , _fragment :: Maybe DocFragment           -- type collected + user supplied info
  , _notes    :: [DocNote]                   -- user supplied
  , _mxParams :: [(String, [DocQueryParam])] -- type collected + user supplied info
  , _rqtypes  :: [M.MediaType]               -- type collected
  , _rqbody   :: [(Text, M.MediaType, ByteString)] -- user supplied
  , _response :: Response                    -- user supplied
  } deriving (Eq, Ord, Show)

-- | Combine two Actions, we can't make a monoid as merging Response breaks the
-- laws.
--
-- As such, we invent a non-commutative, left associative operation
-- 'combineAction' to mush two together taking the response from the very left.
combineAction :: Action -> Action -> Action
Action a c h p f n m ts body resp
  `combineAction` Action a' c' h' p' f' n' m' ts' body' resp' =
        Action (a <> a') (c <> c') (h <> h') (p <> p') (f `combineFragment` f') (n <> n') (m <> m') (ts <> ts') (body <> body') (resp `combineResponse` resp')

-- | Default 'Action'. Has no 'captures', no query 'params', expects
-- no request body ('rqbody') and the typical response is 'defResponse'.
--
-- Tweakable with lenses.
--
-- >>> defAction
-- Action {_authInfo = [], _captures = [], _headers = [], _params = [], _fragment = Nothing, _notes = [], _mxParams = [], _rqtypes = [], _rqbody = [], _response = Response {_respStatus = 200, _respTypes = [], _respBody = [], _respHeaders = []}}
--
-- >>> defAction & response.respStatus .~ 201
-- Action {_authInfo = [], _captures = [], _headers = [], _params = [], _fragment = Nothing, _notes = [], _mxParams = [], _rqtypes = [], _rqbody = [], _response = Response {_respStatus = 201, _respTypes = [], _respBody = [], _respHeaders = []}}
--
defAction :: Action
defAction =
  Action []
         []
         []
         []
         Nothing
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

-- | How many content-types for each example should be shown?
--
--   @since 0.11.1
data ShowContentTypes = AllContentTypes  -- ^ For each example, show each content type.
                      | FirstContentType -- ^ For each example, show only one content type.
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Customise how an 'API' is converted into documentation.
--
--   @since 0.11.1
data RenderingOptions = RenderingOptions
  { _requestExamples    :: !ShowContentTypes
    -- ^ How many content types to display for request body examples?
  , _responseExamples   :: !ShowContentTypes
    -- ^ How many content types to display for response body examples?
  , _notesHeading       :: !(Maybe String)
    -- ^ Optionally group all 'notes' together under a common heading.
  , _renderCurlBasePath :: !(Maybe String)
    -- ^ Optionally render example curl requests under a common base path (e.g. `http://localhost:80`).
  } deriving (Show)

-- | Default API generation options.
--
--   All content types are shown for both 'requestExamples' and
--   'responseExamples'; 'notesHeading' is set to 'Nothing'
--   (i.e. un-grouped).
--
--   @since 0.11.1
defRenderingOptions :: RenderingOptions
defRenderingOptions = RenderingOptions
  { _requestExamples    = AllContentTypes
  , _responseExamples   = AllContentTypes
  , _notesHeading       = Nothing
  , _renderCurlBasePath = Nothing
  }

-- gimme some lenses
makeLenses ''DocAuthentication
makeLenses ''DocOptions
makeLenses ''API
makeLenses ''Endpoint
makeLenses ''DocCapture
makeLenses ''DocQueryParam
makeLenses ''DocFragment
makeLenses ''DocIntro
makeLenses ''DocNote
makeLenses ''Response
makeLenses ''Action
makeLenses ''RenderingOptions

-- | Generate the docs for a given API that implements 'HasDocs'. This is the
-- default way to create documentation.
--
-- > docs == docsWithOptions defaultDocOptions
--
docs :: HasDocs api => Proxy api -> API
docs p = docsWithOptions p defaultDocOptions

-- | Generate the docs for a given API that implements 'HasDocs'.
docsWithOptions :: HasDocs api => Proxy api -> DocOptions -> API
docsWithOptions p = docsFor p (defEndpoint, defAction)

-- | Create an 'ExtraInfo' that is guaranteed to be within the given API layout.
--
-- The safety here is to ensure that you only add custom documentation to an
-- endpoint that actually exists within your API.
--
-- > extra :: ExtraInfo TestApi
-- > extra =
-- >     extraInfo (Proxy :: Proxy ("greet" :> Capture "greetid" Text :> Delete)) $
-- >              defAction & headers <>~ [("X-Num-Unicorns", 1)]
-- >                        & notes   <>~ [ DocNote "Title" ["This is some text"]
-- >                                      , DocNote "Second section" ["And some more"]
-- >                                      ]

extraInfo :: (IsIn endpoint api, HasLink endpoint, HasDocs endpoint)
          => Proxy endpoint -> Action -> ExtraInfo api
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
docsWith :: HasDocs api => DocOptions -> [DocIntro] -> ExtraInfo api -> Proxy api -> API
docsWith opts intros (ExtraInfo endpoints) p =
    docsWithOptions p opts
      & apiIntros <>~ intros
      & apiEndpoints %~ HM.unionWith (flip combineAction) endpoints


-- | Generate the docs for a given API that implements 'HasDocs' with with any
-- number of introduction(s)
docsWithIntros :: HasDocs api => [DocIntro] -> Proxy api -> API
docsWithIntros intros = docsWith defaultDocOptions intros mempty

-- | The class that abstracts away the impact of API combinators
--   on documentation generation.
class HasDocs api where
  docsFor :: Proxy api -> (Endpoint, Action) -> DocOptions -> API

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
defaultSamples _ = second G.to <$> gtoSamples (Proxy :: Proxy (Rep a))

-- | @'ToSample'@ for Generics.
--
-- Note: we use combinators from "Universe.Data.Helpers" for more productive sample generation.
class GToSample t where
  gtoSamples :: proxy t -> [(Text, t x)]

instance GToSample U1 where
  gtoSamples _ = singleSample U1

instance GToSample V1 where
  gtoSamples _ = empty

instance (GToSample p, GToSample q) => GToSample (p :*: q) where
  gtoSamples _ = U.cartesianProduct render ps qs
    where
      ps = gtoSamples (Proxy :: Proxy p)
      qs = gtoSamples (Proxy :: Proxy q)
      render (ta, a) (tb, b)
        | T.null ta || T.null tb = (ta <> tb, a :*: b)
        | otherwise              = (ta <> ", " <> tb, a :*: b)

instance (GToSample p, GToSample q) => GToSample (p :+: q) where
  gtoSamples _ = lefts U.+++ rights
    where
      lefts  = second L1 <$> gtoSamples (Proxy :: Proxy p)
      rights = second R1 <$> gtoSamples (Proxy :: Proxy q)

instance ToSample a => GToSample (K1 i a) where
  gtoSamples _ = second K1 <$> toSamples (Proxy :: Proxy a)

instance (GToSample f) => GToSample (M1 i a f) where
  gtoSamples _ = second M1 <$> gtoSamples (Proxy :: Proxy f)


class AllHeaderSamples ls where
    allHeaderToSample :: Proxy ls -> [HTTP.Header]

instance AllHeaderSamples '[] where
    allHeaderToSample _  = []

instance (ToHttpApiData l, AllHeaderSamples ls, ToSample l, KnownSymbol h)
    => AllHeaderSamples (Header h l ': ls) where
    allHeaderToSample _ = mkHeader (toSample (Proxy :: Proxy l)) :
                          allHeaderToSample (Proxy :: Proxy ls)
      where headerName = CI.mk . cs $ symbolVal (Proxy :: Proxy h)
            mkHeader (Just x) = (headerName, cs $ toHeader x)
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

-- | The class that helps us automatically get documentation for GET
--   (or other 'HTTP.Method') parameters.
--
-- Example of an instance:
--
-- > instance ToParam (QueryParam' mods "capital" Bool) where
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

-- | The class that helps us get documentation for URL fragments.
--
-- Example of an instance:
--
-- > instance ToFragment (Fragment a) where
-- >   toFragment _ = DocFragment "fragment" "fragment description"
class ToFragment t where
  toFragment :: Proxy t -> DocFragment

-- | Generate documentation in Markdown format for
--   the given 'API'.
--
--   This is equivalent to @'markdownWith' 'defRenderingOptions'@.
markdown :: API -> String
markdown = markdownWith defRenderingOptions

-- | Generate documentation in Markdown format for
--   the given 'API' using the specified options.
--
--   These options allow you to customise aspects such as:
--
--   * Choose how many content-types for each request body example are
--     shown with 'requestExamples'.
--
--   * Choose how many content-types for each response body example
--     are shown with 'responseExamples'.
--
--   For example, to only show the first content-type of each example:
--
--   @
--   markdownWith ('defRenderingOptions'
--                   & 'requestExamples'  '.~' 'FirstContentType'
--                   & 'responseExamples' '.~' 'FirstContentType' )
--                myAPI
--   @
--
--   @since 0.11.1
markdownWith :: RenderingOptions -> API -> String
markdownWith RenderingOptions{..} api = unlines $
       introsStr (api ^. apiIntros)
    ++ (concatMap (uncurry printEndpoint) . sort . HM.toList $ api ^. apiEndpoints)

  where printEndpoint :: Endpoint -> Action -> [String]
        printEndpoint endpoint action =
          str :
          "" :
          notesStr (action ^. notes) ++
          authStr (action ^. authInfo) ++
          capturesStr (action ^. captures) ++
          headersStr (toListOf (headers . each . _1 . to (T.pack . BSC.unpack . CI.original)) action) ++
          paramsStr meth (action ^. params) ++
          fragmentStr (action ^. fragment) ++
          rqbodyStr (action ^. rqtypes) (action ^. rqbody) ++
          responseStr (action ^. response) ++
          maybe [] (curlStr endpoint (action ^. headers) (action ^. rqbody)) _renderCurlBasePath ++
          []

          where str = "## " ++ BSC.unpack meth
                    ++ " " ++ showPath (endpoint^.path)

                meth = endpoint ^. method

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
        notesStr = addHeading
                   . concatMap noteStr
          where
            addHeading nts = maybe nts (\hd -> ("### " ++ hd) : "" : nts) _notesHeading

        noteStr :: DocNote -> [String]
        noteStr nt =
            (hdr ++ nt ^. noteTitle) :
            "" :
            intersperse "" (nt ^. noteBody) ++
            "" :
            []
          where
            hdr | isJust _notesHeading = "#### "
                | otherwise            = "### "

        authStr :: [DocAuthentication] -> [String]
        authStr [] = []
        authStr auths =
          let authIntros = mapped %~ view authIntro $ auths
              clientInfos = mapped %~ view authDataRequired $ auths
          in "### Authentication":
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
          "### Captures:" :
          "" :
          map captureStr l ++
          "" :
          []

        captureStr cap =
          "- *" ++ (cap ^. capSymbol) ++ "*: " ++ (cap ^. capDesc)

        headersStr :: [Text] -> [String]
        headersStr [] = []
        headersStr l =
          "### Headers:" :
          "" :
          map headerStr l ++
          "" :
          []

          where headerStr hname = "- This endpoint is sensitive to the value of the **"
                               ++ unpack hname ++ "** HTTP header."

        paramsStr :: HTTP.Method -> [DocQueryParam] -> [String]
        paramsStr _ [] = []
        paramsStr m l =
          ("### " ++ cs m ++ " Parameters:") :
          "" :
          map (paramStr m) l ++
          "" :
          []

        paramStr m param = unlines $
          ("- " ++ param ^. paramName) :
          (if (not (null values) || param ^. paramKind /= Flag)
            then ["     - **Values**: *" ++ intercalate ", " values ++ "*"]
            else []) ++
          ("     - **Description**: " ++ param ^. paramDesc) :
          (if (param ^. paramKind == List)
            then ["     - This parameter is a **list**. All " ++ cs m ++ " parameters with the name "
                  ++ param ^. paramName ++ "[] will forward their values in a list to the handler."]
            else []) ++
          (if (param ^. paramKind == Flag)
            then ["     - This parameter is a **flag**. This means no value is expected to be associated to this parameter."]
            else []) ++
          []

          where values = param ^. paramValues

        fragmentStr :: Maybe DocFragment -> [String]
        fragmentStr Nothing = []
        fragmentStr (Just frag) =
          [ "### Fragment:", ""
          , "- *" ++ (frag ^. fragSymbol) ++ "*: " ++ (frag ^. fragDesc)
          , ""
          ]

        rqbodyStr :: [M.MediaType] -> [(Text, M.MediaType, ByteString)]-> [String]
        rqbodyStr [] [] = []
        rqbodyStr types s =
            ["### Request:", ""]
            <> formatTypes types
            <> formatBodies _requestExamples s

        formatTypes [] = []
        formatTypes ts = ["- Supported content types are:", ""]
            <> map (\t -> "    - `" <> show t <> "`") ts
            <> [""]

        -- This assumes that when the bodies are created, identical
        -- labels and representations are located next to each other.
        formatBodies :: ShowContentTypes -> [(Text, M.MediaType, ByteString)] -> [String]
        formatBodies ex bds = concatMap formatBody (select bodyGroups)
          where
            bodyGroups :: [(Text, NonEmpty M.MediaType, ByteString)]
            bodyGroups =
              map (\grps -> let (t,_,b) = NE.head grps in (t, fmap (\(_,m,_) -> m) grps, b))
              . groupWith (\(t,_,b) -> (t,b))
              $ bds

            select = case ex of
                       AllContentTypes  -> id
                       FirstContentType -> map (\(t,ms,b) -> (t, NE.head ms :| [], b))

        formatBody :: (Text, NonEmpty M.MediaType, ByteString) -> [String]
        formatBody (t, ms, b) =
          "- " <> title <> " (" <> mediaList ms <> "):" :
          contentStr (NE.head ms) b
          where
            mediaList = fold . NE.intersperse ", " . fmap (\m -> "`" ++ show m ++ "`")

            title
              | T.null t  = "Example"
              | otherwise = cs t

        markdownForType mime_type =
            case (M.mainType mime_type, M.subType mime_type) of
                ("text", "html") -> "html"
                ("application", "xml") -> "xml"
                ("text", "xml") -> "xml"
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
          "### Response:" :
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
                    formatBodies _responseExamples xs

        curlStr :: Endpoint -> [HTTP.Header] -> [(Text, M.MediaType, ByteString)] -> String -> [String]
        curlStr endpoint hdrs reqBodies basePath =
          [  "### Sample Request:"
          , ""
          , "```bash"
          , "curl -X" ++ BSC.unpack (endpoint ^. method) ++ " \\"
          ] <>
          maybe [] pure mbMediaTypeStr <>
          headersStrs <>
          maybe [] pure mbReqBodyStr <>
          [  "  " ++ basePath ++ showPath (endpoint ^. path)
          , "```"
          , ""
          ]

          where escapeQuotes :: String -> String
                escapeQuotes = concatMap $ \c -> case c of
                  '\"' -> "\\\""
                  _ -> [c]
                mbReqBody = listToMaybe reqBodies
                mbMediaTypeStr = mkMediaTypeStr <$> mbReqBody
                headersStrs = mkHeaderStr <$> hdrs
                mbReqBodyStr = mkReqBodyStr <$> mbReqBody
                mkMediaTypeStr (_, media_type, _) =
                  "  -H \"Content-Type: " ++ show media_type ++ "\" \\"
                mkHeaderStr (hdrName, hdrVal) =
                  "  -H \"" ++ escapeQuotes (cs (CI.original hdrName)) ++ ": " ++
                  escapeQuotes (cs hdrVal) ++ "\" \\"
                mkReqBodyStr (_, _, body) = "  -d \"" ++ escapeQuotes (cs body) ++ "\" \\"

-- * Instances

-- | The generated docs for @a ':<|>' b@ just appends the docs
--   for @a@ with the docs for @b@.
instance {-# OVERLAPPABLE #-}
         (HasDocs a, HasDocs b)
      => HasDocs (a :<|> b) where

  docsFor Proxy (ep, action) = docsFor p1 (ep, action) <> docsFor p2 (ep, action)

    where p1 :: Proxy a
          p1 = Proxy

          p2 :: Proxy b
          p2 = Proxy

-- | The generated docs for @'EmptyAPI'@ are empty.
instance HasDocs EmptyAPI where
  docsFor Proxy _ _ = emptyAPI

-- | @"books" :> 'Capture' "isbn" Text@ will appear as
-- @/books/:isbn@ in the docs.
instance (KnownSymbol sym, ToCapture (Capture sym a), HasDocs api)
      => HasDocs (Capture' '[] sym a :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint', action')

    where subApiP = Proxy :: Proxy api
          captureP = Proxy :: Proxy (Capture sym a)

          action' = over captures (|> toCapture captureP) action
          endpoint' = over path (\p -> p ++ [":" ++ symbolVal symP]) endpoint
          symP = Proxy :: Proxy sym

instance (KnownSymbol descr, KnownSymbol sym, HasDocs api)
      => HasDocs (Capture' (Description descr ': mods) sym a :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint', action')

    where subApiP = Proxy :: Proxy api

          docCapture = DocCapture (symbolVal symP) (symbolVal descrP)
          action' = over captures (|> docCapture) action
          endpoint' = over path (\p -> p ++ [":" ++ symbolVal symP]) endpoint
          descrP = Proxy :: Proxy descr
          symP = Proxy :: Proxy sym

instance {-# OVERLAPPABLE #-} HasDocs (Capture' mods sym a :> api)
      => HasDocs (Capture' (mod ': mods) sym a :> api) where

  docsFor Proxy =
    docsFor apiP

    where apiP = Proxy :: Proxy (Capture' mods sym a :> api)


-- | @"books" :> 'CaptureAll' "isbn" Text@ will appear as
-- @/books/:isbn@ in the docs.
instance (KnownSymbol sym, ToCapture (CaptureAll sym a), HasDocs sublayout)
      => HasDocs (CaptureAll sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action')

    where sublayoutP = Proxy :: Proxy sublayout
          captureP = Proxy :: Proxy (CaptureAll sym a)

          action' = over captures (|> toCapture captureP) action
          endpoint' = over path (\p -> p ++ [":" ++ symbolVal symP]) endpoint
          symP = Proxy :: Proxy sym


instance {-# OVERLAPPABLE #-}
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

instance (ReflectMethod method) =>
         HasDocs (NoContentVerb method) where
  docsFor Proxy (endpoint, action) DocOptions{..} =
    single endpoint' action'

    where endpoint' = endpoint & method .~ method'
          action' = action & response.respStatus .~ 204
                           & response.respTypes .~ []
                           & response.respBody .~ []
                           & response.respHeaders .~ []
          method' = reflectMethod (Proxy :: Proxy method)

-- | TODO: mention the endpoint is streaming, its framing strategy
--
-- Also there are no samples.
--
-- TODO: AcceptFraming for content-type
instance {-# OVERLAPPABLE #-}
        (Accept ct, KnownNat status, ReflectMethod method)
    => HasDocs (Stream method status framing ct a) where
  docsFor Proxy (endpoint, action) DocOptions{..} =
    single endpoint' action'

    where endpoint' = endpoint & method .~ method'
          action' = action & response.respTypes .~ allMime t
                           & response.respStatus .~ status
          t = Proxy :: Proxy '[ct]
          method' = reflectMethod (Proxy :: Proxy method)
          status = fromInteger $ natVal (Proxy :: Proxy status)

instance {-# OVERLAPPING #-}
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

instance (ToHttpApiData a, ToSample a, KnownSymbol sym, HasDocs api)
      => HasDocs (Header' mods sym a :> api) where
  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint, action')

    where subApiP = Proxy :: Proxy api
          action' = over headers (|> (headerName, headerVal)) action
          headerName = CI.mk . cs $ symbolVal (Proxy :: Proxy sym)
          headerVal = case toSample (Proxy :: Proxy a) of
            Just x -> cs $ toHeader x
            Nothing -> "<no header sample provided>"

instance (KnownSymbol sym, ToParam (QueryParam' mods sym a), HasDocs api)
      => HasDocs (QueryParam' mods sym a :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint, action')

    where subApiP = Proxy :: Proxy api
          paramP = Proxy :: Proxy (QueryParam' mods sym a)
          action' = over params (|> toParam paramP) action

instance (KnownSymbol sym, ToParam (QueryParams sym a), HasDocs api)
      => HasDocs (QueryParams sym a :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint, action')

    where subApiP = Proxy :: Proxy api
          paramP = Proxy :: Proxy (QueryParams sym a)
          action' = over params (|> toParam paramP) action


instance (KnownSymbol sym, ToParam (QueryFlag sym), HasDocs api)
      => HasDocs (QueryFlag sym :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint, action')

    where subApiP = Proxy :: Proxy api
          paramP = Proxy :: Proxy (QueryFlag sym)
          action' = over params (|> toParam paramP) action

instance (ToFragment (Fragment a), HasDocs api)
      => HasDocs (Fragment a :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint, action')

    where subApiP = Proxy :: Proxy api
          fragmentP = Proxy :: Proxy (Fragment a)
          action' = set fragment (Just (toFragment fragmentP)) action

instance HasDocs Raw where
  docsFor _proxy (endpoint, action) _ =
    single endpoint action


instance (KnownSymbol desc, HasDocs api)
  => HasDocs (Description desc :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint, action')

    where subApiP = Proxy :: Proxy api
          action' = over notes (|> note) action
          note = DocNote (symbolVal (Proxy :: Proxy desc)) []

instance (KnownSymbol desc, HasDocs api)
  => HasDocs (Summary desc :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint, action')

    where subApiP = Proxy :: Proxy api
          action' = over notes (|> note) action
          note = DocNote (symbolVal (Proxy :: Proxy desc)) []

-- TODO: We use 'AllMimeRender' here because we need to be able to show the
-- example data. However, there's no reason to believe that the instances of
-- 'AllMimeUnrender' and 'AllMimeRender' actually agree (or to suppose that
-- both are even defined) for any particular type.
instance (ToSample a, AllMimeRender (ct ': cts) a, HasDocs api)
      => HasDocs (ReqBody' mods (ct ': cts) a :> api) where
  docsFor Proxy (endpoint, action) opts@DocOptions{..} =
    docsFor subApiP (endpoint, action') opts

    where subApiP = Proxy :: Proxy api
          action' :: Action
          action' = action & rqbody .~ take _maxSamples (sampleByteStrings t p)
                           & rqtypes .~ allMime t
          t = Proxy :: Proxy (ct ': cts)
          p = Proxy :: Proxy a

-- | TODO: this instance is incomplete.
instance (HasDocs api, Accept ctype) => HasDocs (StreamBody' mods framing ctype a :> api) where
    docsFor Proxy (endpoint, action) opts =
        docsFor subApiP (endpoint, action') opts
      where
        subApiP = Proxy :: Proxy api

        action' :: Action
        action' = action & rqtypes .~ toList (contentTypes t)

        t = Proxy :: Proxy ctype

instance (KnownSymbol path, HasDocs api) => HasDocs (path :> api) where

  docsFor Proxy (endpoint, action) =
    docsFor subApiP (endpoint', action)

    where subApiP = Proxy :: Proxy api
          endpoint' = endpoint & path <>~ [symbolVal pa]
          pa = Proxy :: Proxy path

instance HasDocs api => HasDocs (RemoteHost :> api) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy api) ep

instance HasDocs api => HasDocs (IsSecure :> api) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy api) ep

instance HasDocs api => HasDocs (HttpVersion :> api) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy api) ep

instance HasDocs api => HasDocs (Vault :> api) where
  docsFor Proxy ep =
    docsFor (Proxy :: Proxy api) ep

instance HasDocs api => HasDocs (WithNamedContext name context api) where
  docsFor Proxy = docsFor (Proxy :: Proxy api)

instance (ToAuthInfo (BasicAuth realm usr), HasDocs api) => HasDocs (BasicAuth realm usr :> api) where
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy api) (endpoint, action')
      where
        authProxy = Proxy :: Proxy (BasicAuth realm usr)
        action' = over authInfo (|> toAuthInfo authProxy) action

-- ToSample instances for simple types
instance ToSample NoContent
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
instance ToSample a => ToSample (NonEmpty a)

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

-- $setup
-- >>> :set -XOverloadedStrings
