{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-------------------------------------------------------------------------------
-- | This module lets you get API docs for free. It lets generate
-- an 'API' from the type that represents your API using 'docs':
--
-- @docs :: 'HasDocs' api => 'Proxy' api -> 'API'@
--
-- Alternately, if you wish to add one or more introductions to your
-- documentation, use 'docsWithIntros':
--
-- @docsWithIntros :: 'HasDocs' api => [DocIntro] -> 'Proxy' api -> 'API'@
--
-- You can then call 'markdown' on the 'API' value:
--
-- @markdown :: 'API' -> String@
--
-- or define a custom pretty printer:
--
-- @yourPrettyDocs :: 'API' -> String -- or blaze-html's HTML, or ...@
--
-- The only thing you'll need to do will be to implement some classes
-- for your captures, get parameters and request or response bodies.
--
-- Here's a little (but complete) example that you can run to see the
-- markdown pretty printer in action:
--
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE TypeOperators #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# OPTIONS_GHC -fno-warn-orphans #-}
-- > import Data.Aeson
-- > import Data.Proxy
-- > import Data.Text(Text)
-- > import GHC.Generics
-- > import Servant.API
-- > import Servant.Docs
-- >
-- > -- * Example
-- >
-- > -- | A greet message data type
-- > newtype Greet = Greet Text
-- >   deriving (Generic, Show)
-- >
-- > instance FromJSON Greet
-- > instance ToJSON Greet
-- >
-- > -- We add some useful annotations to our captures,
-- > -- query parameters and request body to make the docs
-- > -- really helpful.
-- > instance ToCapture (Capture "name" Text) where
-- >   toCapture _ = DocCapture "name" "name of the person to greet"
-- >
-- > instance ToCapture (Capture "greetid" Text) where
-- >   toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"
-- >
-- > instance ToParam (QueryParam "capital" Bool) where
-- >   toParam _ =
-- >     DocQueryParam "capital"
-- >                   ["true", "false"]
-- >                   "Get the greeting message in uppercase (true) or not (false).\
-- >                   \Default is false."
-- >                   Normal
-- >
-- > instance ToParam (MatrixParam "lang" String) where
-- >   toParam _ =
-- >     DocQueryParam "lang"
-- >                   ["en", "sv", "fr"]
-- >                   "Get the greeting message selected language. Default is en."
-- >                   Normal
-- >
-- > instance ToSample Greet where
-- >   toSample = Just $ Greet "Hello, haskeller!"
-- >
-- >   toSamples =
-- >     [ ("If you use ?capital=true", Greet "HELLO, HASKELLER")
-- >     , ("If you use ?capital=false", Greet "Hello, haskeller")
-- >     ]
-- >
-- > intro1 :: DocIntro
-- > intro1 = DocIntro "On proper introductions." -- The title
-- >     [ "Hello there."
-- >     , "As documentation is usually written for humans, it's often useful \
-- >       \to introduce concepts with a few words." ] -- Elements are paragraphs
-- >
-- > intro2 :: DocIntro
-- > intro2 = DocIntro "This title is below the last"
-- >     [ "You'll also note that multiple intros are possible." ]
-- >
-- >
-- > -- API specification
-- > type TestApi =
-- >        -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
-- >        "hello" :> MatrixParam "lang" String :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet
-- >
-- >        -- POST /greet with a Greet as JSON in the request body,
-- >        --             returns a Greet as JSON
-- >   :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet
-- >
-- >        -- DELETE /greet/:greetid
-- >   :<|> "greet" :> Capture "greetid" Text :> Delete
-- >
-- > testApi :: Proxy TestApi
-- > testApi = Proxy
-- >
-- > -- Generate the data that lets us have API docs. This
-- > -- is derived from the type as well as from
-- > -- the 'ToCapture', 'ToParam' and 'ToSample' instances from above.
-- > --
-- > -- If you didn't want intros you could just call:
-- > --
-- > -- > docs testAPI
-- > docsGreet :: API
-- > docsGreet = docsWithIntros [intro1, intro2] testApi
-- >
-- > main :: IO ()
-- > main = putStrLn $ markdown docsGreet
module Servant.Docs
  ( -- * 'HasDocs' class and key functions
    HasDocs(..), docs, docsWithIntros, markdown

  , -- * Classes you need to implement for your types
    ToSample(..)
  , sampleByteString
  , sampleByteStrings
  , ToParam(..)
  , ToCapture(..)

  , -- * ADTs to represent an 'API'
    Method(..)
  , Endpoint, path, method, defEndpoint
  , API, emptyAPI
  , DocCapture(..), capSymbol, capDesc
  , DocQueryParam(..), ParamKind(..), paramName, paramValues, paramDesc, paramKind
  , DocNote(..), noteTitle, noteBody
  , DocIntro(..)
  , Response, respStatus, respBody, defResponse
  , Action, captures, headers, notes, params, rqbody, response, defAction
  , single

  , -- * Useful modules when defining your doc printers
    module Control.Lens
  , module Data.Monoid
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Proxy
import Data.String.Conversions
import Data.Text (Text, pack, unpack)
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.API.ContentTypes

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.HTTP.Media as M

-- | Supported HTTP request methods
data Method = DocDELETE -- ^ the DELETE method
            | DocGET    -- ^ the GET method
            | DocPOST   -- ^ the POST method
            | DocPUT    -- ^ the PUT method
  deriving (Eq, Generic)

instance Show Method where
  show DocGET = "GET"
  show DocPOST = "POST"
  show DocDELETE = "DELETE"
  show DocPUT = "PUT"

instance Hashable Method

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
-- λ> 'defEndpoint' & 'path' '<>~' ["foo"] & 'method' '.~' 'DocPOST'
-- POST /foo
-- @
data Endpoint = Endpoint
  { _path   :: [String] -- type collected
  , _method :: Method   -- type collected
  } deriving (Eq, Generic)

instance Show Endpoint where
  show (Endpoint p m) =
    show m ++ " " ++ showPath p

-- |
-- Render a path as a '/'-delimited string
--
showPath :: [String] -> String
showPath [] = "/"
showPath ps = concatMap ('/' :) ps

-- | An 'Endpoint' whose path is `"/"` and whose method is 'DocGET'
--
-- Here's how you can modify it:
--
-- @
-- λ> 'defEndpoint'
-- GET /
-- λ> 'defEndpoint' & 'path' '<>~' ["foo"]
-- GET /foo
-- λ> 'defEndpoint' & 'path' '<>~' ["foo"] & 'method' '.~' 'DocPOST'
-- POST /foo
-- @
defEndpoint :: Endpoint
defEndpoint = Endpoint [] DocGET

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
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

-- | An introductory paragraph for your documentation. You can pass these to
-- 'docsWithIntros'.
data DocIntro = DocIntro
  { _introTitle :: String   -- ^ Appears above the intro blob
  , _introBody  :: [String] -- ^ Each String is a paragraph.
  } deriving (Eq, Show)

instance Ord DocIntro where
    compare = comparing _introTitle

-- | A type to represent extra notes that may be attached to an 'Action'.
--
-- This is intended to be used when writing your own HasDocs instances to
-- add extra sections to your endpoint's documentation.
data DocNote = DocNote
  { _noteTitle :: String
  , _noteBody  :: [String]
  } deriving (Eq, Show)

-- | Type of GET parameter:
--
-- - Normal corresponds to @QueryParam@, i.e your usual GET parameter
-- - List corresponds to @QueryParams@, i.e GET parameters with multiple values
-- - Flag corresponds to @QueryFlag@, i.e a value-less GET parameter
data ParamKind = Normal | List | Flag
  deriving (Eq, Show)

-- | A type to represent an HTTP response. Has an 'Int' status and
-- a 'Maybe ByteString' response body. Tweak 'defResponse' using
-- the 'respStatus' and 'respBody' lenses if you want.
--
-- If you want to respond with a non-empty response body, you'll most likely
-- want to write a 'ToSample' instance for the type that'll be represented
-- as some JSON in the response.
--
-- Can be tweaked with two lenses.
--
-- > λ> defResponse
-- > Response {_respStatus = 200, _respBody = []}
-- > λ> defResponse & respStatus .~ 204 & respBody .~ [("If everything goes well", "{ \"status\": \"ok\" }")]
-- > Response {_respStatus = 204, _respBody = [("If everything goes well", "{ \"status\": \"ok\" }")]}
data Response = Response
  { _respStatus :: Int
  , _respBody   :: [(Text, M.MediaType, ByteString)]
  } deriving (Eq, Show)

-- | Default response: status code 200, no response body.
--
-- Can be tweaked with two lenses.
--
-- > λ> defResponse
-- > Response {_respStatus = 200, _respBody = Nothing}
-- > λ> defResponse & respStatus .~ 204 & respBody .~ Just "[]"
-- > Response {_respStatus = 204, _respBody = Just "[]"}
defResponse :: Response
defResponse = Response 200 []

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
  { _captures :: [DocCapture]                 -- type collected + user supplied info
  , _headers  :: [Text]                       -- type collected
  , _params   :: [DocQueryParam]              -- type collected + user supplied info
  , _notes    :: [DocNote]           -- user supplied
  , _mxParams :: [(String, [DocQueryParam])]  -- type collected + user supplied info
  , _rqbody   :: Maybe [(M.MediaType, ByteString)] -- user supplied
  , _response :: Response                     -- user supplied
  } deriving (Eq, Show)

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
         Nothing
         defResponse

-- | Create an API that's comprised of a single endpoint.
--   'API' is a 'Monoid', so combine multiple endpoints with
--   'mappend' or '<>'.
single :: Endpoint -> Action -> API
single e a = API mempty (HM.singleton e a)

-- gimme some lenses
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
docs :: HasDocs layout => Proxy layout -> API
docs p = docsFor p (defEndpoint, defAction)

-- | Generate the docs for a given API that implements 'HasDocs' with with any
-- number of introduction(s)
docsWithIntros :: HasDocs layout => [DocIntro] -> Proxy layout -> API
docsWithIntros intros p = docs p & apiIntros <>~ intros

-- | The class that abstracts away the impact of API combinators
--   on documentation generation.
class HasDocs layout where
  docsFor :: Proxy layout -> (Endpoint, Action) -> API

-- | The class that lets us display a sample JSON input or output
--   when generating documentation for endpoints that either:
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
-- >   toSample = Just g
-- >
-- >     where g = Greet "Hello, haskeller!"
--
-- You can also instantiate this class using 'toSamples' instead of
-- 'toSample': it lets you specify different responses along with
-- some context (as 'Text') that explains when you're supposed to
-- get the corresponding response.
class ToJSON a => ToSample a where
  {-# MINIMAL (toSample | toSamples) #-}
  toSample :: Maybe a
  toSample = snd <$> listToMaybe samples
    where samples = toSamples :: [(Text, a)]

  toSamples :: [(Text, a)]
  toSamples = maybe [] (return . ("",)) s
    where s = toSample :: Maybe a

-- | Synthesise a sample value of a type, encoded in the specified media types.
sampleByteString
    :: forall ctypes a. (ToSample a, IsNonEmpty ctypes, AllMimeRender ctypes a)
    => Proxy ctypes
    -> Proxy a
    -> Maybe [(M.MediaType, ByteString)]
sampleByteString ctypes@Proxy Proxy =
    fmap (amr ctypes) (toSample :: Maybe a)

-- | Synthesise a list of sample values of a particular type, encoded in the
-- specified media types.
sampleByteStrings
    :: forall ctypes a. (ToSample a, IsNonEmpty ctypes, AllMimeRender ctypes a)
    => Proxy ctypes
    -> Proxy a
    -> [(Text, M.MediaType, ByteString)]
sampleByteStrings ctypes@Proxy Proxy =
    let samples = toSamples :: [(Text, a)]
        enc (t, s) = (\(m,b) -> (t,m,b)) <$> amr ctypes s
    in concatMap enc samples

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

-- | Generate documentation in Markdown format for
--   the given 'API'.
markdown :: API -> String
markdown api = unlines $
       introsStr (api ^. apiIntros)
    ++ (concatMap (uncurry printEndpoint) . HM.toList $ api ^. apiEndpoints)

  where printEndpoint :: Endpoint -> Action -> [String]
        printEndpoint endpoint action =
          str :
          "" :
          notesStr (action ^. notes) ++
          capturesStr (action ^. captures) ++
          mxParamsStr (action ^. mxParams) ++
          headersStr (action ^. headers) ++
          paramsStr (action ^. params) ++
          rqbodyStr (action ^. rqbody) ++
          responseStr (action ^. response) ++
          []

          where str = "## " ++ show (endpoint^.method)
                    ++ " " ++ showPath (endpoint^.path)

        introsStr :: [DocIntro] -> [String]
        introsStr = concatMap introStr

        introStr :: DocIntro -> [String]
        introStr i =
            ("#### " ++ i ^. introTitle) :
            "" :
            intersperse ""  (i ^. introBody) ++
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

        mxParamsStr :: [(String, [DocQueryParam])] -> [String]
        mxParamsStr [] = []
        mxParamsStr l =
          "#### Matrix Parameters:" :
          "" :
          map segmentStr l ++
          "" :
          []
        segmentStr :: (String, [DocQueryParam]) -> String
        segmentStr (segment, l) = unlines $
          ("**" ++ segment ++ "**:") :
          "" :
          map paramStr l ++
          "" :
          []

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
          (" - " ++ param ^. paramName) :
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

        rqbodyStr :: Maybe [(M.MediaType, ByteString)]-> [String]
        rqbodyStr Nothing = []
        rqbodyStr (Just b) = concatMap formatBody b

        formatBody (m, b) =
          "#### Request Body: `" <> show (M.mainType m <> "/" <> M.subType m) <> "`" :
          contentStr m b

        markdownForType mime_type =
            case (M.mainType mime_type, M.subType mime_type) of
                ("text", "html") -> "html"
                ("application", "xml") -> "xml"
                ("application", "json") -> "javascript"
                ("application", "javascript") -> "javascript"
                (_, _) -> ""

        contentStr mime_type body =
          "" :
          "``` " <> markdownForType mime_type :
          cs body :
          "```" :
          "" :
          []

        responseStr :: Response -> [String]
        responseStr resp =
          "#### Response:" :
          "" :
          (" - Status code " ++ show (resp ^. respStatus)) :
          bodies

          where bodies = case resp ^. respBody of
                  []        -> [" - No response body\n"]
                  [("", t, r)] -> " - Response body as below." : contentStr t r
                  xs        ->
                    concatMap (\(ctx, t, r) -> (" - " <> T.unpack ctx) : contentStr t r) xs

-- * Instances

-- | The generated docs for @a ':<|>' b@ just appends the docs
--   for @a@ with the docs for @b@.
instance (HasDocs layout1, HasDocs layout2)
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


instance HasDocs Delete where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocDELETE

          action' = action & response.respBody .~ []
                           & response.respStatus .~ 204

instance (ToSample a, IsNonEmpty cts, AllMimeRender cts a) => HasDocs (Get cts a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocGET
          action' = action & response.respBody .~ sampleByteStrings t p
          t = Proxy :: Proxy cts
          p = Proxy :: Proxy a


instance (KnownSymbol sym, HasDocs sublayout)
      => HasDocs (Header sym a :> sublayout) where
  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          action' = over headers (|> headername) action
          headername = pack $ symbolVal (Proxy :: Proxy sym)

instance (ToSample a, IsNonEmpty cts, AllMimeRender cts a) => HasDocs (Post cts a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPOST
          action' = action & response.respBody .~ sampleByteStrings t p
                           & response.respStatus .~ 201
          t = Proxy :: Proxy cts
          p = Proxy :: Proxy a

instance (ToSample a, IsNonEmpty cts, AllMimeRender cts a) => HasDocs (Put cts a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPUT
          action' = action & response.respBody .~ sampleByteStrings t p
                           & response.respStatus .~ 200
          t = Proxy :: Proxy cts
          p = Proxy :: Proxy a

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


instance (KnownSymbol sym, ToParam (MatrixParam sym a), HasDocs sublayout)
      => HasDocs (MatrixParam sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action')

    where sublayoutP = Proxy :: Proxy sublayout
          paramP = Proxy :: Proxy (MatrixParam sym a)
          segment = endpoint ^. (path._last)
          segment' = action ^. (mxParams._last._1)
          endpoint' = over (path._last) (\p -> p ++ ";" ++ symbolVal symP ++ "=<value>") endpoint

          action' = if segment' /= segment
                    -- This is the first matrix parameter for this segment, insert a new entry into the mxParams list
                    then over mxParams (|> (segment, [toParam paramP])) action
                    -- We've already inserted a matrix parameter for this segment, append to the existing list
                    else action & mxParams._last._2 <>~ [toParam paramP]
          symP = Proxy :: Proxy sym


instance (KnownSymbol sym, {- ToParam (MatrixParams sym a), -} HasDocs sublayout)
      => HasDocs (MatrixParams sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action)

    where sublayoutP = Proxy :: Proxy sublayout
          endpoint' = over path (\p -> p ++ [";" ++ symbolVal symP ++ "=<value>"]) endpoint
          symP = Proxy :: Proxy sym


instance (KnownSymbol sym, {- ToParam (MatrixFlag sym), -} HasDocs sublayout)
      => HasDocs (MatrixFlag sym :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action)

    where sublayoutP = Proxy :: Proxy sublayout

          endpoint' = over path (\p -> p ++ [";" ++ symbolVal symP]) endpoint
          symP = Proxy :: Proxy sym

instance HasDocs Raw where
  docsFor _proxy (endpoint, action) =
    single endpoint action

-- TODO: We use 'AllMimeRender' here because we need to be able to show the
-- example data. However, there's no reason to believe that the instances of
-- 'AllMimeUnrender' and 'AllMimeRender' actually agree (or to suppose that
-- both are even defined) for any particular type.
instance (ToSample a, IsNonEmpty cts, AllMimeRender cts a, HasDocs sublayout)
      => HasDocs (ReqBody cts a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout
          action' = action & rqbody .~ sampleByteString t p
          t = Proxy :: Proxy cts
          p = Proxy :: Proxy a

instance (KnownSymbol path, HasDocs sublayout) => HasDocs (path :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action)

    where sublayoutP = Proxy :: Proxy sublayout
          endpoint' = endpoint & path <>~ [symbolVal pa]
          pa = Proxy :: Proxy path

{-

-- | Serve your API's docs as markdown embedded in an html \<pre> tag.
--
-- > type MyApi = "users" :> Get [User]
-- >         :<|> "docs   :> Raw
-- >
-- > apiProxy :: Proxy MyApi
-- > apiProxy = Proxy
-- >
-- > server :: Server MyApi
-- > server = listUsers
-- >     :<|> serveDocumentation apiProxy
serveDocumentation :: HasDocs api => Proxy api -> Server Raw
serveDocumentation proxy _request respond =
  respond $ responseLBS ok200 [] $ cs $ toHtml $ markdown $ docs proxy

toHtml :: String -> String
toHtml md =
  "<html>" ++
  "<body>" ++
  "<pre>" ++
  md ++
  "</pre>" ++
  "</body>" ++
  "</html>"
-}
