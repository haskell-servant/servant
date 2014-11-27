{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------
-- | This module lets you get API docs for free. It lets generate
-- an 'API' from the type that represents your API using 'docs':
--
-- @docs :: 'HasDocs' api => 'Proxy' api -> 'API'@
--
-- You can then call 'markdown' on it:
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
-- > {-# LANGUAGE PolyKinds #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE TypeOperators #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.Proxy
-- > import Data.Text
-- > import Servant
-- >
-- > -- our type for a Greeting message
-- > data Greet = Greet { _msg :: Text }
-- >   deriving (Generic, Show)
-- >
-- > -- we get our JSON serialization for free
-- > instance FromJSON Greet
-- > instance ToJSON Greet
-- >
-- > -- we provide a sample value for the 'Greet' type
-- > instance ToSample Greet where
-- >   toSample = Just g
-- >
-- >     where g = Greet "Hello, haskeller!"
-- >
-- > instance ToParam (QueryParam "capital" Bool) where
-- >   toParam _ =
-- >     DocQueryParam "capital"
-- >                   ["true", "false"]
-- >                   "Get the greeting message in uppercase (true) or not (false). Default is false."
-- >
-- > instance ToCapture (Capture "name" Text) where
-- >   toCapture _ = DocCapture "name" "name of the person to greet"
-- >
-- > instance ToCapture (Capture "greetid" Text) where
-- >   toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"
-- >
-- > -- API specification
-- > type TestApi =
-- >        "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get Greet
-- >   :<|> "greet" :> RQBody Greet :> Post Greet
-- >   :<|> "delete" :> Capture "greetid" Text :> Delete
-- >
-- > testApi :: Proxy TestApi
-- > testApi = Proxy
-- >
-- > -- Generate the Documentation's ADT
-- > greetDocs :: API
-- > greetDocs = docs testApi
-- >
-- > main :: IO ()
-- > main = putStrLn $ markdown greetDocs
module Servant.Docs
  ( -- * 'HasDocs' class and key functions
    HasDocs(..), docs, markdown

  {- , -- * Serving the documentation
    serveDocumentation -}

  , -- * Classes you need to implement for your types
    ToSample(..)
  , sampleByteString
  , ToParam(..)
  , ToCapture(..)

  , -- * ADTs to represent an 'API'
    Method(..)
  , Endpoint, path, method, defEndpoint
  , API, emptyAPI
  , DocCapture(..), capSymbol, capDesc
  , DocQueryParam(..), ParamKind(..), paramName, paramValues, paramDesc, paramKind
  , Response, respStatus, respBody, defResponse
  , Action, captures, params, rqbody, response, defAction
  , single

  , -- * Useful modules when defining your doc printers
    module Control.Lens
  , module Data.Monoid
  ) where

import Control.Lens hiding (Action)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Monoid
import Data.Proxy
import Data.String.Conversions
import GHC.Generics
import GHC.TypeLits
import Servant

import qualified Data.HashMap.Strict as HM

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
-- λ> 'defEndpoint' & 'path' '<>~' "foo"
-- GET /foo
-- λ> 'defEndpoint' & 'path' '<>~' "foo" & 'method' '.~' 'DocPOST'
-- POST /foo
-- @
data Endpoint = Endpoint
  { _path   :: String -- type collected
  , _method :: Method -- type collected
  } deriving (Eq, Generic)

instance Show Endpoint where
  show (Endpoint p m) =
    show m ++ " " ++ p

-- | An 'Endpoint' whose path is `"/"` and whose method is 'DocGET'
--
-- Here's how you can modify it:
--
-- @
-- λ> 'defEndpoint'
-- GET /
-- λ> 'defEndpoint' & 'path' '<>~' "foo"
-- GET /foo
-- λ> 'defEndpoint' & 'path' '<>~' "foo" & 'method' '.~' 'DocPOST'
-- POST /foo
-- @
defEndpoint :: Endpoint
defEndpoint = Endpoint "/" DocGET

instance Hashable Endpoint

-- | Our API type, a good old hashmap from 'Endpoint' to 'Action'
type API = HashMap Endpoint Action

-- | An empty 'API'
emptyAPI :: API
emptyAPI = HM.empty

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
-- > Response {_respStatus = 200, _respBody = Nothing}
-- > λ> defResponse & respStatus .~ 204 & respBody .~ Just "[]"
-- > Response {_respStatus = 204, _respBody = Just "[]"}
data Response = Response
  { _respStatus :: Int
  , _respBody   :: Maybe ByteString
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
defResponse = Response 200 Nothing

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
  { _captures :: [DocCapture]        -- type collected + user supplied info
  , _params   :: [DocQueryParam]     -- type collected + user supplied info
  , _rqbody   :: Maybe ByteString    -- user supplied
  , _response :: Response            -- user supplied
  } deriving (Eq, Show)

-- Default 'Action'. Has no 'captures', no GET 'params', expects
-- no request body ('rqbody') and the typical response is 'defResponse'.
--
-- Tweakable with lenses.
--
-- > λ> defAction
-- > Action {_captures = [], _params = [], _rqbody = Nothing, _response = Response {_respStatus = 200, _respBody = Nothing}}
-- > λ> defAction & response.respStatus .~ 201
-- > Action {_captures = [], _params = [], _rqbody = Nothing, _response = Response {_respStatus = 201, _respBody = Nothing}}
defAction :: Action
defAction =
  Action []
         []
         Nothing
         defResponse

-- | Create an API that's comprised of a single endpoint.
--   'API' is a 'Monoid', so combine multiple endpoints with
--   'mappend' or '<>'.
single :: Endpoint -> Action -> API
single = HM.singleton

-- gimme some lenses
makeLenses ''Endpoint
makeLenses ''DocCapture
makeLenses ''DocQueryParam
makeLenses ''Response
makeLenses ''Action

-- | Generate the docs for a given API that implements 'HasDocs'.
docs :: HasDocs layout => Proxy layout -> API
docs p = docsFor p (defEndpoint, defAction)

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
class ToJSON a => ToSample a where
  toSample :: Maybe a

instance ToSample () where
  toSample = Just ()

sampleByteString :: forall a . ToSample a => Proxy a -> Maybe ByteString
sampleByteString Proxy = fmap encode (toSample :: Maybe a)

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
markdown = unlines . concat . map (uncurry printEndpoint) . HM.toList

  where printEndpoint :: Endpoint -> Action -> [String]
        printEndpoint endpoint action =
          str :
          replicate len '-' :
          "" :
          capturesStr (action ^. captures) ++
          paramsStr (action ^. params) ++
          rqbodyStr (action ^. rqbody) ++
          responseStr (action ^. response) ++
          []

          where str = show (endpoint^.method) ++ " " ++ endpoint^.path
                len = length str

        capturesStr :: [DocCapture] -> [String]
        capturesStr [] = []
        capturesStr l =
          "**Captures**: " :
          "" :
          map captureStr l ++
          "" :
          []
        captureStr cap =
          "- *" ++ (cap ^. capSymbol) ++ "*: " ++ (cap ^. capDesc)

        paramsStr :: [DocQueryParam] -> [String]
        paramsStr [] = []
        paramsStr l =
          "**GET Parameters**: " :
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

        rqbodyStr :: Maybe ByteString -> [String]
        rqbodyStr Nothing = []
        rqbodyStr (Just b) =
          "**Request Body**: " :
          jsonStr b

        jsonStr b =
          "" :
          "``` javascript" :
          cs b :
          "```" :
          "" :
          []

        responseStr :: Response -> [String]
        responseStr resp =
          "**Response**: " :
          "" :
          (" - Status code " ++ show (resp ^. respStatus)) :
          (resp ^. respBody &
            maybe [" - No response body\n"]
                  (\b -> " - Response body as below." : jsonStr b))

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
          endpoint' = over path (\p -> p++"/:"++symbolVal symP) endpoint
          symP = Proxy :: Proxy sym


instance HasDocs Delete where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocDELETE

          action' = action & response.respBody .~ Nothing
                           & response.respStatus .~ 204

instance ToSample a => HasDocs (Get a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocGET
          action' = action & response.respBody .~ sampleByteString p
          p = Proxy :: Proxy a

instance ToSample a => HasDocs (Post a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPOST

          action' = action & response.respBody .~ sampleByteString p
                           & response.respStatus .~ 201

          p = Proxy :: Proxy a

instance ToSample a => HasDocs (Put a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPUT

          action' = action & response.respBody .~ sampleByteString p
                           & response.respStatus .~ 200

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

instance HasDocs Raw where
  docsFor _proxy (endpoint, action) =
    single endpoint action

instance (ToSample a, HasDocs sublayout)
      => HasDocs (ReqBody a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout

          action' = action & rqbody .~ sampleByteString p
          p = Proxy :: Proxy a

instance (KnownSymbol path, HasDocs sublayout) => HasDocs (path :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action)

    where sublayoutP = Proxy :: Proxy sublayout
          endpoint' = endpoint & path <>~ symbolVal pa
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