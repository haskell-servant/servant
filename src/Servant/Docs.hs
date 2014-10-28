{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Servant.Docs
-- License     : BSD-style
-- Maintainer  : alpmestan@gmail.com
-- Stability   : provisional
-- Portability : TH, TypeFamilies, DeriveGeneric
--
-- This module lets you get API docs for free. It lets generate
-- an 'API' from the type that represents your API using 'docs':
--
-- @docs :: 'HasDocs' api => 'Proxy' api -> 'API'@
--
-- You can then call 'printMarkdown' on it:
--
-- @printMarkdown :: 'API' -> IO ()@
--
-- or define a custom pretty printer:
--
-- @yourPrettyDocs :: 'API' -> IO () -- or blaze-html's HTML, or ...@
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
-- > import Servant.Docs
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
-- >   toSample Proxy = Just (encode g)
-- >
-- >     where g = Greet "Hello, haskeller!"
-- >
-- > instance ToParam (GetParam "capital" Bool) where
-- >   toParam _ =
-- >     DocGetParam "capital"
-- >                 ["true", "false"]
-- >                 "Get the greeting message in uppercase (true) or not (false). Default is false."
-- >
-- > instance ToCapture (Capture "name" Text) where
-- >   toCapture _ = DocCapture "name" "name of the person to greet"
-- >
-- > instance ToCapture (Capture "greetid" Text) where
-- >   toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"
-- >
-- > -- API specification
-- > type TestApi = 
-- >        "hello" :> Capture "name" Text :> GetParam "capital" Bool :> Get Greet
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
-- > main = printMarkdown greetDocs
module Servant.Docs
  ( -- * 'HasDocs' class and key functions
    HasDocs(..), docs, printMarkdown

  , -- * Classes you need to implement for your types
    ToSample(..), ToParam(..), ToCapture(..)

  , -- * ADTs to represent an 'API'
    Method(..)
  , Endpoint, path, method, defEndpoint
  , API, emptyAPI
  , DocCapture(..), capSymbol, capDesc
  , DocGetParam(..), paramName, paramValues, paramDesc
  , Response, respStatus, respBody, defResponse
  , Action, captures, params, rqbody, response, defAction
  , single

  , -- * Useful modules when defining your own instances
    module Control.Lens
  , module Data.Monoid
  ) where

import Control.Lens hiding (Action)
import Control.Monad (when)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Monoid
import Data.Proxy
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as LB
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

-- | A type to represent /GET/ parameters. Holds its name,
--   the possible values (leave empty if there isn't a finite number of them),
--   and a description of how it influences the output or behavior.
--
-- Write a 'ToParam' instance for your GET parameter types
data DocGetParam = DocGetParam
  { _paramName   :: String   -- type supplied
  , _paramValues :: [String] -- user supplied
  , _paramDesc   :: String   -- user supplied
  } deriving (Eq, Show)

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
  , _params   :: [DocGetParam]       -- type collected + user supplied info
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
makeLenses ''DocGetParam
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
-- >   toSample Proxy = Just (encode g)
-- >
-- >     where g = Greet "Hello, haskeller!"
class ToJSON a => ToSample a where
  toSample :: Proxy a -> Maybe ByteString

-- | The class that helps us automatically get documentation
--   for GET parameters.
--
-- Example of an instance:
--
-- > instance ToParam (GetParam "capital" Bool) where
-- >   toParam _ =
-- >     DocGetParam "capital"
-- >                 ["true", "false"]
-- >                 "Get the greeting message in uppercase (true) or not (false). Default is false."
class ToParam t where
  toParam :: Proxy t -> DocGetParam

-- | The class that helps us automatically get documentation
--   for URL captures.
--
-- Example of an instance:
--
-- > instance ToCapture (Capture "name" Text) where
-- >   toCapture _ = DocCapture "name" "name of the person to greet"
class ToCapture c where
  toCapture :: Proxy c -> DocCapture

-- | Print documentation in Markdown format for
--   the given 'API', on standard output.
printMarkdown :: API -> IO ()
printMarkdown = imapM_ printEndpoint

  where printEndpoint endpoint action = do
          putStrLn $ str
          putStrLn $ replicate len '-'
          putStrLn ""
          capturesStr (action ^. captures)
          paramsStr (action ^. params)
          rqbodyStr (action ^. rqbody)
          responseStr (action ^. response) 

          where str = show (endpoint^.method) ++ " " ++ endpoint^.path
                len = length str

        capturesStr :: [DocCapture] -> IO ()
        capturesStr [] = return ()
        capturesStr l = do
          putStrLn "**Captures**: "
          putStrLn ""
          mapM_ captureStr l
          putStrLn ""
        captureStr cap =
          putStrLn $ "- *" ++ (cap ^. capSymbol) ++ "*: " ++ (cap ^. capDesc)

        paramsStr :: [DocGetParam] -> IO ()
        paramsStr [] = return ()
        paramsStr l = do
          putStrLn "**GET Parameters**: "
          putStrLn ""
          mapM_ paramStr l
          putStrLn ""
        paramStr param = do
          putStrLn $ " - " ++ param ^. paramName
          when (not $ null values) $
            putStrLn $ "     - **Values**: *" ++ intercalate ", " values ++ "*"
          putStrLn $   "     - **Description**: " ++ param ^. paramDesc

          where values = param ^. paramValues

        rqbodyStr :: Maybe ByteString -> IO ()
        rqbodyStr Nothing = return ()
        rqbodyStr (Just b) = do
          putStrLn "**Request Body**: "
          jsonStr b

        jsonStr b = do
          putStrLn ""
          putStrLn "``` javascript"
          LB.putStrLn b
          putStrLn "```"
          putStrLn ""

        responseStr :: Response -> IO ()
        responseStr resp = do
          putStrLn $ "**Response**: "
          putStrLn $ ""
          putStrLn $ " - Status code " ++ show (resp ^. respStatus)
          resp ^. respBody &
            maybe (putStrLn " - No response body\n")
                  (\b -> putStrLn " - Response body as below." >> jsonStr b)
          