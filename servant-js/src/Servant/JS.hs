{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.JS
-- License     :  BSD3
-- Maintainer  :  Alp Mestanogullari <alpmestan@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generating Javascript code to query your APIs using vanilla Javascript,
-- Angular.js or JQuery.
--
-- Using this package is very simple. Say you have this API type around:
--
-- > type API = "users" :> Get '[JSON] [Users]
-- >       :<|> "messages" :> Get '[JSON] [Message]
--
-- All you need to do to generate the Javascript code is to write a 'Proxy'
-- for this API type:
--
-- > api :: Proxy API
-- > api = Proxy
--
-- And pick one of the generators:
--
-- - 'vanillaJS' and 'vanillaJSWith' generate functions that use
--   /XMLHttpRequest/ to query your endpoints. The former just calls
--   the latter with default code-generation options.
-- - 'jquery' and 'jqueryWith' follow the same pattern except that they
--   generate functions that use /jQuery/'s AJAX functions.
-- - 'angular' and 'angularWith' do the same but use /Angular.js/'s $http
--   service. In addition, we provide 'angularService' and 'angularServiceWith'
--   which produce functions under an Angular service that your controlers
--   can depend on to query the API.
--
-- Let's keep it simple and produce vanilla Javascript code with the default options.
--
-- @
-- jsCode :: String
-- jsCode = 'jsForAPI' api 'vanillaJS'
-- @
--
-- That's it! If you want to write that code to a file:
--
-- @
-- writeJSCode :: IO ()
-- writeJSCode = 'writeJSForAPI' api 'vanillaJS' "./my_api.js"
-- @
--
-- If you want to customize the rendering options, take a look
-- at 'CommonGeneratorOptions' which are generic options common to all the
-- generators. the /xxxWith/ variants all take 'CommonGeneratorOptions' whereas
-- the /xxx/ versions use 'defCommonGeneratorOptions'. Once you have some custom
--
-- > myOptions :: 'CommonGeneratorOptions'
--
-- All you need to do to use it is to use 'vanillaJSWith' and pass it @myOptions@.
--
-- @
-- jsCodeWithMyOptions :: String
-- jsCodeWithMyOptions = 'jsForAPI' api ('vanillaJSWith' myOptions)
-- @
--
-- Follow the same pattern for any other generator.
--
-- /Note/: The Angular generators take an additional type of options,
-- namely 'AngularOptions', to let you tweak aspects of the code generation
-- that are specific to /Angular.js/.
module Servant.JS
  ( -- * Generating javascript code from an API type
    jsForAPI
  , writeJSForAPI
  , JavaScriptGenerator

  , -- * Options common to all generators
    CommonGeneratorOptions(..)
  , defCommonGeneratorOptions

  , -- * Function renamers
    concatCase
  , snakeCase
  , camelCase

  , -- * Vanilla Javascript code generation
    vanillaJS
  , vanillaJSWith

  , -- * JQuery code generation
    jquery
  , jqueryWith

  , -- * Angular.js code generation
    angular
  , angularWith
  , angularService
  , angularServiceWith
  , AngularOptions(..)
  , defAngularOptions

  , -- * Axios code generation
    axios
  , axiosWith
  , AxiosOptions(..)
  , defAxiosOptions

  , -- * Misc.
    listFromAPI
  , javascript
  , HasJS(..)
  , GenerateList(..)
  , AjaxReq
  ) where

import           Data.Proxy
import           Servant.API
import           Servant.JS.Angular
import           Servant.JS.Axios
import           Servant.JS.Internal
import           Servant.JS.JQuery
import           Servant.JS.Vanilla

-- | Generate the data necessary to generate javascript code
--   for all the endpoints of an API, as ':<|>'-separated values
--   of type 'AjaxReq'.
javascript :: HasJS layout => Proxy layout -> JS layout
javascript p = javascriptFor p defReq

-- | Directly generate all the javascript functions for your API
--   from a 'Proxy' for your API type. You can then write it to
--   a file or integrate it in a page, for example.
jsForAPI :: (HasJS api, GenerateList (JS api))
         => Proxy api -- ^ proxy for your API type
         -> JavaScriptGenerator -- ^ js code generator to use (angular, vanilla js, jquery, others)
         -> String              -- ^ a string that you can embed in your pages or write to a file
jsForAPI p gen = gen (listFromAPI p)

-- | Directly generate all the javascript functions for your API
--   from a 'Proxy' for your API type using the given generator
--   and write the resulting code to a file at the given path.
writeJSForAPI :: (HasJS api, GenerateList (JS api))
              => Proxy api -- ^ proxy for your API type
              -> JavaScriptGenerator -- ^ js code generator to use (angular, vanilla js, jquery, others)
              -> FilePath -- ^ path to the file you want to write the resulting javascript code into
              -> IO ()
writeJSForAPI p gen fp = writeFile fp (jsForAPI p gen)

-- | Utility class used by 'jsForAPI' which computes
--   the data needed to generate a function for each endpoint
--   and hands it all back in a list.
class GenerateList reqs where
  generateList :: reqs -> [AjaxReq]

instance GenerateList AjaxReq where
  generateList r = [r]

instance (GenerateList start, GenerateList rest) => GenerateList (start :<|> rest) where
  generateList (start :<|> rest) = (generateList start) ++ (generateList rest)

-- | Generate the necessary data for JS codegen as a list, each 'AjaxReq'
--   describing one endpoint from your API type.
listFromAPI :: (HasJS api, GenerateList (JS api)) => Proxy api -> [AjaxReq]
listFromAPI p = generateList (javascript p)
