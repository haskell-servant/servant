{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.JQuery
-- License     :  BSD3
-- Maintainer  :  Alp Mestanogullari <alpmestan@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
module Servant.JS
  ( javascript
  , generateJS
  , jsForAPI
  , listFromAPI
  , printJS
  , module Servant.JS.Internal
  , GenerateCode(..)
  , CommonGeneratorOptions(..)
  ) where

import Data.Proxy
import Servant.API
import Servant.JS.Internal

javascript :: HasJS layout => Proxy layout -> JS layout
javascript p = javascriptFor p defReq

printJS :: AjaxReq -> JavaScriptGenerator -> IO ()
printJS req gen = putStrLn (generateJS req gen)

generateJS :: AjaxReq -> JavaScriptGenerator -> String
generateJS req  gen = gen $ req

-- | Utility class used by 'jsForAPI' which will
--   directly hand you all the Javascript code
--   instead of handing you a ':<|>'-separated list
--   of 'AjaxReq' like 'javascript' and then having to
--   use 'generateJS' on each 'AjaxReq'.
class GenerateCode reqs where
  jsFor :: reqs -> JavaScriptGenerator -> String

instance GenerateCode AjaxReq where
  jsFor = generateJS

instance GenerateCode rest => GenerateCode (AjaxReq :<|> rest) where
  jsFor (req :<|> rest) gen = jsFor req gen ++ jsFor rest gen

-- | Directly generate all the javascript functions for your API
--   from a 'Proxy' for your API type. You can then write it to
--   a file or integrate it in a page, for example.
jsForAPI :: (HasJS api, GenerateCode (JS api)) => Proxy api
    -> JavaScriptGenerator -> String
jsForAPI p = jsFor (javascript p)

class GenerateList reqs where
  generateList :: reqs -> [AjaxReq]

instance GenerateList AjaxReq where
  generateList r = [r]

instance GenerateList rest => GenerateList (AjaxReq :<|> rest) where
  generateList (r :<|> rest) = r : generateList rest

listFromAPI :: (HasJS api, GenerateList (JS api)) => Proxy api -> [AjaxReq]
listFromAPI p = generateList (javascript p)
