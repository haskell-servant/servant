{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.JQuery
-- Copyright   :  (C) 2014 Alp Mestanogullari
-- License     :  BSD3
-- Maintainer  :  Alp Mestanogullari <alpmestan@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
module Servant.JQuery
  ( jquery
  , generateJS
  , jsForAPI
  , printJS
  , module Servant.JQuery.Internal
  , GenerateCode(..)
  ) where

import Control.Lens
import Data.List
import Data.Monoid
import Data.Proxy
import Servant.API
import Servant.JQuery.Internal

jquery :: HasJQ layout => Proxy layout -> JQ layout
jquery p = jqueryFor p defReq

-- js codegen
generateJS :: AjaxReq -> String
generateJS req = "\n" <>
    "function " <> fname <> "(" <> argsStr <> ")\n"
 <> "{\n"
 <> "  $.ajax(\n"
 <> "    { url: " <> url <> "\n"
 <> "    , success: onSuccess\n"
 <> dataBody
 <> reqheaders
 <> "    , error: onError\n"
 <> "    , type: '" <> method <> "'\n"
 <> "    });\n"
 <> "}\n"

  where argsStr = intercalate ", " args
        args = captures
            ++ map (view argName) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . headerArgName) hs
            ++ ["onSuccess", "onError"]

        captures = map captureArg
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if req ^. reqBody
                 then ["body"]
                 else []

        dataBody =
          if req ^. reqBody
            then "\n    , data: JSON.stringify(body)\n"
            else ""

        reqheaders =
          if null hs
            then ""
            else "\n    , headers: { " ++ headersStr ++ " }\n"

          where headersStr = intercalate ", " $ map headerStr hs
                headerStr header = "\"" ++
                  headerArgName header ++
                  "\": " ++ show header

        fname = req ^. funcName
        method = req ^. reqMethod
        url = if url' == "'" then "'/'" else url'
        url' = "'"
           ++ urlArgs
           ++ queryArgs

        urlArgs = jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" ++ jsParams queryparams

printJS :: AjaxReq -> IO ()
printJS = putStrLn . generateJS

-- | Utility class used by 'jsForAPI' which will
--   directly hand you all the Javascript code
--   instead of handing you a ':<|>'-separated list
--   of 'AjaxReq' like 'jquery' and then having to
--   use 'generateJS' on each 'AjaxReq'.
class GenerateCode reqs where
  jsFor :: reqs -> String

instance GenerateCode AjaxReq where
  jsFor = generateJS

instance GenerateCode rest => GenerateCode (AjaxReq :<|> rest) where
  jsFor (req :<|> rest) = jsFor req ++ jsFor rest

-- | Directly generate all the javascript functions for your API
--   from a 'Proxy' for your API type. You can then write it to
--   a file or integrate it in a page, for example.
jsForAPI :: (HasJQ api, GenerateCode (JQ api)) => Proxy api -> String
jsForAPI p = jsFor (jquery p)
