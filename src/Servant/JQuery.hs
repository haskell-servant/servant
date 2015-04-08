{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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
  , printJS
  , module Servant.JQuery.Internal
  ) where

import Control.Lens
import Data.List
import Data.Monoid
import Data.Proxy
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
