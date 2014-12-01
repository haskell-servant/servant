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
 <> "    , error: onError\n"
 <> "    , type: '" <> method <> "'\n"
 <> "    });\n"
 <> "}\n"

  where argsStr = intercalate ", " args
        args = captures
            ++ map (view argName) queryparams
            ++ body
            ++ ["onSuccess", "onError"]
        
        captures = map captureArg
                 . filter isCapture
                 $ req ^. reqUrl.path

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if req ^. reqBody
                 then ["body"]
                 else []

        dataBody =
          if req ^. reqBody
            then "\n    , data: JSON.stringify(body)\n"
            else ""

        fname = req ^. funcName
        method = req ^. reqMethod
        url = "'"
           ++ urlArgs
           ++ queryArgs

        urlArgs = jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" ++ jsParams queryparams

printJS :: AjaxReq -> IO ()
printJS = putStrLn . generateJS
