{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

#include "overlapping-compat.h"
-- | An @HTML@ empty data type with `MimeRender` instances for @blaze-html@'s
-- `ToMarkup` class and `Html` datatype.
-- You should only need to import this module for it's instances and the
-- `HTML` datatype.:
--
-- >>> type Eg = Get '[HTML] a
--
-- Will then check that @a@ has a `ToMarkup` instance, or is `Html`.
module Servant.HTML.Blaze where

import           Data.Typeable                 (Typeable)
import qualified Network.HTTP.Media            as M
import           Servant.API                   (Accept (..), MimeRender (..))
import           Text.Blaze.Html               (Html, ToMarkup, toHtml)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data HTML deriving Typeable

-- | @text/html;charset=utf-8@
instance Accept HTML where
    contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance OVERLAPPABLE_ ToMarkup a => MimeRender HTML a where
    mimeRender _ = renderHtml . toHtml

instance OVERLAPPING_ MimeRender HTML Html where
    mimeRender _ = renderHtml

