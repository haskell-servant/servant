{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Servant.API.ContentTypes where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)

data XML deriving Typeable
data HTML deriving Typeable
data JSON deriving Typeable

type ContentTypeBS = ByteString

class Accept ctype where
    isContentType :: Proxy ctype -> ByteString -> Bool
    contentType   :: Proxy ctype -> ContentTypeBS
    isContentType p bs = bs == contentType p

instance Accept HTML where
    contentType _ = "text/html"

instance Accept JSON where
    contentType _ = "application/json"

instance Accept XML where
    contentType _ = "application/xml"

-- | Instantiate this class to register a way of serializing a type based
-- on the @Accept@ header.
class Accept ctype => MimeRender ctype a where
    toByteString  :: Proxy ctype -> a -> ByteString


class AllCTRender list a where
    handleAcceptH :: Proxy list -> ContentTypeBS -> a -> (ByteString, ContentTypeBS)

instance MimeRender ctyp a => AllCTRender '[ctyp] a where
    handleAcceptH _ accept val = (toByteString pctyp val, accept)
      where pctyp = Proxy :: Proxy ctyp

instance ( MimeRender ctyp a
         , AllCTRender ctyps a
         ) => AllCTRender (ctyp ': ctyps) a where
    handleAcceptH _ accept val
        | isContentType pctyp accept = (toByteString pctyp val, accept)
        | otherwise = handleAcceptH pctyps accept val
      where pctyp  = Proxy :: Proxy ctyp
            pctyps = Proxy :: Proxy ctyps
