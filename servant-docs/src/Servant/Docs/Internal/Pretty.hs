{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Docs.Internal.Pretty where

import Data.Aeson               (ToJSON(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Proxy               (Proxy(Proxy))
import Network.HTTP.Media       ((//))
import Servant.API

-- | PrettyJSON content type.
data PrettyJSON

instance Accept PrettyJSON where
    contentType _ = "application" // "json"

instance ToJSON a => MimeRender PrettyJSON a where
    mimeRender _ = encodePretty

-- | Prettify generated JSON documentation.
--
-- @
-- 'docs' ('pretty' ('Proxy' :: 'Proxy' MyAPI))
-- @
pretty :: Proxy api -> Proxy (Pretty api)
pretty Proxy = Proxy

-- | Replace all JSON content types with PrettyJSON.
-- Kind-polymorphic so it can operate on kinds @*@ and @[*]@.
type family Pretty (api :: k) :: k where
    Pretty (x :<|> y)     = Pretty x :<|> Pretty y
    Pretty (x :> y)       = Pretty x :> Pretty y
    Pretty (Get cs r)     = Get     (Pretty cs) r
    Pretty (Post cs r)    = Post    (Pretty cs) r
    Pretty (Put cs r)     = Put     (Pretty cs) r
    Pretty (Delete cs r)  = Delete  (Pretty cs) r
    Pretty (Patch cs r)   = Patch   (Pretty cs) r
    Pretty (ReqBody cs r) = ReqBody (Pretty cs) r
    Pretty (JSON ': xs)   = PrettyJSON ': xs
    Pretty (x ': xs)      = x ': Pretty xs
    Pretty x              = x
