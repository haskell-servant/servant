{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.Docs.Internal.Pretty where

import           Data.Aeson
                 (ToJSON (..))
import           Data.Aeson.Encode.Pretty
                 (encodePretty)
import           Data.Proxy
                 (Proxy (Proxy))
import           Network.HTTP.Media
                 ((//))
import           Servant.API
import           Servant.API.Verbs

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
    Pretty (GetPartialContent cs r)      = GetPartialContent      (Pretty cs) r
    Pretty (PutResetContent cs r)        = PutResetContent        (Pretty cs) r
    Pretty (PatchResetContent cs r)      = PatchResetContent      (Pretty cs) r
    Pretty (DeleteResetContent cs r)     = DeleteResetContent     (Pretty cs) r
    Pretty (PostResetContent cs r)       = PostResetContent       (Pretty cs) r
    Pretty (GetResetContent cs r)        = GetResetContent        (Pretty cs) r
    Pretty (PutNonAuthoritative cs r)    = PutNonAuthoritative    (Pretty cs) r
    Pretty (PatchNonAuthoritative cs r)  = PatchNonAuthoritative  (Pretty cs) r
    Pretty (DeleteNonAuthoritative cs r) = DeleteNonAuthoritative (Pretty cs) r
    Pretty (PostNonAuthoritative cs r)   = PostNonAuthoritative   (Pretty cs) r
    Pretty (GetNonAuthoritative cs r)    = GetNonAuthoritative    (Pretty cs) r
    Pretty (PutAccepted cs r)            = PutAccepted            (Pretty cs) r
    Pretty (PatchAccepted cs r)          = PatchAccepted          (Pretty cs) r
    Pretty (DeleteAccepted cs r)         = DeleteAccepted         (Pretty cs) r
    Pretty (PostAccepted cs r)           = PostAccepted           (Pretty cs) r
    Pretty (GetAccepted cs r)            = GetAccepted            (Pretty cs) r
    Pretty (PutCreated cs r)             = PutCreated             (Pretty cs) r
    Pretty (PostCreated cs r)            = PostCreated            (Pretty cs) r
    Pretty (ReqBody cs r) = ReqBody (Pretty cs) r
    Pretty (JSON ': xs)   = PrettyJSON ': xs
    Pretty (x ': xs)      = x ': Pretty xs
    Pretty x              = x
