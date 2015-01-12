{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Server.ContentTypes where

import Data.Aeson (ToJSON(..), encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Data.String.Conversions (cs)
import qualified Network.HTTP.Media as M


import Servant.API (XML, HTML, JSON, JavaScript, CSS, PlainText)

-- | Instances of 'Accept' represent mimetypes. They are used for matching
-- against the @Accept@ HTTP header of the request, and for setting the
-- @Content-Type@ header of the response
--
-- Example:
--
--   instance Accept HTML where
--      contentType _ = "text" // "html"
--
class Accept ctype where
    contentType   :: Proxy ctype -> M.MediaType

instance Accept HTML where
    contentType _ = "text" M.// "html"

instance Accept JSON where
    contentType _ = "application" M.// "json"

instance Accept XML where
    contentType _ = "application" M.// "xml"

instance Accept JavaScript where
    contentType _ = "application" M.// "javascript"

instance Accept CSS where
    contentType _ = "text" M.// "css"

instance Accept PlainText where
    contentType _ = "text" M.// "plain"

newtype AcceptHeader = AcceptHeader BS.ByteString
    deriving (Eq, Show)

-- | Instantiate this class to register a way of serializing a type based
-- on the @Accept@ header.
class Accept ctype => MimeRender ctype a where
    toByteString  :: Proxy ctype -> a -> ByteString

class AllCTRender list a where
    -- If the Accept header can be matched, returns (Just) a tuple of the
    -- Content-Type and response (serialization of @a@ into the appropriate
    -- mimetype).
    handleAcceptH :: Proxy list -> AcceptHeader -> a -> Maybe (ByteString, ByteString)

instance ( AllMimeRender ctyps a, IsEmpty ctyps ~ 'False
         ) => AllCTRender ctyps a where
    handleAcceptH _ (AcceptHeader accept) val = M.mapAcceptMedia lkup accept
      where pctyps = Proxy :: Proxy ctyps
            amrs = amr pctyps val
            lkup = zip (map fst amrs) $ map (\(a,b) -> (cs $ show a, b)) amrs


--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeRender
--------------------------------------------------------------------------
class AllMimeRender ls a where
    amr :: Proxy ls -> a -> [(M.MediaType, ByteString)] -- list of content-types/response pairs

instance ( MimeRender ctyp a ) => AllMimeRender '[ctyp] a where
    amr _ a = [(contentType pctyp, toByteString pctyp a)]
        where pctyp = Proxy :: Proxy ctyp

instance ( MimeRender ctyp a
         , MimeRender ctyp' a
         , AllMimeRender ctyps a
         ) => AllMimeRender (ctyp ': ctyp' ': ctyps) a where
    amr _ a = (contentType pctyp, toByteString pctyp a)
             :(contentType pctyp', toByteString pctyp' a)
             :(amr pctyps a)
        where pctyp = Proxy :: Proxy ctyp
              pctyps = Proxy :: Proxy ctyps
              pctyp' = Proxy :: Proxy ctyp'


instance AllMimeRender '[] a where
    amr _ _ = []

type family IsEmpty (ls::[*]) where
    IsEmpty '[] = 'True
    IsEmpty x   = 'False

--------------------------------------------------------------------------
-- MimeRender Instances
--------------------------------------------------------------------------

instance ToJSON a => MimeRender JSON a where
    toByteString _ = encode

instance Show a => MimeRender PlainText a where
    toByteString _ = encode . show

instance MimeRender PlainText String where
    toByteString _ = encode
