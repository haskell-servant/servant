module Servant.Server.Embedded.Types (
    Etag(..)
  , EmbeddedContent(..)
  , EmbeddedEntry(..)
) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol)
import Network.Wai
import Servant.Server
import Servant.Server.Internal
import Servant.Utils.Links

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Endpoint for embedded content.
data EmbeddedContent (mime :: Symbol) = EmbeddedContent

-- | An etag is used to return 304 not modified responses and cache control headers.
-- If the content changes, the etag must change as well.
newtype Etag = Etag B.ByteString

-- | This structure exists at runtime and describes content that has
-- been embedded.
data EmbeddedEntry (mime :: Symbol) = EmbeddedEntry {
    eeEtag :: Maybe Etag
  , eeApp :: Application
}

instance HasServer (EmbeddedContent mime) config where
    type ServerT (EmbeddedContent mime) m = EmbeddedEntry mime
    route Proxy _ entry = LeafRouter $ \request respond -> do
        r <- runDelayed entry
        case r of
            Route e -> (eeApp e) request (respond . Route)
            Fail a -> respond $ Fail a
            FailFatal e -> respond $ FailFatal e

instance HasLink (EmbeddedContent mime) where
    type MkLink (EmbeddedContent mime) = Maybe Etag -> URI
    toLink Proxy lnk metag = uri { uriQuery = q }
        where
            uri = linkURI lnk
            q = case (uriQuery uri, metag) of
                    ("", Just (Etag etag)) -> "?etag=" ++ T.unpack (T.decodeUtf8 etag)
                    (query, Just (Etag etag)) -> query ++ "&etag=" ++ T.unpack (T.decodeUtf8 etag)
                    (query, _) -> query
