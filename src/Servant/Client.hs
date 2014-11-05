{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Servant.Client where

import Control.Concurrent
import Control.Monad.Catch
import Data.ByteString.Lazy
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.URI
import System.IO.Unsafe

-- * Accessing APIs as a Client

data Scheme = Http | Https
  deriving (Show, Eq, Ord, Generic)

data BaseUrl = BaseUrl {
  baseUrlScheme :: Scheme,
  baseUrlHost :: String,
  baseUrlPort :: Int
 }
  deriving (Show, Eq, Ord, Generic)

httpBaseUrl :: String -> BaseUrl
httpBaseUrl host = BaseUrl Http host 80

-- | 'client' allows you to produce operations to query an API from a client.
client :: HasClient layout => Proxy layout -> Client layout
client p = clientWithRoute p defReq

class HasClient layout where
  type Client layout :: *
  clientWithRoute :: Proxy layout -> Req -> Client layout

data Req = Req
  { reqPath  :: String
  , qs       :: QueryText
  , reqBody  :: ByteString
  }

defReq :: Req
defReq = Req "" [] ""

appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req ++ "/" ++ p }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }

setRQBody :: ByteString -> Req -> Req
setRQBody b req = req { reqBody = b }

reqToRequest :: (Functor m, MonadThrow m) => Req -> BaseUrl -> m Request
reqToRequest req (BaseUrl scheme host port) = fmap (setrqb . setQS ) $ parseUrl url

  where url = show $ nullURI { uriScheme = case scheme of
                                  Http  -> "http:"
                                  Https -> "https:"
                             , uriAuthority = Just $
                                 URIAuth { uriUserInfo = ""
                                         , uriRegName = host
                                         , uriPort = ":" ++ show port
                                         }
                             , uriPath = reqPath req
                             }

        setrqb r = r { requestBody = RequestBodyLBS (reqBody req) }
        setQS = setQueryString $ queryTextToQuery (qs req)

{-# NOINLINE __manager #-}
__manager :: MVar Manager
__manager = unsafePerformIO (newManager defaultManagerSettings >>= newMVar)

__withGlobalManager :: (Manager -> IO a) -> IO a
__withGlobalManager action = modifyMVar __manager $ \ manager -> do
  result <- action manager
  return (manager, result)
