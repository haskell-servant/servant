{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Put where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.Client
import Servant.Common.BaseUrl
import Servant.Common.Req
import Servant.Docs
import Servant.Server

-- | Endpoint for PUT requests, usually used to update a ressource.
-- The type @a@ is the type of the response body that's returned.
--
-- Example:
--
-- > -- PUT /books/:isbn
-- > -- with a Book as request body, returning the updated Book
-- > type MyApi = "books" :> Capture "isbn" Text :> ReqBody Book :> Put Book
data Put a
  deriving Typeable

-- | When implementing the handler for a 'Put' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Post.Post', the handler code runs in the
-- @EitherT (Int, String) IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we just require that its type has
-- a 'ToJSON' instance and servant takes care of encoding it for you,
-- yielding status code 200 along the way.
instance ToJSON a => HasServer (Put a) where
  type Server (Put a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodPut = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right out ->
            responseLBS ok200 [("Content-Type", "application/json")] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodPut =
        respond $ failWith WrongMethod

    | otherwise = respond $ failWith NotFound

-- | If you have a 'Put' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance FromJSON a => HasClient (Put a) where
  type Client (Put a) = BaseUrl -> EitherT String IO a

  clientWithRoute Proxy req host =
    performRequestJSON methodPut req 200 host

instance ToSample a => HasDocs (Put a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPUT

          action' = action & response.respBody .~ sampleByteString p
                           & response.respStatus .~ 200

          p = Proxy :: Proxy a
