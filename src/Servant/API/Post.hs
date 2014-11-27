{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Post where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.Server

-- | Endpoint for POST requests. The type variable represents the type of the
-- response body (not the request body, use 'Servant.API.RQBody.RQBody' for
-- that).
--
-- Example:
--
-- >            -- POST /books
-- >            -- with a JSON encoded Book as the request body
-- >            -- returning the just-created Book
-- > type MyApi = "books" :> ReqBody Book :> Post Book
data Post a
  deriving Typeable

-- | When implementing the handler for a 'Post' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT (Int, String) IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we just require that its type has
-- a 'ToJSON' instance and servant takes care of encoding it for you,
-- yielding status code 201 along the way.
instance ToJSON a => HasServer (Post a) where
  type Server (Post a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodPost = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right out ->
            responseLBS status201 [("Content-Type", "application/json")] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodPost =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound
