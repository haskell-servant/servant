{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Types for possible backends to run client-side `Request` queries
module Servant.Client.Core.Internal.Class where

import           Control.Monad                        (unless)
import           Control.Monad.Error.Class            (MonadError, throwError)
import           Data.Proxy                           (Proxy)
import qualified Data.Text                            as T
import           Network.HTTP.Media                   (MediaType, matches,
                                                       parseAccept, (//))
import           Servant.API                          (MimeUnrender,
                                                       contentTypes,
                                                       mimeUnrender)
import           Servant.Client.Core.Internal.Request (Request, Response (..),
                                                       ServantError (..))
import Data.Foldable (toList)

class (MonadError ServantError m) => RunClient m where
  -- | How to make a request.
  runRequest :: Request -> m Response

checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> pure $ "application"//"octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwError $ InvalidContentTypeHeader response
      Just t' -> pure t'

decodedAs :: forall ct a m. (MimeUnrender ct a, RunClient m)
  => Response -> Proxy ct -> m a
decodedAs response contentType = do
  responseContentType <- checkContentTypeHeader response
  unless (any (matches responseContentType) accept) $
    throwError $ UnsupportedContentType responseContentType response
  case mimeUnrender contentType $ responseBody response of
    Left err -> throwError $ DecodeFailure (T.pack err) response
    Right val -> return val
  where
    accept = toList $ contentTypes contentType
