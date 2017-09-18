{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types for possible backends to run client-side `Request` queries
module Servant.Client.Core.Internal.RunClient where

import           Prelude                              ()
import           Prelude.Compat

import           Control.Monad                        (unless)
import           Data.Foldable                        (toList)
import           Data.Proxy                           (Proxy)
import qualified Data.Text                            as T
import           Network.HTTP.Media                   (MediaType, matches,
                                                       parseAccept, (//))
import           Servant.API                          (MimeUnrender,
                                                       contentTypes,
                                                       mimeUnrender)
import           Servant.Client.Core.Internal.Request (Request, Response (..),
                                                       ServantError (..))

class (Monad m) => RunClient m where
  -- | How to make a request.
  runRequest :: Request -> m Response
  throwServantError :: ServantError -> m a
  catchServantError :: m a -> (ServantError -> m a) -> m a

checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application"//"octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwServantError $ InvalidContentTypeHeader response
      Just t' -> return t'

decodedAs :: forall ct a m. (MimeUnrender ct a, RunClient m)
  => Response -> Proxy ct -> m a
decodedAs response contentType = do
  responseContentType <- checkContentTypeHeader response
  unless (any (matches responseContentType) accept) $
    throwServantError $ UnsupportedContentType responseContentType response
  case mimeUnrender contentType $ responseBody response of
    Left err -> throwServantError $ DecodeFailure (T.pack err) response
    Right val -> return val
  where
    accept = toList $ contentTypes contentType
