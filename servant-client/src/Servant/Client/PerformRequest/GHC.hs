{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.PerformRequest.GHC (
  ServantError(..),
  performHttpRequest,
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Client

import           Servant.Client.PerformRequest.Base

performHttpRequest :: Manager -> Request
  -> IO (Either ServantError (Response LBS.ByteString))
performHttpRequest manager request =
  catchConnectionError $ httpLbs request manager

catchConnectionError :: IO a -> IO (Either ServantError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError $ SomeException (e :: HttpException)
