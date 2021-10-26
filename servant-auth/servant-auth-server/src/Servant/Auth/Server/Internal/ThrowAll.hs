{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Auth.Server.Internal.ThrowAll where

#if !MIN_VERSION_servant_server(0,16,0)
#define ServerError ServantErr
#endif

import Control.Monad.Error.Class
import Data.Tagged               (Tagged (..))
import Servant                   ((:<|>) (..), ServerError(..))
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Char8 as BS

class ThrowAll a where
  -- | 'throwAll' is a convenience function to throw errors across an entire
  -- sub-API
  --
  --
  -- > throwAll err400 :: Handler a :<|> Handler b :<|> Handler c
  -- >    == throwError err400 :<|> throwError err400 :<|> err400
  throwAll :: ServerError -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
  throwAll e = throwAll e :<|> throwAll e

-- Really this shouldn't be necessary - ((->) a) should be an instance of
-- MonadError, no?
instance {-# OVERLAPPING #-} ThrowAll b => ThrowAll (a -> b) where
  throwAll e = const $ throwAll e

instance {-# OVERLAPPABLE #-} (MonadError ServerError m) => ThrowAll (m a) where
  throwAll = throwError

-- | for @servant <0.11@
instance {-# OVERLAPPING #-} ThrowAll Application where
  throwAll e _req respond
      = respond $ responseLBS (mkStatus (errHTTPCode e) (BS.pack $ errReasonPhrase e))
                              (errHeaders e)
                              (errBody e)

-- | for @servant >=0.11@
instance {-# OVERLAPPING #-} MonadError ServerError m => ThrowAll (Tagged m Application) where
  throwAll e = Tagged $ \_req respond ->
      respond $ responseLBS (mkStatus (errHTTPCode e) (BS.pack $ errReasonPhrase e))
                              (errHeaders e)
                              (errBody e)
