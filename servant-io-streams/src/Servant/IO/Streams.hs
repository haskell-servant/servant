{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module exports 'ToSourceIO' and 'FromSourceIO' for 'IOStreams.InputStream'
module Servant.IO.Streams where

import           Control.Monad.IO.Class (liftIO)
import qualified System.IO.Streams.Core as IOS
import           Servant.API.Stream
import qualified Servant.Types.SourceT  as S

instance ToSourceIO a (IOS.InputStream a) where
  toSourceIO src = S.SourceT ($ go)
    where
      go = S.Effect $ trans <$> IOS.read src

      trans Nothing = S.Stop
      trans (Just c) = S.Yield c go

instance FromSourceIO a (IOS.InputStream a) where
  fromSourceIO src = S.unSourceT src $ IOS.fromGenerator . gen
    where
      gen S.Stop = pure ()
      gen (S.Error s) = liftIO $ fail s
      gen (S.Skip s) = gen s
      gen (S.Yield a s) = IOS.yield a >> gen s
      gen (S.Effect ms) = liftIO ms >>= gen
