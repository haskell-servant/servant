{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module T7 where

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Network.Wai
import           Servant

type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> Get '[JSON] String

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b

  where a :: Reader String Int
        a = return 1797

        b :: Reader String String
        b = ask

readerServer :: Server ReaderAPI
readerServer = enter readerToEither readerServerT

  where readerToEither :: Reader String :~> EitherT ServantErr IO
        readerToEither = Nat $ \r -> return (runReader r "hi")

app :: Application
app = serve readerAPI readerServer
