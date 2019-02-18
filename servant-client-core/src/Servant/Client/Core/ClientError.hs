{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Servant.Client.Core.ClientError (
    ClientError (..),
    ) where

import           Prelude ()
import           Prelude.Compat

import           Control.DeepSeq
                 (NFData (..))
import           Control.Exception
                 (SomeException (..))
import           Control.Monad.Catch
                 (Exception)
import qualified Data.ByteString              as BS
import           Data.Text
                 (Text)
import           Data.Typeable
                 (Typeable, typeOf)
import           GHC.Generics
                 (Generic)
import           Network.HTTP.Media
                 (MediaType)
import           Network.HTTP.Types ()

import           Servant.Client.Core.BaseUrl
import           Servant.Client.Core.Internal
                 (mediaTypeRnf)
import           Servant.Client.Core.Request
import           Servant.Client.Core.Response


-- | A type representing possible errors in a request
--
-- Note that this type substantially changed in 0.12.
data ClientError =
  -- | The server returned an error response including the
  -- failing request. 'requestPath' includes the 'BaseUrl' and the
  -- path of the request.
    FailureResponse (RequestF () (BaseUrl, BS.ByteString)) Response
  -- | The body could not be decoded at the expected type
  | DecodeFailure Text Response
  -- | The content-type of the response is not supported
  | UnsupportedContentType MediaType Response
  -- | The content-type header is invalid
  | InvalidContentTypeHeader Response
  -- | There was a connection error, and no response was received
  | ConnectionError SomeException
  deriving (Show, Generic, Typeable)

instance Eq ClientError where
  FailureResponse req res     == FailureResponse req' res'     = req == req' && res == res'
  DecodeFailure t r           == DecodeFailure t' r'           = t == t' && r == r'
  UnsupportedContentType mt r == UnsupportedContentType mt' r' = mt == mt' && r == r'
  InvalidContentTypeHeader r  == InvalidContentTypeHeader r'   = r == r'
  ConnectionError exc         == ConnectionError exc'          = eqSomeException exc exc'
    where
      -- returns true, if type of exception is the same
      eqSomeException (SomeException a) (SomeException b) = typeOf a == typeOf b

  -- prevent wild card blindness
  FailureResponse          {} == _ = False
  DecodeFailure            {} == _ = False
  UnsupportedContentType   {} == _ = False
  InvalidContentTypeHeader {} == _ = False
  ConnectionError          {} == _ = False

instance Exception ClientError

-- | Note: an exception in 'ConnectionError' might not be evaluated fully,
-- We only 'rnf' its 'show'ed value.
instance NFData ClientError where
    rnf (FailureResponse req res)        = rnf req `seq` rnf res
    rnf (DecodeFailure err res)          = rnf err `seq` rnf res
    rnf (UnsupportedContentType mt' res) = mediaTypeRnf mt' `seq` rnf res
    rnf (InvalidContentTypeHeader res)   = rnf res
    rnf (ConnectionError err)            = err `seq` rnf (show err)
