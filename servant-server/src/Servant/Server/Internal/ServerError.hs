{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Servant.Server.Internal.ServerError where

import           Control.Exception
                 (Exception)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Typeable
                 (Typeable)
import qualified Network.HTTP.Types    as HTTP
import           Network.Wai
                 (Response, responseLBS)

data ServerError = ServerError
    { errHTTPCode     :: Int
    , errReasonPhrase :: String
    , errBody         :: LBS.ByteString
    , errHeaders      :: [HTTP.Header]
    }
  deriving (Show, Eq, Read, Typeable)

instance Exception ServerError

responseServerError :: ServerError -> Response
responseServerError ServerError{..} = responseLBS status errHeaders errBody
  where
    status = HTTP.mkStatus errHTTPCode (BS.pack errReasonPhrase)

-- | 'err300' Multiple Choices
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err300 { errBody = "I can't choose." }
--
err300 :: ServerError
err300 = ServerError { errHTTPCode = 300
                    , errReasonPhrase = "Multiple Choices"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err301' Moved Permanently
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err301
--
err301 :: ServerError
err301 = ServerError { errHTTPCode = 301
                    , errReasonPhrase = "Moved Permanently"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err302' Found
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err302
--
err302 :: ServerError
err302 = ServerError { errHTTPCode = 302
                    , errReasonPhrase = "Found"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err303' See Other
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err303
--
err303 :: ServerError
err303 = ServerError { errHTTPCode = 303
                    , errReasonPhrase = "See Other"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err304' Not Modified
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err304
--
err304 :: ServerError
err304 = ServerError { errHTTPCode = 304
                    , errReasonPhrase = "Not Modified"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err305' Use Proxy
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err305
--
err305 :: ServerError
err305 = ServerError { errHTTPCode = 305
                    , errReasonPhrase = "Use Proxy"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err307' Temporary Redirect
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err307
--
err307 :: ServerError
err307 = ServerError { errHTTPCode = 307
                    , errReasonPhrase = "Temporary Redirect"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err400' Bad Request
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err400 { errBody = "Your request makes no sense to me." }
--
err400 :: ServerError
err400 = ServerError { errHTTPCode = 400
                    , errReasonPhrase = "Bad Request"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err401' Unauthorized
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err401 { errBody = "Your credentials are invalid." }
--
err401 :: ServerError
err401 = ServerError { errHTTPCode = 401
                    , errReasonPhrase = "Unauthorized"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err402' Payment Required
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err402 { errBody = "You have 0 credits. Please give me $$$." }
--
err402 :: ServerError
err402 = ServerError { errHTTPCode = 402
                    , errReasonPhrase = "Payment Required"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err403' Forbidden
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err403 { errBody = "Please login first." }
--
err403 :: ServerError
err403 = ServerError { errHTTPCode = 403
                    , errReasonPhrase = "Forbidden"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err404' Not Found
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err404 { errBody = "Are you lost?" }
--
err404 :: ServerError
err404 = ServerError { errHTTPCode = 404
                    , errReasonPhrase = "Not Found"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err405' Method Not Allowed
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err405 { errBody = "Your account privileges does not allow for this.  Please pay $$$." }
--
err405 :: ServerError
err405 = ServerError { errHTTPCode = 405
                    , errReasonPhrase = "Method Not Allowed"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err406' Not Acceptable
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err406
--
err406 :: ServerError
err406 = ServerError { errHTTPCode = 406
                    , errReasonPhrase = "Not Acceptable"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err407' Proxy Authentication Required
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err407
--
err407 :: ServerError
err407 = ServerError { errHTTPCode = 407
                    , errReasonPhrase = "Proxy Authentication Required"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err409' Conflict
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err409 { errBody = "Transaction conflicts with 59879cb56c7c159231eeacdd503d755f7e835f74" }
--
err409 :: ServerError
err409 = ServerError { errHTTPCode = 409
                    , errReasonPhrase = "Conflict"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err410' Gone
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err410 { errBody = "I know it was here at some point, but.. I blame bad luck." }
--
err410 :: ServerError
err410 = ServerError { errHTTPCode = 410
                    , errReasonPhrase = "Gone"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err411' Length Required
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError err411
--
err411 :: ServerError
err411 = ServerError { errHTTPCode = 411
                    , errReasonPhrase = "Length Required"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err412' Precondition Failed
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err412 { errBody = "Precondition fail: x < 42 && y > 57" }
--
err412 :: ServerError
err412 = ServerError { errHTTPCode = 412
                    , errReasonPhrase = "Precondition Failed"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err413' Request Entity Too Large
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err413 { errBody = "Request exceeded 64k." }
--
err413 :: ServerError
err413 = ServerError { errHTTPCode = 413
                    , errReasonPhrase = "Request Entity Too Large"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err414' Request-URI Too Large
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err414 { errBody = "Maximum length is 64." }
--
err414 :: ServerError
err414 = ServerError { errHTTPCode = 414
                    , errReasonPhrase = "Request-URI Too Large"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err415' Unsupported Media Type
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err415 { errBody = "Supported media types:  gif, png" }
--
err415 :: ServerError
err415 = ServerError { errHTTPCode = 415
                    , errReasonPhrase = "Unsupported Media Type"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err416' Request range not satisfiable
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err416 { errBody = "Valid range is [0, 424242]." }
--
err416 :: ServerError
err416 = ServerError { errHTTPCode = 416
                    , errReasonPhrase = "Request range not satisfiable"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err417' Expectation Failed
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err417 { errBody = "I found a quux in the request.  This isn't going to work." }
--
err417 :: ServerError
err417 = ServerError { errHTTPCode = 417
                    , errReasonPhrase = "Expectation Failed"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err418' Expectation Failed
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err418 { errBody = "Apologies, this is not a webserver but a teapot." }
--
err418 :: ServerError
err418 = ServerError { errHTTPCode = 418
                    , errReasonPhrase = "I'm a teapot"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err422' Unprocessable Entity
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err422 { errBody = "I understood your request, but can't process it." }
--
err422 :: ServerError
err422 = ServerError { errHTTPCode = 422
                    , errReasonPhrase = "Unprocessable Entity"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err500' Internal Server Error
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err500 { errBody = "Exception in module A.B.C:55.  Have a great day!" }
--
err500 :: ServerError
err500 = ServerError { errHTTPCode = 500
                    , errReasonPhrase = "Internal Server Error"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err501' Not Implemented
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err501 { errBody = "/v1/foo is not supported with quux in the request." }
--
err501 :: ServerError
err501 = ServerError { errHTTPCode = 501
                    , errReasonPhrase = "Not Implemented"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err502' Bad Gateway
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err502 { errBody = "Tried gateway foo, bar, and baz.  None responded." }
--
err502 :: ServerError
err502 = ServerError { errHTTPCode = 502
                    , errReasonPhrase = "Bad Gateway"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err503' Service Unavailable
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err503 { errBody = "We're rewriting in PHP." }
--
err503 :: ServerError
err503 = ServerError { errHTTPCode = 503
                    , errReasonPhrase = "Service Unavailable"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err504' Gateway Time-out
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err504 { errBody = "Backend foobar did not respond in 5 seconds." }
--
err504 :: ServerError
err504 = ServerError { errHTTPCode = 504
                    , errReasonPhrase = "Gateway Time-out"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err505' HTTP Version not supported
--
-- Example usage:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err505 { errBody = "I support HTTP/4.0 only." }
--
err505 :: ServerError
err505 = ServerError { errHTTPCode = 505
                    , errReasonPhrase = "HTTP Version not supported"
                    , errBody = ""
                    , errHeaders = []
                    }
