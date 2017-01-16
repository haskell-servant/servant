{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Servant.Server.Internal.ServantErr where

import           Control.Exception (Exception)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Typeable (Typeable)
import qualified Network.HTTP.Types    as HTTP
import           Network.Wai           (Response, responseLBS)

data ServantErr = ServantErr { errHTTPCode     :: Int
                             , errReasonPhrase :: String
                             , errBody         :: LBS.ByteString
                             , errHeaders      :: [HTTP.Header]
                             } deriving (Show, Eq, Read, Typeable)

instance Exception ServantErr

responseServantErr :: ServantErr -> Response
responseServantErr ServantErr{..} = responseLBS status errHeaders errBody
  where
    status = HTTP.mkStatus errHTTPCode (BS.pack errReasonPhrase)

-- | 'err300' Multiple Choices
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err300 { errBody = "I can't choose." }
--
err300 :: ServantErr
err300 = ServantErr { errHTTPCode = 300
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
err301 :: ServantErr
err301 = ServantErr { errHTTPCode = 301
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
err302 :: ServantErr
err302 = ServantErr { errHTTPCode = 302
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
err303 :: ServantErr
err303 = ServantErr { errHTTPCode = 303
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
err304 :: ServantErr
err304 = ServantErr { errHTTPCode = 304
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
err305 :: ServantErr
err305 = ServantErr { errHTTPCode = 305
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
err307 :: ServantErr
err307 = ServantErr { errHTTPCode = 307
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
err400 :: ServantErr
err400 = ServantErr { errHTTPCode = 400
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
err401 :: ServantErr
err401 = ServantErr { errHTTPCode = 401
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
err402 :: ServantErr
err402 = ServantErr { errHTTPCode = 402
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
err403 :: ServantErr
err403 = ServantErr { errHTTPCode = 403
                    , errReasonPhrase = "Forbidden"
                    , errBody = ""
                    , errHeaders = []
                    }

-- | 'err404' Not Found
--
-- Example:
--
-- > failingHandler :: Handler ()
-- > failingHandler = throwError $ err404 { errBody = "(╯°□°）╯︵ ┻━┻)." }
--
err404 :: ServantErr
err404 = ServantErr { errHTTPCode = 404
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
err405 :: ServantErr
err405 = ServantErr { errHTTPCode = 405
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
err406 :: ServantErr
err406 = ServantErr { errHTTPCode = 406
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
err407 :: ServantErr
err407 = ServantErr { errHTTPCode = 407
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
err409 :: ServantErr
err409 = ServantErr { errHTTPCode = 409
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
err410 :: ServantErr
err410 = ServantErr { errHTTPCode = 410
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
err411 :: ServantErr
err411 = ServantErr { errHTTPCode = 411
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
err412 :: ServantErr
err412 = ServantErr { errHTTPCode = 412
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
err413 :: ServantErr
err413 = ServantErr { errHTTPCode = 413
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
err414 :: ServantErr
err414 = ServantErr { errHTTPCode = 414
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
err415 :: ServantErr
err415 = ServantErr { errHTTPCode = 415
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
err416 :: ServantErr
err416 = ServantErr { errHTTPCode = 416
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
err417 :: ServantErr
err417 = ServantErr { errHTTPCode = 417
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
err418 :: ServantErr
err418 = ServantErr { errHTTPCode = 418
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
err422 :: ServantErr
err422 = ServantErr { errHTTPCode = 422
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
err500 :: ServantErr
err500 = ServantErr { errHTTPCode = 500
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
err501 :: ServantErr
err501 = ServantErr { errHTTPCode = 501
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
err502 :: ServantErr
err502 = ServantErr { errHTTPCode = 502
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
err503 :: ServantErr
err503 = ServantErr { errHTTPCode = 503
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
err504 :: ServantErr
err504 = ServantErr { errHTTPCode = 504
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
err505 :: ServantErr
err505 = ServantErr { errHTTPCode = 505
                    , errReasonPhrase = "HTTP Version not supported"
                    , errBody = ""
                    , errHeaders = []
                    }
