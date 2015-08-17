{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Servant.Server.Internal.ServantErr where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Network.HTTP.Types    as HTTP
import           Network.Wai           (Response, responseLBS)

data ServantErr = ServantErr { errHTTPCode     :: Int
                             , errReasonPhrase :: String
                             , errBody         :: LBS.ByteString
                             , errHeaders      :: [HTTP.Header]
                             } deriving (Show, Eq)

responseServantErr :: ServantErr -> Response
responseServantErr ServantErr{..} = responseLBS status errHeaders errBody
  where
    status = HTTP.mkStatus errHTTPCode (BS.pack errReasonPhrase)

err300 :: ServantErr
err300 = ServantErr { errHTTPCode = 300
                    , errReasonPhrase = "Multiple Choices"
                    , errBody = ""
                    , errHeaders = []
                    }

err301 :: ServantErr
err301 = ServantErr { errHTTPCode = 301
                    , errReasonPhrase = "Moved Permanently"
                    , errBody = ""
                    , errHeaders = []
                    }

err302 :: ServantErr
err302 = ServantErr { errHTTPCode = 302
                    , errReasonPhrase = "Found"
                    , errBody = ""
                    , errHeaders = []
                    }

err303 :: ServantErr
err303 = ServantErr { errHTTPCode = 303
                    , errReasonPhrase = "See Other"
                    , errBody = ""
                    , errHeaders = []
                    }

err304 :: ServantErr
err304 = ServantErr { errHTTPCode = 304
                    , errReasonPhrase = "Not Modified"
                    , errBody = ""
                    , errHeaders = []
                    }

err305 :: ServantErr
err305 = ServantErr { errHTTPCode = 305
                    , errReasonPhrase = "Use Proxy"
                    , errBody = ""
                    , errHeaders = []
                    }

err307 :: ServantErr
err307 = ServantErr { errHTTPCode = 307
                    , errReasonPhrase = "Temporary Redirect"
                    , errBody = ""
                    , errHeaders = []
                    }

err400 :: ServantErr
err400 = ServantErr { errHTTPCode = 400
                    , errReasonPhrase = "Bad Request"
                    , errBody = ""
                    , errHeaders = []
                    }

err401 :: ServantErr
err401 = ServantErr { errHTTPCode = 401
                    , errReasonPhrase = "Unauthorized"
                    , errBody = ""
                    , errHeaders = []
                    }

err402 :: ServantErr
err402 = ServantErr { errHTTPCode = 402
                    , errReasonPhrase = "Payment Required"
                    , errBody = ""
                    , errHeaders = []
                    }

err403 :: ServantErr
err403 = ServantErr { errHTTPCode = 403
                    , errReasonPhrase = "Forbidden"
                    , errBody = ""
                    , errHeaders = []
                    }

err404 :: ServantErr
err404 = ServantErr { errHTTPCode = 404
                    , errReasonPhrase = "Not Found"
                    , errBody = ""
                    , errHeaders = []
                    }

err405 :: ServantErr
err405 = ServantErr { errHTTPCode = 405
                    , errReasonPhrase = "Method Not Allowed"
                    , errBody = ""
                    , errHeaders = []
                    }

err406 :: ServantErr
err406 = ServantErr { errHTTPCode = 406
                    , errReasonPhrase = "Not Acceptable"
                    , errBody = ""
                    , errHeaders = []
                    }

err407 :: ServantErr
err407 = ServantErr { errHTTPCode = 407
                    , errReasonPhrase = "Proxy Authentication Required"
                    , errBody = ""
                    , errHeaders = []
                    }

err409 :: ServantErr
err409 = ServantErr { errHTTPCode = 409
                    , errReasonPhrase = "Conflict"
                    , errBody = ""
                    , errHeaders = []
                    }

err410 :: ServantErr
err410 = ServantErr { errHTTPCode = 410
                    , errReasonPhrase = "Gone"
                    , errBody = ""
                    , errHeaders = []
                    }

err411 :: ServantErr
err411 = ServantErr { errHTTPCode = 411
                    , errReasonPhrase = "Length Required"
                    , errBody = ""
                    , errHeaders = []
                    }

err412 :: ServantErr
err412 = ServantErr { errHTTPCode = 412
                    , errReasonPhrase = "Precondition Failed"
                    , errBody = ""
                    , errHeaders = []
                    }

err413 :: ServantErr
err413 = ServantErr { errHTTPCode = 413
                    , errReasonPhrase = "Request Entity Too Large"
                    , errBody = ""
                    , errHeaders = []
                    }

err414 :: ServantErr
err414 = ServantErr { errHTTPCode = 414
                    , errReasonPhrase = "Request-URI Too Large"
                    , errBody = ""
                    , errHeaders = []
                    }

err415 :: ServantErr
err415 = ServantErr { errHTTPCode = 415
                    , errReasonPhrase = "Unsupported Media Type"
                    , errBody = ""
                    , errHeaders = []
                    }

err416 :: ServantErr
err416 = ServantErr { errHTTPCode = 416
                    , errReasonPhrase = "Request range not satisfiable"
                    , errBody = ""
                    , errHeaders = []
                    }

err417 :: ServantErr
err417 = ServantErr { errHTTPCode = 417
                    , errReasonPhrase = "Expectation Failed"
                    , errBody = ""
                    , errHeaders = []
                    }

err500 :: ServantErr
err500 = ServantErr { errHTTPCode = 500
                    , errReasonPhrase = "Internal Server Error"
                    , errBody = ""
                    , errHeaders = []
                    }

err501 :: ServantErr
err501 = ServantErr { errHTTPCode = 501
                    , errReasonPhrase = "Not Implemented"
                    , errBody = ""
                    , errHeaders = []
                    }

err502 :: ServantErr
err502 = ServantErr { errHTTPCode = 502
                    , errReasonPhrase = "Bad Gateway"
                    , errBody = ""
                    , errHeaders = []
                    }

err503 :: ServantErr
err503 = ServantErr { errHTTPCode = 503
                    , errReasonPhrase = "Service Unavailable"
                    , errBody = ""
                    , errHeaders = []
                    }

err504 :: ServantErr
err504 = ServantErr { errHTTPCode = 504
                    , errReasonPhrase = "Gateway Time-out"
                    , errBody = ""
                    , errHeaders = []
                    }

err505 :: ServantErr
err505 = ServantErr { errHTTPCode = 505
                    , errReasonPhrase = "HTTP Version not supported"
                    , errBody = ""
                    , errHeaders = []
                    }
