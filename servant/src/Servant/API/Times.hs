{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.API.Times
    ( FTime(..)
    , toFormatProxy
    , getFormat
    , renderFTime
    , parseFTime
    , ISO8601Date
    , ISO8601DateTime
    , ISO8601DateTimeZ
    ) where

import           Data.Typeable    (Typeable)
import           GHC.TypeLits     (Symbol,KnownSymbol,symbolVal)
import           Web.HttpApiData
import qualified Data.Time.Format as T
#if !MIN_VERSION_time(1,5,0)
import qualified System.Locale    as T
#endif
import           Data.Text        (pack, Text)
import           Data.Proxy
import           Control.Monad    ((>=>))
import           Control.Arrow    (first)


type ISO8601Date      = "%Y-%m-%d"
type ISO8601DateTime  = "%Y-%m-%dT%H:%M:%S"
type ISO8601DateTimeZ = "%Y-%m-%dT%H:%M:%S%z"

-- | A wrapper around a time type which can be parsed/rendered to with `format',
-- as specified in 'Data.Time.Format'.
--
-- Example:
--
-- >>>            -- GET /events/:date
-- >>> type MyApi = "events" :> Capture "date" (FTime "%Y-%m-%d" Day) :> Get '[JSON] [Event]
--
-- __Note:__ Time Zones parsed in the @%z@ format (@±HHMM@) need to ensure that the @+@ symbol is
-- url encoded (@%2B@) in requests, as the @+@ symbol is interpreted as a space in query params.
newtype FTime (format :: Symbol) t = FTime {getFTime :: t}
    deriving (Typeable, Eq, Ord)

instance (KnownSymbol format, T.FormatTime t) => Show (FTime format t) where
  showsPrec i t = showParen (i > 1) (renderFTime t ++)

instance (KnownSymbol format, T.ParseTime t) => Read (FTime format t) where
  readsPrec i str = res where
    fmt = symbolVal (Proxy :: Proxy format)
    res = fmap (first FTime)
               (readParen (i > 1)
                          (rtime T.defaultTimeLocale fmt)
                          str)


instance (KnownSymbol format, T.FormatTime t) => ToHttpApiData (FTime format t) where
  toUrlPiece   = toUrlPiece   . renderFTime
  toHeader     = toHeader     . renderFTime
  toQueryParam = toQueryParam . renderFTime


instance (KnownSymbol format, T.ParseTime t) => FromHttpApiData (FTime format t) where
  parseUrlPiece   = parseUrlPiece   >=> parseFTime
  parseHeader     = parseHeader     >=> parseFTime
  parseQueryParam = parseQueryParam >=> parseFTime


toFormatProxy :: FTime format t -> Proxy format
toFormatProxy _ = Proxy

getFormat :: KnownSymbol format => FTime format t -> String
getFormat t = symbolVal (toFormatProxy t)

renderFTime :: (KnownSymbol format, T.FormatTime t) => FTime format t -> String
renderFTime tt@(FTime t) = T.formatTime T.defaultTimeLocale (getFormat tt) t

parseFTime :: (KnownSymbol format, T.ParseTime t) => String -> Either Text (FTime format t)
parseFTime str = res where
  res = case ptime T.defaultTimeLocale fmt str of
      Nothing -> Left . pack $ "Could not parse time string \""
                             ++ str ++ "\" with format \"" ++ fmt ++ "\""
      Just t -> Right (FTime t)

        fmt = getFormat (toFTimeTy res)

        toFTimeTy :: Either Text (FTime format t) -> FTime format a
        toFTimeTy _ = undefined


ptime :: T.ParseTime t => T.TimeLocale -> String -> String -> Maybe t
rtime :: T.ParseTime t => T.TimeLocale -> String -> ReadS t
#if !MIN_VERSION_time(1,5,0)
ptime = T.parseTime
rtime = T.readsTime
#else
ptime = T.parseTimeM False
rtime = T.readSTime False
#endif

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> import Data.Time.Calendar
-- >>> data Event
-- >>> instance ToJSON Event where { toJSON = undefined }
