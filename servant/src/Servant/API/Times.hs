{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.API.Times
    ( FTime(..)
    , toFormatProxy
    , getFormat
    , renderTime
    , parseTime
    , ISO8601Date
    , ISO8601DateTime
    , ISO8601DateTimeZ
    ) where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  -- (Symbol)
import           Web.HttpApiData
import qualified Data.Time.Format as T
#if !MIN_VERSION_time(1,5,0)
import           System.Locale as T
#endif
import           Data.Text (pack, Text)
import           Data.Proxy
import           Control.Monad ((>=>))
import           Control.Arrow (first)


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
    showsPrec i t = showParen (i > 1) (\str -> renderTime t ++ str)

instance (KnownSymbol format, T.ParseTime t) => Read (FTime format t) where
    readsPrec i str = res
        where
            fmt = symbolVal (Proxy :: Proxy format)
            res = fmap (first FTime)
                       (readParen (i > 1)
#if !MIN_VERSION_time(1,5,0)
                                  (T.readsTime T.defaultTimeLocale fmt)
#else
                                  (T.readSTime False T.defaultTimeLocale fmt)
#endif
                                  str
                        )


instance (KnownSymbol format, T.FormatTime t) => ToHttpApiData (FTime format t) where
    toUrlPiece   = toUrlPiece   . renderTime
    toHeader     = toHeader     . renderTime
    toQueryParam = toQueryParam . renderTime


instance (KnownSymbol format, T.ParseTime t) => FromHttpApiData (FTime format t) where
    parseUrlPiece   = parseUrlPiece   >=> parseTime
    parseHeader     = parseHeader     >=> parseTime
    parseQueryParam = parseQueryParam >=> parseTime


toFormatProxy :: FTime format t -> Proxy format
toFormatProxy _ = Proxy

getFormat :: KnownSymbol format => FTime format t -> String
getFormat t = symbolVal (toFormatProxy t)

renderTime :: (KnownSymbol format, T.FormatTime t) => FTime format t -> String
renderTime tt@(FTime t) = T.formatTime T.defaultTimeLocale (getFormat tt) t

parseTime :: (KnownSymbol format, T.ParseTime t) => String -> Either Text (FTime format t)
parseTime str = res
    where
#if !MIN_VERSION_time(1,5,0)
        res = case T.parseTime T.defaultTimeLocale fmt str of
#else
        res = case T.parseTimeM False T.defaultTimeLocale fmt str of
#endif
            Nothing -> Left . pack $ "Could not parse time string \"" ++ str ++ "\" with format \"" ++ fmt ++ "\""
            Just t -> Right (FTime t)

        fmt = getFormat (toFTimeTy res)

        toFTimeTy :: Either Text (FTime format t) -> FTime format a
        toFTimeTy _ = undefined

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> import Data.Time.Calendar
-- >>> data Event
-- >>> instance ToJSON Event where { toJSON = undefined }
