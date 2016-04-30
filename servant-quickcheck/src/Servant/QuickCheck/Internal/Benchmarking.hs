-- This is a WIP module that shouldn't be used.
module Servant.QuickCheck.Internal.Benchmarking where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Client
import Network.HTTP.Types
import Servant.Client

data BenchOptions = BenchOptions
  { duration    :: Int
  , threads     :: Int
  , connections :: Int
  , noOfTests   :: Int
  } deriving (Eq, Show, Read)

defaultBenchOptions :: BenchOptions
defaultBenchOptions = BenchOptions
  { duration    = 10
  , threads     = 1
  , connections = 10
  , noOfTests   = 10
  }

data WrkScript = WrkScript
  { wrkScheme  :: Scheme
  , wrkHost    :: ByteString
  , wrkPort    :: Int
  , wrkMethod  :: Method
  , wrkPath    :: ByteString
  , wrkHeaders :: [Header]
  , wrkBody    :: ByteString
  } deriving (Eq, Show)

mkScript :: WrkScript -> String
mkScript w
  =  "wrk.scheme = \"" ++ sscheme (wrkScheme w) ++ "\""
  ++ "\nwrk.host = " ++ show (wrkHost w)
  ++ "\nwrk.port = " ++ show (wrkPort w)
  ++ "\nwrk.method = " ++ show (wrkMethod w)
  ++ "\nwrk.path = " ++ show (wrkPath w)
  ++ foldr (\(h,v) old -> old ++ "\nwrk.headers[" ++ show h ++ "] = " ++ show v)
           ""
           (wrkHeaders w)
  ++ "\nwrk.body = " ++ show (wrkBody w)
  ++ "\n" ++ reportFmt
  where
    sscheme Http  = "http"
    sscheme Https = "https"

reqToWrk :: Request -> WrkScript
reqToWrk r = WrkScript
  { wrkScheme  = Http
  , wrkHost    = host r
  , wrkPort    = port r
  , wrkMethod  = method r
  , wrkPath    = path r
  , wrkHeaders = requestHeaders r
  , wrkBody    = case requestBody r of
     RequestBodyLBS r' -> toStrict r'
     _ -> error "expecting RequestBodyLBS"
  }

reportFmt :: String
reportFmt
  =  "done = function(summary, latency, requests)\n"
  ++ "  for _, p in pairs({ 50, 75, 99, 99.999 }) do\n"
  ++ "     n  = latency:percentile(p)\n"
  ++ "     io.write(string.format(\"%g%%, %d\\n\", p, n))\n"
  ++ "  end\n"
  ++ "end\n"

{-data BenchResult = BenchResult-}
  {-{ benchReq :: Request-}
  {-, benchLatencyDist :: [(Percentile, Microsecs)]-}
  {-, benchLatencyAvg :: Microsecs-}
  {-} deriving (Eq, Show, Read, Generic)-}

{-newtype Microsecs = Microsecs { unMicroSecs :: Int }-}
  {-deriving (Eq, Show, Read, Generic)-}

{-newtype Percentile = Percentile { unPercentile :: Int }-}
  {-deriving (Eq, Show, Read, Generic)-}
