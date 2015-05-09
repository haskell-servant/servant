import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import qualified GS1
import qualified GS2
import qualified GS3
import qualified GS4
import qualified GS5
import qualified GS6
import qualified GS7
import qualified GS9
import qualified GS10

app :: String -> (Application -> IO ()) -> IO ()
app n f = case n of
  "1" -> f GS1.app
  "2" -> f GS2.app
  "3" -> f GS3.app
  "4" -> f GS4.app
  "5" -> f GS5.app
  "6" -> f GS6.app
  "7" -> f GS7.app
  "8" -> f GS3.app
  "9" -> GS9.writeJSFiles >> f GS9.app
  "10" -> f GS10.app
  _   -> usage

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> app n (run 8081)
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:\t getting-started N"
  putStrLn "\t\twhere N is the number of the example you want to run."
