import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment

import qualified T1
import qualified T10
import qualified T2
import qualified T3
import qualified T4
import qualified T5
import qualified T6
import qualified T7
import qualified T9

app :: String -> (Application -> IO ()) -> IO ()
app n f = case n of
  "1" -> f T1.app
  "2" -> f T2.app
  "3" -> f T3.app
  "4" -> f T4.app
  "5" -> f T5.app
  "6" -> f T6.app
  "7" -> f T7.app
  "8" -> f T3.app
  "9" -> T9.writeJSFiles >> f T9.app
  "10" -> f T10.app
  _   -> usage

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> app n (run 8081)
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:\t tutorial N"
  putStrLn "\t\twhere N is the number of the example you want to run."
