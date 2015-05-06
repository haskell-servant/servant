import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import qualified GS1
import qualified GS2
import qualified GS3
import qualified GS4
import qualified GS5

app :: String -> Maybe Application
app n = case n of
  "1" -> Just GS1.app
  "2" -> Just GS2.app
  "3" -> Just GS3.app
  "4" -> Just GS4.app
  "5" -> Just GS5.app
  _   -> Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> maybe usage (run 8081) (app n)
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:\t getting-started N"
  putStrLn "\t\twhere N is the number of the example you want to run."
