
import           Control.Arrow
import           Data.Foldable
import           Data.List
import           System.Environment

main = do
  files <- getArgs
  forM_ files $ \ file -> do
    convertM file

convertM :: FilePath -> IO ()
convertM file = do
  contents <- readFile file
  seq (length contents) (return ())
  writeFile file (convert contents)

convert :: String -> String
convert =
  lines >>>
  groupBy (\ a b -> take 1 a == take 1 b) >>>
  map go >>>
  concat >>>
  unlines
  where
    go :: [String] -> [String]
    go (a : r)
      | ">" `isPrefixOf` a
        = "``` haskell" : map (drop 2) (a : r) ++ "```" : []
    go x = x
