module Servant.Types.SourceTSpec where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Hspec
import Prelude hiding (readFile)

import Servant.Types.SourceT

spec :: Spec
spec = describe "Servant.Types.SourceT" $ do
  describe "readFile" $ do
    -- Sizes around the chunk size, to check that no bytes are lost or
    -- duplicated at chunk boundaries.
    let sizes = [0, 1, defaultChunkSize - 1, defaultChunkSize, defaultChunkSize + 1, 3 * defaultChunkSize + 17]

    forM_ sizes $ \size ->
      it ("reads a file of " ++ show size ++ " bytes") $ do
        let contents = BS.pack (take size (cycle [0 .. 255]))
        result <- withTempFile contents $ runExceptT . runSourceT . readFile
        case result of
          Left err -> expectationFailure err
          Right chunks -> do
            BS.concat chunks `shouldBe` contents
            chunks `shouldSatisfy` (not . any BS.null)
            chunks `shouldSatisfy` all ((<= defaultChunkSize) . BS.length)

-- | Run an action with the path of a temporary file with the given contents.
withTempFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withTempFile contents =
  bracket create removeFile
  where
    create = do
      dir <- getTemporaryDirectory
      (path, h) <- openBinaryTempFile dir "servant-readfile.bin"
      BS.hPut h contents
      hClose h
      pure path
