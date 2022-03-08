{-# LANGUAGE OverloadedStrings #-}

module Servant.Client.Core.ServerSentEventsSpec (spec) where

import           Control.Monad.Trans.Except
                 (runExceptT)
import qualified Data.ByteString.Lazy                 as ByteString
import           Data.Foldable
                 (for_)
import           Data.Int
                 (Int64)
import           Servant.API.ContentTypes
                 (EventStreamChunk (..))
import           Servant.API.Stream
                 (FromSourceIO (fromSourceIO))
import           Servant.Client.Core.ServerSentEvents
                 (Event (..), EventIgnoreReason (EventComment),
                 EventMessage (..), unEventMessageStreamT, unEventStreamT)
import           Servant.Types.SourceT
                 (runSourceT, source)
import           Test.Hspec
                 (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Servant.Client.Core.ServerSentEvent" $ do
    describe "EventMessageStreamT" $ do
        it "processes chunks correctly" $ do
            let allMessages = ByteString.intercalate "\n"
                    [ "retry: 30"
                    , "data: Hello World"
                    , "id: 1"
                    , ""
                    , "event: my_event"
                    , "data"
                    , "id: 2"
                    , ":Just a comment"
                    , ""
                    , "data: Bye"
                    ]

            for_ [1, 10, 100] $ \chunkSize -> do
                result <-
                    runExceptT
                    $ runSourceT
                    $ unEventMessageStreamT
                    $ fromSourceIO
                    $ source
                    $ map EventStreamChunk
                    $ chunkify chunkSize allMessages

                result `shouldBe` Right
                    [ EventRetry 30
                    , EventData "Hello World"
                    , EventSetLastId "1"
                    , EventDispatch
                    , EventSetName "my_event"
                    , EventData ""
                    , EventSetLastId "2"
                    , EventIgnore (EventComment "Just a comment")
                    , EventDispatch
                    , EventData "Bye"
                    , EventDispatch
                    ]

    describe "EventStreamT" $ do
        it "processes chunks correctly" $ do
            let allMessages = ByteString.intercalate "\n"
                    [ "retry: 30"
                    , "data: Hello World"
                    , "id: 1"
                    , ""
                    , "event: my_event"
                    , "data"
                    , "id: 2"
                    , ":Just a comment"
                    , ""
                    , "data: Bye"
                    ]

            for_ [1, 10, 100] $ \chunkSize -> do
                result <-
                    runExceptT
                    $ runSourceT
                    $ unEventStreamT
                    $ fromSourceIO
                    $ source
                    $ map EventStreamChunk
                    $ chunkify chunkSize allMessages

                result `shouldBe` Right
                    [ Event Nothing "Hello World"
                    , Event (Just "my_event") ""
                    , Event Nothing "Bye"
                    ]

chunkify :: Int64 -> ByteString.ByteString -> [ByteString.ByteString]
chunkify chunkSize input =
    if ByteString.null input then
        []
    else
        let (h, t) = ByteString.splitAt chunkSize input
        in  h : chunkify chunkSize t
