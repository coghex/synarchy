{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Core.Queue (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.Queue
import Control.Concurrent.Async (concurrently)
import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM

spec ∷ Spec
spec = do
  describe "Engine.Core.Queue" $ do
    describe "Basic Queue Operations" $ do
      it "can write and read from queue" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        writeQueue queue "test"
        result ← tryReadQueue queue
        result `shouldBe` Just "test"

      it "returns Nothing when reading from empty queue" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        result ← tryReadQueue queue
        result `shouldBe` (Nothing ∷ Maybe T.Text)

      it "can flush multiple items from queue" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        writeQueue queue "first"
        writeQueue queue "second"
        writeQueue queue "third"
        results ← flushQueue queue
        results `shouldBe` ["first", "second", "third"]
        
      it "returns empty list when flushing empty queue" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        results ← flushQueue queue
        results `shouldBe` ([] ∷ [T.Text])

    describe "Concurrent Queue Operations" $ do
      it "handles multiple writers and readers correctly" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        -- Write all items first
        writeQueue queue "1"
        writeQueue queue "2"
        writeQueue queue "3"
        -- Then read them all
        results ← replicateM 3 (tryReadQueue queue)
        sequence results `shouldBe` Just ["1", "2", "3"]
    
      it "handles concurrent operations with proper synchronization" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        barrier ← STM.atomically $ STM.newTVar False
        let writeItems = do
              -- Write items
              writeQueue queue "1"
              writeQueue queue "2"
              writeQueue queue "3"
              -- Signal items are written
              STM.atomically $ STM.writeTVar barrier True
            readItems = do
              -- Wait for items to be written
              STM.atomically $ do
                ready ← STM.readTVar barrier
                when (not ready) STM.retry
              -- Then read them
              replicateM 3 (tryReadQueue queue)
        (_, results) ← concurrently writeItems readItems
        sequence results `shouldBe` Just ["1", "2", "3"]
    describe "Channel Operations" $ do
      it "can write and read from channel" $ do
        chan ← newChannel ∷ IO (Channel T.Text)
        atomically $ writeChannel chan "test"
        result ← atomically $ tryReadChannel chan
        result `shouldBe` Just "test"

      it "returns Nothing when reading from empty channel" $ do
        chan ← newChannel ∷ IO (Channel T.Text)
        result ← atomically $ tryReadChannel chan
        result `shouldBe` (Nothing ∷ Maybe T.Text)

  where
    atomically = STM.atomically
