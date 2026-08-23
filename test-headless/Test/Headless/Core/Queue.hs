{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | #1612: coverage for the two "Engine.Core.Queue" functions that have
--   blocking or timeout semantics, and which had no test caller at all —
--   'readQueue' (@src\/Engine\/Core\/Queue.hs:19-20@) and
--   'readQueueTimeout' (@:29-36@, the module's only project-owned logic).
--   Both are load-bearing: the Lua thread's main pump waits up to one
--   60 fps frame per iteration on 'readQueueTimeout', and
--   @debug.captureScreenshot@ awaits the render thread's reply on it.
--
--   == What is NOT claimed here
--
--   'readQueueTimeout' exists to close a narrow window that
--   @System.Timeout.timeout micros (readQueue q)@ leaves open: that
--   wrapper's asynchronous exception can land AFTER the STM dequeue has
--   committed but before the wrapped action returns, silently dropping
--   the message. Forcing that window is scheduler-dependent, so nothing
--   below distinguishes the atomic implementation from the wrapper —
--   these examples cover the PUBLIC blocking and timeout behaviour only.
--
--   == Why the RTS status poll
--
--   Two of the examples need a writer to become eligible while a reader
--   is ALREADY blocked. A handshake immediately before the read call
--   would establish invocation order and nothing more: the reader can be
--   descheduled between the signal and the call, which leaves exactly
--   the ambiguity those examples exist to remove. 'awaitBlockedReader'
--   instead asks the RTS whether the thread is parked in STM, the same
--   discipline "Test.Headless.Save.AutosaveGuards" uses for an analogous
--   boundary. Every bound in this module is a HANG guard whose only
--   power is to fail an example; none of them establishes an ordering.
module Test.Headless.Core.Queue (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.Queue
import Control.Concurrent (ThreadId, forkIO, yield)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (concurrently)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (BlockReason(..), ThreadStatus(..), threadStatus)
import System.Timeout (timeout)
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

    -- #1612: this group used to be called "Concurrent Queue Operations",
    -- which hspec prepends to every example name inside it. Neither case
    -- can overlap a read with a write — the first runs on one thread, and
    -- the second releases its reader only once every write has already
    -- committed — so the group and both examples are named for the FIFO
    -- smoke coverage they actually provide. Genuine overlap lives in
    -- "Blocking Reads And Timeouts" below.
    describe "FIFO Ordering" $ do
      it "preserves write order when a batch is written and then read on \
         \one thread" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        -- Write all items first
        writeQueue queue "1"
        writeQueue queue "2"
        writeQueue queue "3"
        -- Then read them all
        results ← replicateM 3 (tryReadQueue queue)
        sequence results `shouldBe` Just ["1", "2", "3"]
    
      it "preserves write order across two threads when the reader is \
         \released only after every write has committed" $ do
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

    describe "Blocking Reads And Timeouts" $ do
      it "readQueueTimeout on an empty queue returns Nothing only after \
         \its delay has elapsed" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        before ← getMonotonicTimeNSec
        result ← readQueueTimeout shortTimeoutUs queue
        after ← getMonotonicTimeNSec
        result `shouldBe` (Nothing ∷ Maybe T.Text)
        -- A LOWER bound, and deliberately nothing else.
        -- 'STM.registerDelay' flips its TVar after at least the
        -- requested interval and never before it, so "did not return
        -- immediately" is a fact about the call rather than about how
        -- the scheduler happened to run. An upper bound would be the
        -- scheduler-sensitive assertion this module avoids, and the
        -- call needs no outer hang guard either: its own delay — the
        -- thing under test — is what bounds it.
        (after - before) `shouldSatisfy`
            (≥ fromIntegral shortTimeoutUs * nsPerUs)

      it "readQueueTimeout returns an already-available value promptly, \
         \without waiting out its delay" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        writeQueue queue "ready"
        -- Orders of magnitude apart rather than a tight measurement: the
        -- delay is sixty watchdogs long, so a run that actually waited it
        -- out trips the guard, while a correct one finishes with margin
        -- far too wide for load to close.
        outcome ← timeout boundaryWatchdogUs
                      (readQueueTimeout unreachableTimeoutUs queue)
        outcome `shouldBe` Just (Just ("ready" ∷ T.Text))

      it "readQueueTimeout already blocked on an empty queue is completed \
         \by a writer that becomes eligible afterwards" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        done ← newEmptyMVar
        -- The delay is unreachable on purpose: once the reader is
        -- observed parked in STM, the write below is the only thing that
        -- can resolve its transaction. A short delay could expire during
        -- the observation loop and fail a correct implementation; here
        -- expiry can only ever FAIL the example, never pass it.
        reader ← forkIO $ readQueueTimeout unreachableTimeoutUs queue
                              ≫= putMVar done
        observation ← awaitBlockedReader reader
        observation `shouldBe` BlockedInSTM
        writeQueue queue "late"
        result ← awaitReaderResult done
        result `shouldBe` Just (Just ("late" ∷ T.Text))
        -- Delivered exactly once: neither dropped nor left behind.
        leftover ← tryReadQueue queue
        leftover `shouldBe` (Nothing ∷ Maybe T.Text)

      it "readQueue blocks on an empty queue and returns the value a \
         \writer supplies afterwards" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        done ← newEmptyMVar
        reader ← forkIO $ readQueue queue ≫= putMVar done
        observation ← awaitBlockedReader reader
        observation `shouldBe` BlockedInSTM
        writeQueue queue "delivered"
        result ← awaitReaderResult done
        result `shouldBe` Just ("delivered" ∷ T.Text)
        leftover ← tryReadQueue queue
        leftover `shouldBe` (Nothing ∷ Maybe T.Text)

-- | What the RTS says became of a forked reader that is supposed to park
--   inside 'STM.atomically'. Every constructor other than 'BlockedInSTM'
--   is a failed observation, so an example that cannot establish the
--   blocked wait fails instead of quietly asserting nothing.
data ReaderObservation
  = BlockedInSTM        -- ^ parked in an STM transaction, as intended
  | RanWithoutBlocking  -- ^ finished before it could ever block
  | DiedUnobserved      -- ^ threw before it could ever block
  | NeverSettled        -- ^ still neither when the watchdog expired
  deriving (Eq, Show)

-- | Watch a forked reader until the RTS says which of those happened.
--
--   'yield' between polls hands the capability to that reader and then
--   re-reads its real status, so no step here depends on how long
--   anything takes; the watchdog only stops a wedged example from
--   hanging the suite.
awaitBlockedReader ∷ ThreadId → IO ReaderObservation
awaitBlockedReader tid =
    fromMaybe NeverSettled ⊚ timeout boundaryWatchdogUs poll
  where
    poll = do
        status ← threadStatus tid
        case status of
          ThreadBlocked BlockedOnSTM → pure BlockedInSTM
          ThreadFinished             → pure RanWithoutBlocking
          ThreadDied                 → pure DiedUnobserved
          _                          → yield ≫ poll

-- | Collect a forked reader's own result, bounded so a wedged example
--   fails instead of hanging the suite. 'Nothing' is always a failure of
--   the example, never a passing outcome.
awaitReaderResult ∷ MVar α → IO (Maybe α)
awaitReaderResult = timeout boundaryWatchdogUs ∘ takeMVar

-- | Generous on purpose. Every bound in this module is a HANG guard
--   whose only power is to fail the example; none of them establishes
--   the ordering under test, so making one tight would buy nothing and
--   could expire under load on a perfectly correct run.
boundaryWatchdogUs ∷ Int
boundaryWatchdogUs = 10 * 1000 * 1000

-- | A 'readQueueTimeout' delay whose expiry can never be what a passing
--   run observed: it is sixty 'boundaryWatchdogUs' long, so any run that
--   really waited it out fails its watchdog first.
unreachableTimeoutUs ∷ Int
unreachableTimeoutUs = 60 * boundaryWatchdogUs

-- | The one delay that is meant to expire — short enough to measure
--   without slowing the suite, and only ever compared as a lower bound.
shortTimeoutUs ∷ Int
shortTimeoutUs = 50 * 1000

nsPerUs ∷ Word64
nsPerUs = 1000
