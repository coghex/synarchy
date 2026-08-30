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
--   == Telemetry (#1910)
--
--   "Queue Telemetry" below covers the counters and the oldest-message
--   age every 'Queue' now maintains, and "debug.getQueueStats" covers
--   the console query that reports them for the ten long-lived engine
--   queues. The two groups are deliberately separate: the first is the
--   data structure's own contract and needs no engine, while the second
--   is a wiring proof that runs the PRODUCTION Lua registration path
--   against a real headless environment with no world loaded.
module Test.Headless.Core.Queue (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.Queue
import Engine.Core.State (EngineEnv(..), engineQueueInventory)
import qualified Engine.Core.Thread as EThread
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import World.Command.Types (WorldCommand(..))
import World.Types (WorldPageId(..))
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Control.Concurrent (ThreadId, forkIO, yield, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (concurrently)
import Data.IORef (newIORef)
import Data.List (sort)
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

    -- #1910: the telemetry contract. Every example reads its snapshot
    -- through 'statsOf', which asserts @depth == enqueued - dequeued@
    -- before handing it back, so that invariant is checked after each
    -- operation rather than once at the end of one example.
    describe "Queue Telemetry" $ do
      it "tracks depth across every write and every dequeue path" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        fresh ← statsOf queue
        counts fresh `shouldBe` (0, 0, 0, 0)

        forM_ ["a", "b", "c", "d"] (writeQueue queue)
        written ← statsOf queue
        counts written `shouldBe` (4, 4, 0, 4)

        first ← readQueue queue
        first `shouldBe` "a"
        afterRead ← statsOf queue
        counts afterRead `shouldBe` (3, 4, 1, 4)

        second ← tryReadQueue queue
        second `shouldBe` Just "b"
        afterTry ← statsOf queue
        counts afterTry `shouldBe` (2, 4, 2, 4)

        third ← readQueueTimeout unreachableTimeoutUs queue
        third `shouldBe` Just "c"
        afterTimeout ← statsOf queue
        counts afterTimeout `shouldBe` (1, 4, 3, 4)

        rest ← flushQueue queue
        rest `shouldBe` ["d"]
        afterFlush ← statsOf queue
        counts afterFlush `shouldBe` (0, 4, 4, 4)

      it "never lowers the high-water mark once the queue drains" $ do
        queue ← newQueue ∷ IO (Queue Int)
        forM_ [1 .. 5 ∷ Int] (writeQueue queue)
        peak ← statsOf queue
        (qsDepth peak, qsHighWater peak) `shouldBe` (5, 5)

        _ ← flushQueue queue
        drained ← statsOf queue
        (qsDepth drained, qsHighWater drained) `shouldBe` (0, 5)

        -- A shallower later burst leaves the recorded peak alone: the
        -- mark exists to preserve the evidence that a backlog happened.
        forM_ [6, 7 ∷ Int] (writeQueue queue)
        shallow ← statsOf queue
        (qsDepth shallow, qsHighWater shallow) `shouldBe` (2, 5)

      it "reports an oldest age only while something is undrained" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        fresh ← statsOf queue
        qsOldestAgeNs fresh `shouldBe` Nothing

        writeQueue queue "waiting"
        occupied ← statsOf queue
        qsOldestAgeNs occupied `shouldSatisfy` isJust

        _ ← readQueue queue
        drained ← statsOf queue
        -- Absent rather than zero: a just-enqueued element legitimately
        -- reports an age near zero, so a zero sentinel would be
        -- ambiguous exactly where the distinction matters.
        qsOldestAgeNs drained `shouldBe` Nothing

      it "ages the element that is actually at the head, not the \
         \observation" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        writeQueue queue "older"

        -- The same element stays at the head across both snapshots, so
        -- its reported age must have grown by at least the delay
        -- between them. That is what proves the timestamp belongs to
        -- the ELEMENT and is not resampled at observation time. The
        -- bound is guaranteed rather than scheduler-sensitive:
        -- 'threadDelay' waits AT LEAST its argument and the monotonic
        -- clock never runs backwards, so extra load can only widen it.
        held ← statsOf queue ≫= requireOldestAge
        threadDelay ageGapUs
        stillHeld ← statsOf queue ≫= requireOldestAge
        (stillHeld - held) `shouldSatisfy` (≥ ageGapNs)

        -- The delay above also separates the two enqueues, so "newer"
        -- is stamped at least one full gap after "older". Removing the
        -- head must then re-derive the age from "newer"'s own timestamp
        -- rather than carry the departed element's forward.
        writeQueue queue "newer"
        t0 ← getMonotonicTimeNSec
        beforeRemoval ← statsOf queue ≫= requireOldestAge
        removed ← readQueue queue
        promoted ← statsOf queue ≫= requireOldestAge
        t1 ← getMonotonicTimeNSec
        removed `shouldBe` "older"

        -- Still the older element's here, which 'threadDelay' guarantees
        -- is at or past one gap. Checked before the bound below, which
        -- subtracts one gap from it.
        beforeRemoval `shouldSatisfy` (≥ ageGapNs)

        -- The bound the promoted age must respect, derived from clocks
        -- this example read ITSELF rather than from any assumption about
        -- how promptly it got scheduled. Writing @tA@\/@tB@ for the two
        -- enqueue instants and @s@\/@u@ for the two snapshots (both
        -- inside @[t0, t1]@): @beforeRemoval = s - tA@ and
        -- @promoted = u - tB@, and @tB ≥ tA + gap@, so
        -- @promoted ≤ (u - s) + beforeRemoval - gap@ and @u - s@ is at
        -- most @t1 - t0@. A stall anywhere widens the right-hand side by
        -- at least as much as the left, so a correct implementation
        -- cannot fail this however the scheduler behaves — while an
        -- implementation that kept the DEPARTED head's timestamp reports
        -- @u - tA@, which exceeds the bound by a whole gap.
        promoted `shouldSatisfy` (≤ (t1 - t0) + beforeRemoval - ageGapNs)

      it "leaves every counter unchanged when tryReadQueue finds nothing" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        writeQueue queue "only"
        _ ← readQueue queue
        drained ← statsOf queue

        missed ← tryReadQueue queue
        missed `shouldBe` (Nothing ∷ Maybe T.Text)
        afterMiss ← statsOf queue
        afterMiss `shouldBe` drained

      it "counts a timed-out readQueueTimeout as no dequeue at all" $ do
        queue ← newQueue ∷ IO (Queue T.Text)
        writeQueue queue "served"
        served ← readQueueTimeout unreachableTimeoutUs queue
        served `shouldBe` Just "served"
        afterServed ← statsOf queue
        counts afterServed `shouldBe` (0, 1, 1, 1)

        timedOut ← readQueueTimeout shortTimeoutUs queue
        timedOut `shouldBe` (Nothing ∷ Maybe T.Text)
        afterTimeout ← statsOf queue
        -- 'STM.orElse' discards the dequeue branch's writes when it
        -- retries, so the timeout cannot record a dequeue that never
        -- happened.
        afterTimeout `shouldBe` afterServed

      it "returns depth to zero on flushQueue while keeping the \
         \cumulative counts" $ do
        queue ← newQueue ∷ IO (Queue Int)
        forM_ [1 .. 3 ∷ Int] (writeQueue queue)
        flushed ← flushQueue queue
        flushed `shouldBe` [1, 2, 3 ∷ Int]
        afterFlush ← statsOf queue
        afterFlush `shouldBe` QueueStats
            { qsDepth       = 0
            , qsEnqueued    = 3
            , qsDequeued    = 3
            , qsHighWater   = 3
            , qsOldestAgeNs = Nothing
            }

        -- An empty flush is a no-op, not a dequeue of nothing.
        again ← flushQueue queue
        again `shouldBe` ([] ∷ [Int])
        afterEmpty ← statsOf queue
        afterEmpty `shouldBe` afterFlush

    -- #1910: the console query, driven through the PRODUCTION Lua
    -- registration path ('registerLuaAPI') against a real headless
    -- environment with NO world — the state a backlog diagnostic has to
    -- answer in, and the one a wiring mistake would show up in first.
    -- Resource-root isolation wraps the engine because engine init is
    -- itself a config writer (#1357).
    describe "debug.getQueueStats" $
      around (withIsolatedResourceRoot ∘ withHeadlessEngineNoWorld) $ do
        it "names exactly the ten long-lived engine queues, once each" $
          \env → do
            rows ← queueStatsRows env
            map rowName rows `shouldBe` canonicalQueueNames

        it "reports the documented row fields for every queue" $ \env → do
          rows ← queueStatsRows env
          forM_ rows $ \row →
            (rowName row, rowKeys row) `shouldBe`
                (rowName row, expectedRowKeys row)

        it "reports counters satisfying depth == enqueued - dequeued" $
          \env → do
            rows ← queueStatsRows env
            forM_ rows $ \row →
              (rowName row, rowDepth row) `shouldBe`
                  (rowName row, rowEnqueued row - rowDequeued row)

        it "agrees with the inventory the environment itself exposes" $
          \env → do
            rows ← queueStatsRows env
            map nqName (engineQueueInventory env) `shouldBe` map rowName rows

        it "carries oldestAgeSeconds for a queue with something \
           \undrained" $ \env → do
          -- The other examples see ten idle queues, so none of them
          -- exercises the optional field at all. This one parks a
          -- message that nothing can consume — the no-world harness
          -- starts no world thread, and 'WorldHide' of a page that was
          -- never created is inert even if one ever drained it — so the
          -- row is guaranteed to be the non-empty case.
          writeQueue (worldQueue env) (WorldHide (WorldPageId "no-such-page"))
          rows ← queueStatsRows env
          let named n = filter ((≡ n) ∘ rowName) rows
          case named "Engine.Core.State.worldQueue" of
            [row] → do
                rowDepth row `shouldBe` 1
                rowKeys row `shouldBe` expectedRowKeys row
                rowAge row `shouldSatisfy` isJust
                forM_ (rowAge row) (`shouldSatisfy` (≥ 0))
            other → expectationFailure
                ("expected exactly one worldQueue row, got " <> show other)

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

-- * #1910 telemetry helpers

-- | Snapshot a queue, asserting the relation the telemetry contract
--   guarantees at every commit boundary — @depth == enqueued -
--   dequeued@ — before handing the snapshot back. Reading every
--   snapshot through this is what makes the invariant checked after
--   each operation rather than only where an example happens to look.
statsOf ∷ Queue α → IO QueueStats
statsOf q = do
    stats ← queueStats q
    (qsDepth stats, qsEnqueued stats, qsDequeued stats)
        `shouldSatisfy` \(d, e, dq) → fromIntegral d ≡ e - dq
    return stats

-- | Depth, enqueued, dequeued and high water as one comparable tuple,
--   so a failure prints all four rather than the first that differs.
counts ∷ QueueStats → (Int, Word64, Word64, Int)
counts stats =
    (qsDepth stats, qsEnqueued stats, qsDequeued stats, qsHighWater stats)

-- | The reported oldest age, FAILING the example when the queue
--   reported none. An example that silently skipped its assertion
--   because the age was absent would assert nothing at all.
requireOldestAge ∷ QueueStats → IO Word64
requireOldestAge stats = case qsOldestAgeNs stats of
    Just age → return age
    Nothing  → do
        expectationFailure "expected an oldest age, but the queue \
                           \reported none"
        return 0

-- | The delay separating the two enqueues, and the two observations, in
--   the age example. Long enough that the microseconds of STM and
--   scheduling around each snapshot cannot approach it, short enough to
--   be invisible in the suite's runtime.
ageGapUs ∷ Int
ageGapUs = 100 * 1000

ageGapNs ∷ Word64
ageGapNs = fromIntegral ageGapUs * nsPerUs

-- | The ten names 'engineQueueInventory' declares, in its order. Spelled
--   out independently here rather than derived from the inventory: this
--   list is what makes a renamed, dropped or duplicated queue fail
--   instead of quietly changing what the console reports.
canonicalQueueNames ∷ [Text]
canonicalQueueNames =
    [ "Engine.Core.State.inputQueue"
    , "Engine.Core.State.luaToEngineQueue"
    , "Engine.Core.State.luaQueue"
    , "Engine.Core.State.worldQueue"
    , "Engine.Core.State.screenshotRequestQueue"
    , "Engine.Core.State.bloodDisposeQueue"
    , "Engine.Core.State.unitQueue"
    , "Engine.Core.State.buildingQueue"
    , "Engine.Core.State.combatQueue"
    , "Engine.Core.State.simQueue"
    ]

-- | One decoded @debug.getQueueStats()@ row.
data StatsRow = StatsRow
    { rowName     ∷ Text
    , rowDepth    ∷ Integer
    , rowEnqueued ∷ Integer
    , rowDequeued ∷ Integer
    , rowHighWater ∷ Integer
    , rowAge      ∷ Maybe Double
    , rowKeys     ∷ [Text]
      -- ^ Every key the row table actually carries, sorted — so an
      --   EXTRA field fails as loudly as a missing one.
    } deriving (Eq, Show)

-- | The exact key set a row is required to carry: the five mandatory
--   fields, plus @oldestAgeSeconds@ only when the queue was non-empty.
expectedRowKeys ∷ StatsRow → [Text]
expectedRowKeys row = sort $
    ["depth", "dequeued", "enqueued", "highWater", "name"]
        <> ["oldestAgeSeconds" | isJust (rowAge row)]

-- | Run @debug.getQueueStats()@ through the production Lua registration
--   on a bare engine environment and decode its rows.
--
--   The Lua side reports each row as delimited text rather than letting
--   the console serialize the table: that keeps the decode independent
--   of the console's own JSON rendering, and @#rows@ additionally
--   proves the result is a DENSE array.
queueStatsRows ∷ EngineEnv → IO [StatsRow]
queueStatsRows env = do
    ls ← newBareLuaBackend env
    raw ← executeDebugLua (lbsLuaState ls) queueStatsProbe
    case T.stripPrefix "\"" raw ≫= T.stripSuffix "\"" of
      Nothing      → fail ("debug.getQueueStats() did not return a \
                           \string: " <> T.unpack raw)
      Just payload → mapM decodeRow (T.splitOn ";" payload)

-- | A Lua state with the FULL production API registered, which is what
--   makes these examples a wiring proof: an unregistered verb fails
--   here exactly as it would on the console.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef EThread.ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    return ls

queueStatsProbe ∷ Text
queueStatsProbe = T.intercalate "\n"
    [ "local rows = debug.getQueueStats()"
    , "if type(rows) ~= 'table' then return 'not-a-table' end"
    , "local out = {}"
    , "for i = 1, #rows do"
    , "  local r = rows[i]"
    , "  local keys = {}"
    , "  for k in pairs(r) do keys[#keys + 1] = k end"
    , "  table.sort(keys)"
    , "  out[#out + 1] = table.concat({ tostring(r.name),"
    , "    tostring(r.depth), tostring(r.enqueued), tostring(r.dequeued),"
    , "    tostring(r.highWater), tostring(r.oldestAgeSeconds),"
    , "    table.concat(keys, ',') }, '|')"
    , "end"
    , "return table.concat(out, ';')"
    ]

decodeRow ∷ Text → IO StatsRow
decodeRow raw = case T.splitOn "|" raw of
    [name, depth, enq, deq, hw, age, keys] → StatsRow name
        <$> readInteger depth
        <*> readInteger enq
        <*> readInteger deq
        <*> readInteger hw
        <*> readAge age
        <*> pure (T.splitOn "," keys)
    _ → fail ("malformed debug.getQueueStats() row: " <> T.unpack raw)
  where
    readInteger field = case reads (T.unpack field) of
        [(n, "")] → return (n ∷ Integer)
        _         → fail ("row field is not an integer: " <> T.unpack field)
    readAge "nil" = return Nothing
    readAge field = case reads (T.unpack field) of
        [(d, "")] → return (Just (d ∷ Double))
        _         → fail ("oldestAgeSeconds is neither a number nor \
                          \absent: " <> T.unpack field)
