-- | Regression coverage for issue #1147's shared worker-thread
--   lifecycle ('Engine.Core.Thread.startWorkerThread' \/
--   'Engine.Core.Thread.workerLoop').
--
--   Six workers used to hand-implement this shape, so the invariants
--   below — one guarded tick per iteration with the recursion OUTSIDE
--   the catch, a paused branch that runs no tick at all, a stop branch
--   that signals completion exactly once, and a startup failure that
--   propagates as a typed 'EngineException' rather than a bare 'error'
--   — had to stay correct in six places independently. They are now
--   stated once, so they are checked once, here.
--
--   #2165 added the shutdown half: 'Engine.Core.Thread.shutdownThread'
--   is a join on every path — a tick that outlives the graceful timeout
--   is force-killed and STILL waited for, an asynchronous exception is
--   a forced termination rather than a crash, a worker that survives
--   the kill is reported fatally on both channels, and a stop request
--   that was merely written is never mistaken for a confirmed exit.
--   Those cases drive 'Engine.Core.Thread.shutdownThreadWith' with
--   millisecond bounds; the production entry point keeps its 10 s.
--
--   Nothing in this module boots an engine: the shared definition
--   deliberately does not mention 'Engine.Core.State.EngineEnv', which
--   is exactly what lets a throwing startup action be injected.
--
--   Every @await@ below is a TOLERANCE, not a timing assertion. The
--   cases assert what did or did not happen — which callbacks ran, how
--   many ticks were observed, what the loop state threaded to — never
--   how long anything took.
module Test.Headless.Core.WorkerLifecycle (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    (newEmptyMVar, putMVar, readMVar, takeMVar, tryReadMVar, tryTakeMVar)
import Control.Exception
    ( AsyncException(..), ErrorCall, SomeException, fromException
    , throwIO, throwTo, try, uninterruptibleMask_ )
import Data.Void (Void)
import Data.IORef
    (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.Timeout (timeout)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Core.Error.Exception
    (EngineException(..), ExceptionType(..), SystemError(..))
import Engine.Core.Log
    ( LogBackend(..), LogCategory(..), LogConfig(..), LogEntry(..)
    , LogLevel(..), LoggerState(..), defaultLogConfig, initLogger )
import Engine.Core.Thread

-- | A logger whose entries land in an 'IORef' instead of a handle.
--
--   The three post-'initLogger' writes neutralise @ENGINE_LOG_LEVEL@ /
--   @ENGINE_DEBUG@, so the captured set is identical on every machine.
captureLogger ∷ IO (IORef [LogEntry], IORef LoggerState)
captureLogger = do
    capturedRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback $ \e →
            atomicModifyIORef' capturedRef (\es → (e : es, ()))
        }
    writeIORef (lsMinLevel logger) LevelDebug
    writeIORef (lsEnabled logger) True
    writeIORef (lsCategoryLevels logger) Map.empty
    loggerRef ← newIORef logger
    pure (capturedRef, loggerRef)

capturedEntries ∷ IORef [LogEntry] → IO [LogEntry]
capturedEntries capturedRef = reverse ⊚ readIORef capturedRef

-- | A worker whose only distinguishing marks are the log lines, so a
--   case can assert on them. Callbacks default to inert.
probeSpec ∷ IORef LoggerState
          → (IORef ThreadControl → IO (Either ε σ))
          → (σ → IO (Maybe σ))
          → WorkerSpec ε σ
probeSpec loggerRef startup tick = WorkerSpec
    { wsName        = "Probe"
    , wsLoggerRef   = loggerRef
    , wsCategory    = CatThread
    , wsStartingMsg = "Starting probe worker..."
    , wsStartedMsg  = Just "Probe worker started"
    , wsFailMsg     = "Failed starting probe worker: "
    , wsFailLevel   = WorkerFailError
    , wsFailFatal   = "Probe worker start failure."
    , wsStartup     = startup
    , wsTick        = tick
    , wsOnStop      = \_ → pure ()
    , wsOnCrash     = \_ _ → pure ()
    }

-- | Poll to a generous ceiling (~10 s). Returns the condition's final
--   value so a case can fail with its own message.
awaitTrue ∷ IO Bool → IO Bool
awaitTrue check = go (2000 ∷ Int)
  where
    go n = do
        ok ← check
        if ok ∨ n ≤ 0 then pure ok else threadDelay 5000 ⌦ \_ → go (n - 1)

-- | Long enough for an in-flight tick to finish and for several paused
--   polls to elapse.
settleQuiet ∷ IO ()
settleQuiet = threadDelay (5 * pausedPollMicros)

bumpAndRecord ∷ IORef [Int] → Int → IO (Maybe Int)
bumpAndRecord seenRef n = do
    atomicModifyIORef' seenRef (\ns → (n : ns, ()))
    pure (Just (n + 1))

-- | Shutdown bounds short enough for a case to reach the forced path
--   in milliseconds: the graceful wait expires almost at once, while
--   the post-kill wait is generous, because a killed tick unwinding is
--   the thing under test and must not be raced.
quickTimeouts ∷ ShutdownTimeouts
quickTimeouts = ShutdownTimeouts
    { stGracefulMicros = 50000
    , stForcedMicros   = 5 * 1000000
    }

-- | The lines at warn level or above, which is where every line the
--   shared lifecycle writes about a forced termination lands; the info
--   and debug lines beneath them are the startup lines every case has.
reportedEntries ∷ IORef [LogEntry] → IO [(LogLevel, Text)]
reportedEntries capturedRef = do
    entries ← capturedEntries capturedRef
    pure [ (leLevel e, leMessage e)
         | e ← entries, leLevel e ≡ LevelWarn ∨ leLevel e ≡ LevelError ]

-- | A stop callback that counts its completion only after a pause, so
--   a shutdown that returned BEFORE the join — the #2165 defect — would
--   read the count as still 0.
countAfterPause ∷ IORef Int → σ → IO ()
countAfterPause ref _ = do
    threadDelay 20000
    atomicModifyIORef' ref (\c → (c + 1, ()))

spec ∷ Spec
spec = describe "Engine.Core.Thread shared worker lifecycle (#1147)" $ do

  describe "startup failure" $ do
    it "propagates a typed EngineException, not a bare error call" $ do
      (capturedRef, loggerRef) ← captureLogger
      ticksRef ← newIORef (0 ∷ Int)
      let worker = probeSpec loggerRef
            (\_ → throwIO (userError "probe startup exploded")
                    ∷ IO (Either Void ()))
            (\() → atomicModifyIORef' ticksRef (\n → (n + 1, Just ())))
      outcome ← try (startWorkerThread worker)
      case outcome of
        Right _ → expectationFailure
          "a throwing startup action still produced a ThreadState"
        Left (e ∷ SomeException) → do
          -- The whole point of requirement 5: an 'ErrorCall' from
          -- 'error' is what this used to be, and must no longer be.
          (fromException e ∷ Maybe ErrorCall) `shouldSatisfy` isNothing
          case fromException e ∷ Maybe EngineException of
            Nothing → expectationFailure $
              "startup failure was not an EngineException: " ⧺ show e
            Just ee → do
              errorMsg ee `shouldBe` "Probe worker start failure."
              case errorType ee of
                ExSystem (IOError detail) →
                  detail `shouldSatisfy` T.isInfixOf "probe startup exploded"
                other → expectationFailure $
                  "expected ExSystem (IOError ...), got " ⧺ show other
      -- No loop was forked, so nothing ticked.
      settleQuiet
      readIORef ticksRef `shouldReturn` 0
      -- The failure was reported BEFORE the throw, at the worker's own
      -- level, and no post-fork "started" line was emitted.
      entries ← capturedEntries capturedRef
      map (\e → (leLevel e, leMessage e)) entries `shouldBe`
        [ (LevelInfo,  "Starting probe worker...")
        , (LevelError, "Failed starting probe worker: user error (probe startup exploded)")
        ]

    it "logs the failure at the level the worker chose (the Lua thread warns)" $ do
      (capturedRef, loggerRef) ← captureLogger
      let worker = (probeSpec loggerRef
                      (\_ → throwIO (userError "quiet failure")
                              ∷ IO (Either Void ()))
                      (\() → pure (Just ())))
            { wsFailLevel = WorkerFailWarn }
      outcome ← try (startWorkerThread worker)
                  ∷ IO (Either EngineException ThreadState)
      case outcome of
        Left ee → errorMsg ee `shouldBe` "Probe worker start failure."
        Right _ → expectationFailure
          "a throwing startup action still produced a ThreadState"
      entries ← capturedEntries capturedRef
      map leLevel entries `shouldBe` [LevelInfo, LevelWarn]

  describe "startup refusal" $
    it "forks nothing and hands the refusal back without an exception" $ do
      (capturedRef, loggerRef) ← captureLogger
      ticksRef ← newIORef (0 ∷ Int)
      let worker = probeSpec loggerRef
            (\_ → pure (Left ("no listener" ∷ Text)))
            (\() → atomicModifyIORef' ticksRef (\n → (n + 1, Just ())))
      outcome ← startWorkerThreadEither worker
      case outcome of
        Right _ → expectationFailure "a refused startup produced a ThreadState"
        Left refusal → refusal `shouldBe` "no listener"
      settleQuiet
      readIORef ticksRef `shouldReturn` 0
      -- Only the "starting" line: nothing was forked, so nothing started.
      entries ← capturedEntries capturedRef
      map leMessage entries `shouldBe` ["Starting probe worker..."]

  describe "running ticks" $ do
    it "threads the startup's value and each tick's state into the next tick" $ do
      (capturedRef, loggerRef) ← captureLogger
      seenRef ← newIORef []
      ts ← startWorkerThread $ probeSpec loggerRef
             (\_ → noRefusal (pure (7 ∷ Int)))
             (bumpAndRecord seenRef)
      reached ← awaitTrue ((≥ 4) ∘ length ⊚ readIORef seenRef)
      reached `shouldBe` True
      shutdownThread ts
      seen ← reverse ⊚ readIORef seenRef
      take 4 seen `shouldBe` [7, 8, 9, 10]
      -- Both info lines, in order, for a worker that declares one.
      entries ← capturedEntries capturedRef
      take 2 (map leMessage entries) `shouldBe`
        ["Starting probe worker...", "Probe worker started"]

    it "omits the post-fork line for a worker that declares none" $ do
      (capturedRef, loggerRef) ← captureLogger
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure ()))
                        (\() → pure (Just ())))
               { wsStartedMsg = Nothing }
      shutdownThread ts
      entries ← capturedEntries capturedRef
      map leMessage entries `shouldBe` ["Starting probe worker..."]

    it "ends the loop when a tick returns Nothing" $ do
      (_, loggerRef) ← captureLogger
      ticksRef ← newIORef (0 ∷ Int)
      stopsRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure (0 ∷ Int)))
                (\n → do
                   writeIORef ticksRef (n + 1)
                   pure (if n ≥ 2 then Nothing else Just (n + 1))))
               { wsOnStop = \_ → atomicModifyIORef' stopsRef
                                   (\c → (c + 1, ())) }
      takeMVar (tsDone ts)
      readIORef ticksRef `shouldReturn` 3
      -- A self-ended loop is not a STOP: the stop callback belongs to
      -- the ThreadStopped branch alone.
      readIORef stopsRef `shouldReturn` 0

  describe "paused polling" $
    it "runs no tick while paused and resumes when set back to running" $ do
      (_, loggerRef) ← captureLogger
      ticksRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $ probeSpec loggerRef
             (\_ → noRefusal (pure ()))
             (\() → atomicModifyIORef' ticksRef (\n → (n + 1, Just ())))
      started ← awaitTrue ((> 0) ⊚ readIORef ticksRef)
      started `shouldBe` True

      writeIORef (tsRunning ts) ThreadPaused
      -- One settle window absorbs the tick that was already in flight.
      settleQuiet
      pausedAt ← readIORef ticksRef
      -- Several further poll intervals must add nothing.
      settleQuiet
      readIORef ticksRef `shouldReturn` pausedAt

      writeIORef (tsRunning ts) ThreadRunning
      resumed ← awaitTrue ((> pausedAt) ⊚ readIORef ticksRef)
      resumed `shouldBe` True
      shutdownThread ts

  describe "stop cleanup and completion signalling" $
    it "runs the stop callback once, fills the done MVar, and ticks no more" $ do
      (_, loggerRef) ← captureLogger
      ticksRef ← newIORef (0 ∷ Int)
      stopsRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure ()))
                (\() → atomicModifyIORef' ticksRef (\n → (n + 1, Just ()))))
               { wsOnStop = \() → atomicModifyIORef' stopsRef
                                    (\c → (c + 1, ())) }
      started ← awaitTrue ((> 0) ⊚ readIORef ticksRef)
      started `shouldBe` True

      writeIORef (tsRunning ts) ThreadStopped
      -- The done MVar is the completion signal itself: taking it proves
      -- the loop actually exited, not merely that a stop was requested.
      takeMVar (tsDone ts)
      readIORef stopsRef `shouldReturn` 1
      stoppedAt ← readIORef ticksRef
      settleQuiet
      readIORef ticksRef `shouldReturn` stoppedAt

  describe "tick exceptions" $ do
    it "ends the loop on the first throw, crashing once and never stopping" $ do
      (_, loggerRef) ← captureLogger
      ticksRef ← newIORef (0 ∷ Int)
      crashesRef ← newIORef ([] ∷ [Text])
      stopsRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure (0 ∷ Int)))
                (\n → do
                   writeIORef ticksRef (n + 1)
                   when (n ≥ 2) $ throwIO (userError "tick exploded")
                   pure (Just (n + 1))))
               { wsOnStop  = \_ → atomicModifyIORef' stopsRef
                                    (\c → (c + 1, ()))
               , wsOnCrash = \_ e → atomicModifyIORef' crashesRef
                                    (\es → (T.pack (show e) : es, ()))
               }
      takeMVar (tsDone ts)
      readIORef ticksRef `shouldReturn` 3
      crashes ← readIORef crashesRef
      case crashes of
        [reported] → reported `shouldSatisfy` T.isInfixOf "tick exploded"
        other → expectationFailure $
          "expected exactly one crash callback, got " ⧺ show other
      -- Fail-stop: a crashed loop never reaches the stop branch.
      readIORef stopsRef `shouldReturn` 0
      -- Nothing kept running after the crash.
      settleQuiet
      readIORef ticksRef `shouldReturn` 3
      tryTakeMVar (tsDone ts) `shouldReturn` Nothing

    it "runs a large tick count to completion without exhausting the stack" $ do
      -- The recursive call lives OUTSIDE the per-tick catch. Were it
      -- inside, every iteration would push a catch frame that never
      -- pops. Frame counts are not observable, so this asserts the
      -- consequence: a bounded-but-large run simply completes.
      (_, loggerRef) ← captureLogger
      let iterations = 200000 ∷ Int
      finalRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $ probeSpec loggerRef
             (\_ → noRefusal (pure (0 ∷ Int)))
             (\n → do
                writeIORef finalRef (n + 1)
                pure (if n + 1 ≥ iterations then Nothing else Just (n + 1)))
      takeMVar (tsDone ts)
      readIORef finalRef `shouldReturn` iterations

  describe "forced termination (#2165)" $ do
    it "kills a tick that outlives the graceful timeout and returns only after the join" $ do
      (capturedRef, loggerRef) ← captureLogger
      entered ← newEmptyMVar
      gate ← newEmptyMVar
      stopsRef ← newIORef (0 ∷ Int)
      crashesRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure ()))
                -- Blocks until released, which this case never does:
                -- the stop request can only be honoured by a kill.
                (\() → putMVar entered () ≫ takeMVar gate ≫ pure (Just ())))
               { wsOnStop  = countAfterPause stopsRef
               , wsOnCrash = \_ _ → atomicModifyIORef' crashesRef
                                      (\c → (c + 1, ()))
               }
      -- Only once the tick is blocked: a stop request written before
      -- the loop's first control read is honoured cooperatively, and
      -- nothing below would be exercised.
      takeMVar entered
      shutdownThreadWith quickTimeouts ts
      -- Returned only once the loop had exited: the completion signal
      -- is already filled, and the join left it filled.
      tryReadMVar (tsDone ts) `shouldReturn` Just ()
      -- The cleanup callback ran to completion exactly once, and the
      -- kill was never mistaken for a crash.
      readIORef stopsRef `shouldReturn` 1
      readIORef crashesRef `shouldReturn` 0
      -- Both sides of the kill name the worker: the escalation, then
      -- the worker's own account of what ended it.
      reportedEntries capturedRef `shouldReturn`
        [ (LevelWarn, "Probe thread did not stop within 50 ms; forcing termination")
        , (LevelWarn, "Probe thread forcibly terminated: thread killed")
        ]

    it "treats a non-ThreadKilled asynchronous exception as a forced termination, not a crash" $ do
      (capturedRef, loggerRef) ← captureLogger
      entered ← newEmptyMVar
      gate ← newEmptyMVar
      stopsRef ← newIORef (0 ∷ Int)
      crashesRef ← newIORef ([] ∷ [Text])
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure ()))
                (\() → putMVar entered () ≫ takeMVar gate ≫ pure (Just ())))
               { wsOnStop  = countAfterPause stopsRef
               , wsOnCrash = \_ e → atomicModifyIORef' crashesRef
                                      (\es → (T.pack (show e) : es, ()))
               }
      -- Only once the tick is inside the guarded region: an exception
      -- landing between ticks would unwind past the guard entirely.
      takeMVar entered
      throwTo (tsThreadId ts) UserInterrupt
      -- The production entry point, with its production bounds: the
      -- worker is already terminating, so this joins without a kill.
      shutdownThread ts
      tryReadMVar (tsDone ts) `shouldReturn` Just ()
      readIORef stopsRef `shouldReturn` 1
      readIORef crashesRef `shouldReturn` []
      -- No escalation line — nothing timed out — only the worker's own.
      reportedEntries capturedRef `shouldReturn`
        [ (LevelWarn, "Probe thread forcibly terminated: user interrupt") ]

    it "reports a worker that survives the kill on the log and as an EngineException" $ do
      (capturedRef, loggerRef) ← captureLogger
      entered ← newEmptyMVar
      release ← newEmptyMVar
      stopsRef ← newIORef (0 ∷ Int)
      crashesRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure ()))
                -- Stands in for a worker stuck in a safe foreign call:
                -- under an uninterruptible mask the kill cannot be
                -- delivered until this case releases the tick.
                (\() → do
                   uninterruptibleMask_ (putMVar entered () ≫ takeMVar release)
                   pure (Just ())))
               { wsOnStop  = \() → atomicModifyIORef' stopsRef (\c → (c + 1, ()))
               , wsOnCrash = \_ _ → atomicModifyIORef' crashesRef
                                      (\c → (c + 1, ()))
               }
      takeMVar entered
      outcome ← try $ shutdownThreadWith
                  (ShutdownTimeouts { stGracefulMicros = 50000
                                    , stForcedMicros   = 50000 }) ts
      case outcome of
        Right () → expectationFailure
          "shutdownThreadWith returned although the worker never terminated"
        Left ee → do
          errorMsg ee `shouldBe` "Probe thread failed to terminate."
          case errorType ee of
            ExSystem (TimeoutError detail) → detail `shouldBe`
              "Probe thread failed to terminate within 50 ms after a forced kill"
            other → expectationFailure $
              "expected ExSystem (TimeoutError ...), got " ⧺ show other
      -- Nothing was joined, and the report says so on the log as well.
      tryReadMVar (tsDone ts) `shouldReturn` Nothing
      reportedEntries capturedRef `shouldReturn`
        [ (LevelWarn,  "Probe thread did not stop within 50 ms; forcing termination")
        , (LevelError, "Probe thread failed to terminate within 50 ms after a forced kill")
        ]
      -- Release and join, so the case leaves no thread behind. The
      -- pending kill lands the moment the mask lifts and is handled as
      -- a forced termination; if it has not arrived by then, the loop
      -- reads the stop request instead. Either way the stop callback
      -- runs exactly once and the crash callback never.
      putMVar release ()
      joined ← timeout (5 * 1000000) (readMVar (tsDone ts))
      joined `shouldBe` Just ()
      readIORef stopsRef `shouldReturn` 1
      readIORef crashesRef `shouldReturn` 0

  describe "shutdown join and idempotency (#2165)" $ do
    it "joins a stop request that was written before the call instead of skipping it" $ do
      (_, loggerRef) ← captureLogger
      entered ← newEmptyMVar
      gate ← newEmptyMVar
      stopsRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure ()))
                (\() → putMVar entered () ≫ takeMVar gate ≫ pure (Just ())))
               { wsOnStop = countAfterPause stopsRef }
      -- The caller wrote the request itself, with the loop blocked
      -- inside its tick, so it cannot have honoured it — a shutdown
      -- that read the request back as completion would return with
      -- nothing joined.
      takeMVar entered
      writeIORef (tsRunning ts) ThreadStopped
      shutdownThreadWith quickTimeouts ts
      tryReadMVar (tsDone ts) `shouldReturn` Just ()
      readIORef stopsRef `shouldReturn` 1

    it "returns at once on a second call after a confirmed join, re-running nothing" $ do
      (_, loggerRef) ← captureLogger
      stopsRef ← newIORef (0 ∷ Int)
      ts ← startWorkerThread $
             (probeSpec loggerRef (\_ → noRefusal (pure ()))
                (\() → pure (Just ())))
               { wsOnStop = \() → atomicModifyIORef' stopsRef (\c → (c + 1, ())) }
      shutdownThread ts
      readIORef stopsRef `shouldReturn` 1
      -- Completion stays observable after the join, so the repeat is a
      -- read of that record, not a second wait. The ceiling is a
      -- tolerance: a repeat that waited out even the graceful timeout
      -- would blow through it.
      tryReadMVar (tsDone ts) `shouldReturn` Just ()
      again ← timeout (1 * 1000000) (shutdownThread ts)
      again `shouldBe` Just ()
      readIORef stopsRef `shouldReturn` 1
      -- Filled exactly once across both calls: taking it now empties
      -- it for good.
      takeMVar (tsDone ts)
      tryTakeMVar (tsDone ts) `shouldReturn` Nothing
