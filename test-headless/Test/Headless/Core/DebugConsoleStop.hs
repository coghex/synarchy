-- | The debug console's BOUNDED stop (#2689).
--
--   #2170 time-boxed every console join with 'System.Timeout.timeout',
--   and #2165 then ran every worker's cleanup — cooperative stop,
--   forced termination and crash cleanup alike — under an
--   UNINTERRUPTIBLE mask. 'timeout' works by throwing to the waiter,
--   which that mask blocks, so inside the Lua worker's cleanup the
--   bounds never fired: a stalled console thread held the queue drain
--   and @Lua.close@ past both of the worker's own shutdown budgets.
--   The suites that already existed never saw it, because
--   "Test.Headless.Core.DebugSocket" stops its consoles unmasked and
--   "Test.Headless.Core.WorkerLifecycle" records cleanup-step names
--   rather than running a real console.
--
--   Every example here therefore stops a REAL listener, on a real
--   loopback port, with a real stall injected through its own seams
--   ('dscAccept' for the accept thread, 'dscBuiltin' for a client, both
--   of which run on the thread under test), and most of them stop it
--   from inside a REAL worker's cleanup, under the mask 'startWorkerThread'
--   forks with.
--
--   Timing: each stall is ESTABLISHED by synchronisation (the stalled
--   code signals that it has entered) before the stop begins, never by
--   sleeping. The elapsed-time assertions allow 'schedulingTolerance'
--   over 'listenerJoinMicros' for thread scheduling and socket setup on
--   a loaded machine; a regression to the unbounded wait overruns that
--   by the whole remaining stall, which every example here makes
--   indefinite. Every stall is released in a @finally@, and its threads
--   are waited for, so a failing example leaves nothing blocked.
module Test.Headless.Core.DebugConsoleStop (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
    (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception
    ( SomeException, finally, onException, throwIO, try
    , uninterruptibleMask_ )
import Data.IORef
    (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTimeNSec)
import System.Timeout (timeout)
import Network.Socket (SockAddr, Socket, accept, close)
import Network.Socket.ByteString (sendAll)

import Engine.Core.Lifecycle (EngineLifecycle(..))
import Engine.Core.Log
    ( LogBackend(..), LogCategory(..), LogConfig(..), LoggerState
    , defaultLogConfig, initLogger )
import Engine.Core.Thread
import Engine.Scripting.Lua.DebugServer
import Test.Headless.Core.DebugSocket
    ( newDiagnostics, testConfig, withServer, connectTo
    , readUntilContains, Diagnostics(..) )

spec ∷ Spec
spec =
    -- Filed under #2170's group, whose acceptance command selects it,
    -- and named for #2689 beneath it.
    describe "debug-console socket supervision (#2170)" $
      describe "the bounded stop (#2689)" $ do
        workerMaskSpec
        deferredKillSpec
        fullHouseSpec
        promptSpec

-- ---------------------------------------------------------------- --
-- An accept stall, stopped from inside a real worker's cleanup
-- ---------------------------------------------------------------- --

workerMaskSpec ∷ Spec
workerMaskSpec = describe "an accept thread stalled well past the bound" $ do
  forM_ [Cooperative, Forced, Crash] $ \path →
    it ("is stopped within the bound from a worker's " <> pathName path
        <> ", and the cleanup after the stop still runs") $ do
      stall ← newStall
      warnings ← newIORef []
      withStalledServer stall stallAcceptInterruptibly (record warnings) $ \console → do
        awaitEntered stall 1
        outcome ← runThroughWorker path console
        woElapsed outcome `shouldSatisfy` withinBound
        woDone outcome `shouldBe` True
        woSteps outcome `shouldBe` cleanupSteps
        -- An INTERRUPTIBLE stall takes the kill the stop sends once the
        -- close alone has not woken it, so the thread exits and there
        -- is nothing to warn about.
        settle
        readIORef warnings `shouldReturn` []

-- ---------------------------------------------------------------- --
-- A thread that will not take the kill
-- ---------------------------------------------------------------- --

deferredKillSpec ∷ Spec
deferredKillSpec = describe "a console thread that defers the kill" $ do

  it "cannot stretch an unmasked stop, and is warned about once, by \
     \kind, even across a repeated stop" $ do
    stall ← newStall
    warnings ← newIORef []
    withStalledServer stall stallAcceptUninterruptibly (record warnings) $ \console → do
        awaitEntered stall 1
        elapsed ← timedStop (stopDebugConsole console)
        elapsed `shouldSatisfy` withinBound
        -- Idempotent: the second stop does nothing, so it warns about
        -- nothing either.
        stopDebugConsole console
        awaitWarnings warnings 1 `shouldReturn` [ConsoleAcceptThread]
        settle
        readIORef warnings `shouldReturn` [ConsoleAcceptThread]

  it "cannot stretch a stop under the worker's mask, even through a \
     \warning sink that blocks or throws" $ do
    forM_ [BlockingSink, ThrowingSink] $ \sinkKind → do
      stall ← newStall
      calls ← newIORef []
      sinkGate ← newEmptyMVar
      let sink kind = do
              record calls kind
              case sinkKind of
                  BlockingSink → readMVar sinkGate
                  ThrowingSink → throwIO (userError "log handle vanished")
      withStalledServer stall stallAcceptUninterruptibly sink (\console → do
          awaitEntered stall 1
          outcome ← runThroughWorker Cooperative console
          woElapsed outcome `shouldSatisfy` withinBound
          woDone outcome `shouldBe` True
          woSteps outcome `shouldBe` cleanupSteps
          awaitWarnings calls 1 `shouldReturn` [ConsoleAcceptThread])
        `finally` void (tryPutMVar sinkGate ())

-- ---------------------------------------------------------------- --
-- Every admitted client stalled at once
-- ---------------------------------------------------------------- --

fullHouseSpec ∷ Spec
fullHouseSpec = describe "the connection cap's worth of stalled clients" $ do
  it "share one deadline when stopped unmasked" $
    fullHouse Nothing
  it "share one deadline when stopped from a worker's masked cleanup" $
    fullHouse (Just Cooperative)

-- | 'defaultMaxConnections' clients, every one parked inside a
--   built-in that has masked itself uninterruptibly — so neither the
--   socket close nor the kill can move it, and each costs the stop the
--   most a client can. A per-thread allowance would add up to minutes;
--   the shared deadline must not.
fullHouse ∷ Maybe WorkerPath → IO ()
fullHouse mPath = do
    stall ← newStall
    warnings ← newIORef []
    let builtin _ = do
            -- Left INSIDE the mask: the stop's pending kill lands the
            -- moment the mask lifts.
            uninterruptibleMask_ $ do
                enter stall
                readMVar (stallGate stall)
                leave stall
            pure (Just "released")
        tweak cfg = cfg { dscBuiltin = builtin
                        , dscOnStopIncomplete = record warnings }
    withReleasedStall stall $ withQuietServer tweak $ \port console → do
        socks ← newIORef []
        (do forM_ [1 .. defaultMaxConnections] $ \_ → do
                sock ← connectTo port
                atomicModifyIORef' socks (\ss → (sock : ss, ()))
                void (readUntilContains twoSeconds "> " sock)
                sendAll sock "stall\n"
            awaitEntered stall defaultMaxConnections
            (elapsed, steps) ← case mPath of
                Nothing → do
                    e ← timedStop (stopDebugConsole console)
                    pure (e, cleanupSteps)
                Just path → do
                    outcome ← runThroughWorker path console
                    woDone outcome `shouldBe` True
                    pure (woElapsed outcome, woSteps outcome)
            elapsed `shouldSatisfy` withinBound
            steps `shouldBe` cleanupSteps
            ws ← awaitWarnings warnings defaultMaxConnections
            ws `shouldBe` replicate defaultMaxConnections ConsoleClientThread
            settle
            length ⊚ readIORef warnings `shouldReturn` defaultMaxConnections)
          `finally` (readIORef socks ⌦ mapM_ (void ∘ tryAny ∘ close))
    -- 'withReleasedStall' has released the built-ins by now; they must
    -- all come back out, or the stall leaked a thread.
    awaitLeft stall defaultMaxConnections `shouldReturn` True

-- ---------------------------------------------------------------- --
-- The prompt path is unchanged
-- ---------------------------------------------------------------- --

promptSpec ∷ Spec
promptSpec = describe "a stop in which every thread exits promptly" $
  forM_ [False, True] $ \masked →
    it ("keeps its order and warns about nothing"
        <> (if masked then " (masked caller)" else " (unmasked caller)")) $ do
      events ← newIORef []
      warnings ← newIORef []
      diags ← newDiagnostics
      parked ← newStall
      never ← newEmptyMVar ∷ IO (MVar (Maybe Text))
      let acceptRecorded sock = accept sock `onException`
              record events "accept released"
          -- An INTERRUPTIBLE park the socket close cannot reach, so the
          -- client leaves only on the stop's kill — which is what makes
          -- the kill observable, and so its place in the order.
          builtin _ = do
              enter parked
              takeMVar never `onException` record events "client killed"
          tweak cfg = cfg { dscAccept = acceptRecorded
                          , dscBuiltin = builtin
                          , dscOnStopIncomplete = record warnings }
      withServer (testConfig diags tweak) $ \port console → do
          sock ← connectTo port
          (do void (readUntilContains twoSeconds "> " sock)
              sendAll sock "park\n"
              awaitEntered parked 1
              elapsed ← timedStop $
                  if masked then uninterruptibleMask_ (stopDebugConsole console)
                            else stopDebugConsole console
              -- Nothing was waited out: well inside the accept's half of
              -- the bound, never mind the whole of it.
              elapsed `shouldSatisfy` (< listenerJoinMicros `div` 2)
              -- The accept thread is joined BEFORE any client is told to
              -- stop, exactly as before #2689.
              void $ awaitCount events 2
              readIORef events
                  `shouldReturn` ["accept released", "client killed"])
            `finally` close sock
      settle
      readIORef warnings `shouldReturn` []
      -- And an intentional stop is still not a loss.
      readIORef (diagRetries diags) `shouldReturn` []
      readIORef (diagLosses diags) `shouldReturn` []

-- ---------------------------------------------------------------- --
-- Worker fixtures
-- ---------------------------------------------------------------- --

-- | The three cleanup paths #2165 runs under the worker's mask.
data WorkerPath = Cooperative | Forced | Crash
    deriving (Eq, Show)

pathName ∷ WorkerPath → String
pathName Cooperative = "cooperative stop"
pathName Forced      = "forced termination"
pathName Crash       = "crash cleanup"

data WorkerOutcome = WorkerOutcome
    { woElapsed ∷ Int
      -- ^ Microseconds from the stop's trigger to the loop's exit.
    , woDone    ∷ Bool
      -- ^ Whether the worker's done-cell was filled in time.
    , woSteps   ∷ [Text]
      -- ^ The cleanup steps that ran, in order.
    }

-- | The Lua worker's cleanup shape, with the two steps after the
--   console stop recorded by name: the console is stopped for real,
--   and what matters about the rest is that it RUNS, once, afterwards.
cleanupSteps ∷ [Text]
cleanupSteps = ["stopDebugConsole", "drainDebugQueue", "Lua.close"]

-- | Start a real worker owning @console@, trigger @path@, and time how
--   long the loop takes to exit. The cleanup runs under the mask the
--   worker was forked with — which is the whole point.
runThroughWorker ∷ WorkerPath → DebugConsole → IO WorkerOutcome
runThroughWorker path console = do
    loggerRef ← quietLogger
    lifecycle ← newIORef EngineRunning
    stepsRef ← newIORef []
    tickEntered ← newEmptyMVar
    tickGate ← newEmptyMVar ∷ IO (MVar ())
    let cleanup = do
            stopDebugConsole console
            record stepsRef "stopDebugConsole"
            record stepsRef "drainDebugQueue"
            record stepsRef "Lua.close"
        tick () = case path of
            -- A short, returning tick: the stop is picked up at the top
            -- of the next iteration.
            Cooperative → threadDelay 1000 ≫ pure (Just ())
            -- Parks until killed: only a forced termination ends it.
            Forced → do
                void (tryPutMVar tickEntered ())
                takeMVar tickGate ≫ pure (Just ())
            -- Fails synchronously once released.
            Crash → do
                void (tryPutMVar tickEntered ())
                readMVar tickGate
                throwIO (userError "tick exploded")
    ts ← startWorkerThread WorkerSpec
        { wsName           = "ConsoleProbe"
        , wsLoggerRef      = loggerRef
        , wsCategory       = CatLua
        , wsLifecycleRef   = lifecycle
        , wsCrashSink      = \_ → pure ()
        , wsStartingMsg    = "Starting console probe worker..."
        , wsStartedMsg     = Nothing
        , wsFailMsg        = "Console probe worker failed to start: "
        , wsFailLevel      = WorkerFailWarn
        , wsFailFatal      = "Console probe worker failed to start."
        , wsStartup        = \_ → noRefusal (pure ())
        , wsTick           = tick
        , wsOnStop         = \() → cleanup
        , wsOnCrash        = \_ _ → pure ()
        , wsOnCrashCleanup = \_ _ → cleanup
        }
    when (path ≢ Cooperative) $ takeMVar tickEntered
    let awaitExit = isJust ⊚ timeout generousJoin (readMVar (tsDone ts))
    (elapsed, done) ← timedWith $ case path of
        Cooperative → writeIORef (tsRunning ts) ThreadStopped ≫ awaitExit
        Crash       → putMVar tickGate () ≫ awaitExit
        -- The graceful wait expires at once and the kill lands in the
        -- parked tick; the post-kill budget is exactly what a masked
        -- console stop used to overrun.
        Forced → do
            r ← try $ shutdownThreadWith ShutdownTimeouts
                { stGracefulMicros = 1000
                , stForcedMicros   = listenerJoinMicros + schedulingTolerance
                } ts
            case r of
                Left (_ ∷ SomeException) → pure False
                Right ()                 → awaitExit
    -- Whatever happened, the worker must not outlive the example.
    void ∘ timeout generousJoin $ readMVar (tsDone ts)
    steps ← readIORef stepsRef
    pure WorkerOutcome { woElapsed = elapsed, woDone = done, woSteps = steps }

quietLogger ∷ IO (IORef LoggerState)
quietLogger = do
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\_ → pure ()) }
    newIORef logger

-- ---------------------------------------------------------------- --
-- Stalls
-- ---------------------------------------------------------------- --

-- | A stall some number of threads enter and, once its gate is opened,
--   leave. Entering and leaving are counted so an example can wait for
--   the stall to be ESTABLISHED, and for it to be fully released.
data Stall = Stall
    { stallGate    ∷ MVar ()
    , stallEntered ∷ IORef Int
    , stallLeft    ∷ IORef Int
    }

newStall ∷ IO Stall
newStall = Stall ⊚ newEmptyMVar ⊛ newIORef 0 ⊛ newIORef 0

enter, leave ∷ Stall → IO ()
enter s = atomicModifyIORef' (stallEntered s) (\n → (n + 1, ()))
leave s = atomicModifyIORef' (stallLeft s) (\n → (n + 1, ()))

awaitEntered ∷ Stall → Int → IO ()
awaitEntered s wanted = do
    ok ← waitFor fiveSeconds $ (≥ wanted) ⊚ readIORef (stallEntered s)
    unless ok $ expectationFailure $
        show wanted <> " stall(s) wanted; they never all arrived"

awaitLeft ∷ Stall → Int → IO Bool
awaitLeft s wanted = waitFor fiveSeconds $ (≥ wanted) ⊚ readIORef (stallLeft s)

-- | An accept that parks INTERRUPTIBLY until released: the socket close
--   does not wake it (it is not in @accept@ at all), but the stop's
--   kill does.
stallAcceptInterruptibly ∷ Stall → Socket → IO a
stallAcceptInterruptibly s _ = do
    enter s
    readMVar (stallGate s) `finally` leave s
    throwIO (userError "stall released")

-- | An accept that parks with asynchronous exceptions DEFERRED — the
--   thread a synchronous 'killThread' would wait on forever.
stallAcceptUninterruptibly ∷ Stall → Socket → IO a
stallAcceptUninterruptibly s _ = do
    -- Left INSIDE the mask: the stop's pending kill lands the moment
    -- the mask lifts, so nothing after it would ever run.
    uninterruptibleMask_ $ do
        enter s
        readMVar (stallGate s)
        leave s
    throwIO (userError "stall released")

-- | Open the stall's gate however @act@ ends, so nothing it parked is
--   left blocked behind a failed example.
withReleasedStall ∷ Stall → IO a → IO a
withReleasedStall s act = act `finally` void (tryPutMVar (stallGate s) ())

-- | A real server whose accept seam parks in @stall@, with @sink@ as
--   its incomplete-stop sink. Once the example is done, the stall is
--   released and its accept thread must come back out.
withStalledServer ∷ Stall
                  → (Stall → Socket → IO (Socket, SockAddr))
                  → (ConsoleThreadKind → IO ())
                  → (DebugConsole → IO ())
                  → IO ()
withStalledServer stall stallAccept sink act = do
    let tweak cfg = cfg { dscAccept = stallAccept stall
                        , dscOnStopIncomplete = sink }
    withReleasedStall stall $ withQuietServer tweak (\_ console → act console)
    awaitLeft stall 1 `shouldReturn` True

-- | 'withServer' with its diagnostics captured and ignored.
withQuietServer ∷ (DebugServerConfig → DebugServerConfig)
                → (Int → DebugConsole → IO α) → IO α
withQuietServer tweak act = do
    diags ← newDiagnostics
    withServer (testConfig diags tweak) act

-- ---------------------------------------------------------------- --
-- Small helpers
-- ---------------------------------------------------------------- --

data SinkKind = BlockingSink | ThrowingSink

record ∷ IORef [α] → α → IO ()
record ref x = atomicModifyIORef' ref (\xs → (xs <> [x], ()))

-- | Warnings are delivered on helper threads, so they arrive shortly
--   AFTER the stop returns; wait for the expected number, then return
--   whatever is there.
awaitWarnings ∷ IORef [α] → Int → IO [α]
awaitWarnings ref wanted = do
    void ∘ waitFor twoSeconds $ (≥ wanted) ∘ length ⊚ readIORef ref
    readIORef ref

awaitCount ∷ IORef [α] → Int → IO Bool
awaitCount ref wanted = waitFor twoSeconds $ (≥ wanted) ∘ length ⊚ readIORef ref

waitFor ∷ Int → IO Bool → IO Bool
waitFor budget check = do
    start ← getMonotonicTimeNSec
    let loop = do
            held ← check
            if held then pure True else do
                now ← getMonotonicTimeNSec
                if micros start now ≥ budget then pure False
                    else threadDelay 5000 ≫ loop
    loop

-- | Time a stop made directly by the test, on a thread of its own so an
--   unbounded one FAILS the example — reading as 'generousJoin' — rather
--   than hanging it: a synchronous kill of a masked thread blocks even an
--   unmasked caller for good.
timedStop ∷ IO () → IO Int
timedStop stop = do
    done ← newEmptyMVar
    (elapsed, finished) ← timedWith $ do
        void ∘ forkIO $ stop `finally` putMVar done ()
        isJust ⊚ timeout generousJoin (takeMVar done)
    pure (if finished then elapsed else max elapsed generousJoin)

timedWith ∷ IO α → IO (Int, α)
timedWith act = do
    start ← getMonotonicTimeNSec
    r ← act
    end ← getMonotonicTimeNSec
    pure (micros start end, r)

micros ∷ Word64 → Word64 → Int
micros start end = fromIntegral ((end - start) `div` 1000)

-- | The bound plus 'schedulingTolerance'.
withinBound ∷ Int → Bool
withinBound elapsed = elapsed ≤ listenerJoinMicros + schedulingTolerance

-- | Slack over 'listenerJoinMicros' for thread scheduling and helper
--   start-up on a loaded CI runner. The regressions this suite exists
--   for overrun by the whole indefinite stall, not by a fraction.
schedulingTolerance ∷ Int
schedulingTolerance = 750000

-- | Long enough that a bounded stop always fits, short enough that the
--   unbounded one fails the example instead of hanging it.
generousJoin ∷ Int
generousJoin = listenerJoinMicros + fiveSeconds

-- | Let helper threads that are going to report something do so.
settle ∷ IO ()
settle = threadDelay 200000

twoSeconds, fiveSeconds ∷ Int
twoSeconds  = 2000000
fiveSeconds = 5000000
