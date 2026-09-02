-- | The one definition of a worker thread's lifecycle.
--
--   Six workers — 'Unit.Thread', 'Combat.Thread', 'World.Thread',
--   'Sim.Thread', 'Engine.Scripting.Lua.Thread' and
--   'Engine.Input.Thread' — used to hand-implement the same startup and
--   the same loop skeleton: a 'ThreadRunning' 'IORef', an empty
--   done-'MVar', a caught 'forkIO' whose body is
--   @loop \`finally\` putMVar doneVar ()@, and the identical
--   stopped\/paused\/running dispatch. This module already owned
--   'shutdownThread'; #1147 folded the other half in beside it, so the
--   stack-growth invariant on the running branch is now stated and
--   enforced ONCE instead of having to stay correct in six places
--   independently.
--
--   What is shared is only the control-state dispatch, the paused poll,
--   the one-catch-per-running-tick boundary, the fork\/finalizer, the
--   completion signal, and the typed startup-failure throw. Everything
--   a worker actually does — its setup, its per-tick body, its
--   save-barrier participation and acknowledgements, its stop cleanup,
--   its crash handling — stays a callback on 'WorkerSpec', because the
--   six differ there and must keep differing (see 'WorkerSpec' and
--   #1147 requirement 4).
--
--   The definition deliberately does NOT mention 'EngineEnv': a worker
--   passes whatever it needs through its own closures, which is what
--   lets @Test.Headless.Core.WorkerLifecycle@ drive this module with
--   a bare logger and no engine at all.
--
--   == Shutdown: graceful wait, forced kill, second join (#2165)
--
--   'shutdownThread' is a JOIN, on every path. It writes the stop
--   request, then waits on 'tsDone' up to the graceful timeout
--   ('stGracefulMicros', 10 s in production). A worker that has not
--   exited by then is force-killed and waited for AGAIN, bounded by
--   'stForcedMicros' (5 s in production); only if that second wait also
--   expires does it give up — and giving up is loud: an error line
--   naming the worker on the engine log, then an 'EngineException'.
--   It never returns while the worker's loop, its stop or crash
--   callback, or its fork finalizer may still be running, so a caller
--   may tear down the logger, the Lua state's neighbours, or Vulkan the
--   moment it returns.
--
--   The kill is sent from a helper thread. 'killThread' blocks until
--   the target ACCEPTS the exception, and a worker inside a @safe@
--   foreign call (a long Lua script) or an uninterruptible mask cannot
--   accept it until that call returns — delivered inline, one stuck
--   worker would hang the shutdown caller indefinitely, and the bounded
--   second wait could never reach its report. Making such a worker
--   interruptible is out of scope; the fatal report is the observable
--   outcome there.
--
--   What the kill does on the worker's side: the running-tick guard
--   classifies every exception it catches. One that downcasts to
--   'SomeAsyncException' — 'ThreadKilled' from the kill above, but by
--   the same test any other asynchronous exception, including the
--   GHC-classified 'StackOverflow' and 'HeapOverflow' — is a FORCED
--   TERMINATION, not a crash: the guard logs it as such, naming the
--   worker and the exception, runs 'wsOnStop' (the same cleanup a
--   cooperative stop runs, so the Lua worker still drains its debug
--   queue and closes its state exactly once), and ends the loop.
--   'wsOnCrash' is reserved for everything else — a synchronous tick
--   failure keeps its fail-stop report and its lifecycle write. An
--   asynchronous exception that lands OUTSIDE a tick (in the paused
--   poll, the control read, or 'wsOnStop' itself) unwinds without a
--   second cleanup, which is exactly how the once-only guarantee holds
--   when the stop callback was already running.
--
--   'tsDone' is read, never taken: completion stays observable after a
--   successful join, so a repeated 'shutdownThread' distinguishes a
--   CONFIRMED termination (returns at once) from a merely WRITTEN stop
--   request (waits, and kills, like the first call would). The stop
--   request alone is never trusted as evidence that the loop exited.
--
--   The two timeouts are a parameter of 'shutdownThreadWith'; the
--   production entry point 'shutdownThread' fixes them at
--   'productionShutdownTimeouts', and the focused specs shorten both.
module Engine.Core.Thread where

import UPrelude
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception
    (SomeAsyncException, SomeException, catch, finally, fromException, throwIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Void (Void, absurd)
import System.Timeout (timeout)
import qualified Data.Text as T
import Engine.Core.Error.Exception
    (EngineException(..), ExceptionType(..), SystemError(..), mkErrorContext)
import Engine.Core.Log
    (LogCategory, LoggerState, logError, logInfo, logWarn)

data ThreadState = ThreadState
    { tsRunning   ∷ IORef ThreadControl
    , tsThreadId  ∷ ThreadId
    , tsDone      ∷ MVar ()
      -- ^ Filled exactly once when the thread's loop actually exits —
      --   via 'finally' at the fork site, so it covers a clean stop, a
      --   self-crash, and an async 'killThread' alike. Distinct from
      --   'tsRunning' (the stop *request*) so 'shutdownThread' genuinely
      --   waits instead of reading back the flag it just set. Readers
      --   use 'readMVar', never 'takeMVar': a filled done-'MVar' is the
      --   persistent record that the loop has exited.
    , tsName      ∷ T.Text
      -- ^ 'wsName', so shutdown's own lines can name the worker.
    , tsLoggerRef ∷ IORef LoggerState
    , tsCategory  ∷ LogCategory
    }

data ThreadControl = ThreadRunning | ThreadPaused | ThreadStopped
    deriving (Show, Eq)

-- | How long a 'ThreadPaused' iteration sleeps before re-reading the
--   control ref. Shared so every worker polls at the same 100 ms.
pausedPollMicros ∷ Int
pausedPollMicros = 100000

-- | The two bounds 'shutdownThreadWith' waits under, in microseconds.
data ShutdownTimeouts = ShutdownTimeouts
    { stGracefulMicros ∷ Int
      -- ^ How long a worker gets to honour the stop request before it
      --   is force-killed.
    , stForcedMicros   ∷ Int
      -- ^ How long a force-killed worker gets to unwind before it is
      --   reported as failing to terminate.
    }
    deriving (Show, Eq)

-- | What every production caller waits under: the 10 s graceful
--   timeout the workers have always had, and a finite 5 s bound on the
--   post-kill unwind.
productionShutdownTimeouts ∷ ShutdownTimeouts
productionShutdownTimeouts = ShutdownTimeouts
    { stGracefulMicros = 10 * 1000000
    , stForcedMicros   =  5 * 1000000
    }

-- | The level a worker's startup-failure line is logged at. Five use
--   'logError'; the Lua thread has always used 'logWarn'. Parameterised
--   rather than normalised — #1147 changes no existing log line.
data WorkerFailLevel = WorkerFailError | WorkerFailWarn
    deriving (Show, Eq)

-- | Everything the shared lifecycle cannot decide for a worker.
--
--   @ε@ is how the worker's startup may REFUSE without any thread being
--   forked and without an exception: only the Lua thread has one (a
--   'Engine.Scripting.Lua.DebugServer.DebugListenerFailure' from
--   \#1190), so the other five instantiate it at 'Void' and start
--   through 'startWorkerThread'. @σ@ is the loop's own state: whatever
--   the startup action produced, threaded through every tick, so a
--   worker carrying a counter across iterations (Combat's wound-tick
--   modulo) needs nothing of its own.
data WorkerSpec ε σ = WorkerSpec
    { wsName        ∷ T.Text
      -- ^ The worker's short name (@"Unit"@, @"Lua"@, …), used only by
      --   the lines the shared lifecycle itself logs about it: the
      --   forced-termination and failed-to-terminate reports.
    , wsLoggerRef   ∷ IORef LoggerState
      -- ^ Read once, before the guarded startup, so the failure line can
      --   be logged whatever the startup action did.
    , wsCategory    ∷ LogCategory
    , wsStartingMsg ∷ T.Text
      -- ^ Logged at info level as the first thing inside the guard.
    , wsStartedMsg  ∷ Maybe T.Text
      -- ^ Logged at info level after the fork. 'Nothing' for the two
      --   workers (Input, Lua) that have never emitted a post-fork line.
    , wsFailMsg     ∷ T.Text
      -- ^ Prefix of the startup-failure line; the exception's own text
      --   is appended.
    , wsFailLevel   ∷ WorkerFailLevel
    , wsFailFatal   ∷ T.Text
      -- ^ 'errorMsg' of the thrown 'EngineException' — the text each
      --   worker used to pass to a bare 'error'.
    , wsStartup     ∷ IORef ThreadControl → IO (Either ε σ)
      -- ^ Per-worker setup, run inside the guard. Receives the control
      --   ref because the Lua thread hands it to @registerLuaAPI@.
    , wsTick        ∷ σ → IO (Maybe σ)
      -- ^ One running tick. 'Just' continues the loop with that state;
      --   'Nothing' ends it.
    , wsOnStop      ∷ σ → IO ()
      -- ^ Runs on the 'ThreadStopped' branch — per-worker stop logging
      --   and cleanup (the Lua thread drains its debug queue and closes
      --   its Lua state here) — and ALSO when a running tick is ended
      --   by an asynchronous exception (#2165), so a force-killed
      --   worker gets the same cleanup, once.
    , wsOnCrash     ∷ σ → SomeException → IO ()
      -- ^ Runs when a tick throws a SYNCHRONOUS exception — anything
      --   that does not downcast to 'SomeAsyncException'. Every worker
      --   is fail-stop, so the loop always ends afterwards; the callback
      --   owns only the reporting and cleanup, not the decision.
    }

-- | Startup for the five workers that cannot refuse.
startWorkerThread ∷ WorkerSpec Void σ → IO ThreadState
startWorkerThread spec = either absurd id ⊚ startWorkerThreadEither spec

-- | Convenience for a 'wsStartup' that has no refusal case.
noRefusal ∷ IO σ → IO (Either Void σ)
noRefusal act = Right ⊚ act

-- | Start a worker: create its control ref and done-'MVar', run its
--   startup under a guard, fork its loop, and hand back the
--   'ThreadState' 'shutdownThread' later consumes.
--
--   A startup EXCEPTION is logged at the worker's own level and then
--   rethrown as a typed 'EngineException'. It is deliberately not routed
--   into @guardNativeExceptions@ or the @Either EngineException@ boot
--   channel: every boot module starts its workers strictly before
--   @runEngineM@, so this escapes the boot module uncaught and
--   terminates the process — the same fatal outcome the bare 'error'
--   calls produced, with a typed exception instead of an 'ErrorCall'
--   (#1147 requirement 5, and requirement 7 forbids changing boot
--   sequencing to catch it).
--
--   A startup REFUSAL ('Left') is a different thing entirely: no thread
--   is forked, the done-'MVar' is never filled, and the caller gets the
--   refusal instead of a 'ThreadState'.
startWorkerThreadEither ∷ WorkerSpec ε σ → IO (Either ε ThreadState)
startWorkerThreadEither spec = do
    logger ← readIORef (wsLoggerRef spec)
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    result ← catch
        (do
            logInfo logger (wsCategory spec) (wsStartingMsg spec)
            eSeed ← wsStartup spec stateRef
            case eSeed of
                Left refusal → pure (Left refusal)
                Right seed → do
                    tid ← forkIO $ workerLoop spec stateRef seed
                                     `finally` putMVar doneVar ()
                    forM_ (wsStartedMsg spec) $ logInfo logger (wsCategory spec)
                    pure (Right tid)
        )
        (\(e ∷ SomeException) → do
            logStartFailure spec logger $
                wsFailMsg spec <> tshow e
            throwIO $ EngineException
                (ExSystem (IOError (tshow e)))
                (wsFailFatal spec)
                mkErrorContext
        )
    pure $ (\tid → ThreadState
                { tsRunning   = stateRef
                , tsThreadId  = tid
                , tsDone      = doneVar
                , tsName      = wsName spec
                , tsLoggerRef = wsLoggerRef spec
                , tsCategory  = wsCategory spec
                }) ⊚ result

logStartFailure ∷ WorkerSpec ε σ → LoggerState → T.Text → IO ()
logStartFailure spec logger msg = case wsFailLevel spec of
    WorkerFailError → logError logger (wsCategory spec) msg
    WorkerFailWarn  → logWarn  logger (wsCategory spec) msg

-- | The shared loop: control-state dispatch, paused polling, and the
--   guarded running tick.
workerLoop ∷ WorkerSpec ε σ → IORef ThreadControl → σ → IO ()
workerLoop spec stateRef = go
  where
    go seed = do
        control ← readIORef stateRef
        case control of
            ThreadStopped → wsOnStop spec seed
            ThreadPaused  → do
                threadDelay pausedPollMicros
                go seed
            ThreadRunning → do
                -- One guarded tick per iteration; the recursive call
                -- lives OUTSIDE the catch — inside it, each tick pushes
                -- a catch frame that never pops (unbounded stack
                -- growth). This is the single place that invariant is
                -- now stated; a seventh worker inherits it.
                next ← catch (wsTick spec seed)
                    (\(e ∷ SomeException) → do
                        -- Provenance (throw vs throwTo) is not
                        -- recoverable here; the classification is the
                        -- exception's TYPE, the same downcast
                        -- 'Engine.Loop.Shutdown' and the Lua API's
                        -- function wrapper use.
                        case fromException e ∷ Maybe SomeAsyncException of
                            Just _  → forcedTermination seed e
                            Nothing → wsOnCrash spec seed e
                        pure Nothing
                    )
                case next of
                    Just seed' → go seed'
                    Nothing    → pure ()
    -- A forced termination is not a crash: no crash report, no
    -- lifecycle write — the same cleanup a cooperative stop runs, and
    -- a line that says what happened. Runs inside the catch handler,
    -- so a second kill cannot interrupt the cleanup mid-way.
    forcedTermination seed e = do
        logger ← readIORef (wsLoggerRef spec)
        logWarn logger (wsCategory spec) $
            wsName spec <> " thread forcibly terminated: " <> tshow e
        wsOnStop spec seed

-- | Signal stop and block until the thread's loop actually exits, under
--   'productionShutdownTimeouts': 10 s to stop on its own, then a
--   forced kill and a further 5 s to unwind, then a fatal report.
--   Idempotent: a second call after a confirmed join returns at once,
--   because 'tsDone' stays filled. The full contract is the module
--   haddock's shutdown section.
shutdownThread ∷ ThreadState → IO ()
shutdownThread = shutdownThreadWith productionShutdownTimeouts

-- | 'shutdownThread' with both bounds supplied, so a focused spec can
--   exercise the timeout branch and the post-kill report in
--   milliseconds. Production has one caller and it passes
--   'productionShutdownTimeouts'.
--
--   Throws an 'EngineException' (@ExSystem (TimeoutError …)@) — after
--   logging the same report at error level — when the worker has still
--   not exited 'stForcedMicros' after the kill. 'tsDone' is left empty
--   in that case: nothing was joined, and a later call will say so
--   again rather than pretend otherwise.
shutdownThreadWith ∷ ShutdownTimeouts → ThreadState → IO ()
shutdownThreadWith timeouts ts = do
    -- Written unconditionally: an already-written request is not
    -- evidence that the loop exited, so it never short-circuits the
    -- join below. The write is idempotent.
    writeIORef (tsRunning ts) ThreadStopped
    graceful ← await (stGracefulMicros timeouts)
    unless graceful $ do
        logger ← readIORef (tsLoggerRef ts)
        logWarn logger (tsCategory ts) $
            tsName ts <> " thread did not stop within "
            <> showMillis (stGracefulMicros timeouts)
            <> "; forcing termination"
        -- From a helper: 'killThread' returns only once the target has
        -- accepted the exception, and a worker in a safe foreign call
        -- or an uninterruptible mask cannot accept it yet. The helper
        -- keeps the kill pending for whenever it can; this thread keeps
        -- its wait bounded.
        _ ← forkIO $ killThread (tsThreadId ts)
        forced ← await (stForcedMicros timeouts)
        unless forced $ do
            let detail = tsName ts <> " thread failed to terminate within "
                         <> showMillis (stForcedMicros timeouts)
                         <> " after a forced kill"
            logError logger (tsCategory ts) detail
            throwIO $ EngineException
                (ExSystem (TimeoutError detail))
                (tsName ts <> " thread failed to terminate.")
                mkErrorContext
  where
    -- 'readMVar', not 'takeMVar': the join must leave the completion
    -- record in place for the next reader.
    await micros = isJust ⊚ timeout micros (readMVar (tsDone ts))
    showMillis micros = tshow (micros `div` 1000) <> " ms"
