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
module Engine.Core.Thread where

import UPrelude
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, finally, throwIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Void (Void, absurd)
import System.Timeout (timeout)
import qualified Data.Text as T
import Engine.Core.Error.Exception
    (EngineException(..), ExceptionType(..), SystemError(..), mkErrorContext)
import Engine.Core.Log
    (LogCategory, LoggerState, logError, logInfo, logWarn)

data ThreadState = ThreadState
    { tsRunning  ∷ IORef ThreadControl
    , tsThreadId ∷ ThreadId
    , tsDone     ∷ MVar ()
      -- ^ Filled exactly once when the thread's loop actually exits —
      --   via 'finally' at the fork site, so it covers a clean stop, a
      --   self-crash, and an async 'killThread' alike. Distinct from
      --   'tsRunning' (the stop *request*) so 'shutdownThread' genuinely
      --   waits instead of reading back the flag it just set.
    }

data ThreadControl = ThreadRunning | ThreadPaused | ThreadStopped
    deriving (Show, Eq)

-- | How long a 'ThreadPaused' iteration sleeps before re-reading the
--   control ref. Shared so every worker polls at the same 100 ms.
pausedPollMicros ∷ Int
pausedPollMicros = 100000

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
    { wsLoggerRef   ∷ IORef LoggerState
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
      -- ^ Runs on the 'ThreadStopped' branch: per-worker stop logging
      --   and cleanup (the Lua thread drains its debug queue and closes
      --   its Lua state here).
    , wsOnCrash     ∷ σ → SomeException → IO ()
      -- ^ Runs when a tick throws. Every worker is fail-stop, so the
      --   loop always ends afterwards; the callback owns only the
      --   reporting and cleanup, not the decision.
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
                wsFailMsg spec <> T.pack (show e)
            throwIO $ EngineException
                (ExSystem (IOError (T.pack (show e))))
                (wsFailFatal spec)
                mkErrorContext
        )
    pure $ (\tid → ThreadState stateRef tid doneVar) ⊚ result

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
                        wsOnCrash spec seed e
                        pure Nothing
                    )
                case next of
                    Just seed' → go seed'
                    Nothing    → pure ()

-- | Signal stop and block until the thread's loop actually exits, up to
--   a 10 s timeout, then force-kill. Idempotent: a second call once the
--   thread is already stopped returns immediately (no re-wait).
shutdownThread ∷ ThreadState → IO ()
shutdownThread ts = do
    tstate ← readIORef (tsRunning ts)
    case tstate of
        ThreadStopped → pure ()
        _ → do
            writeIORef (tsRunning ts) ThreadStopped
            result ← timeout (10 * 1000000) (takeMVar (tsDone ts))
            case result of
                Just () → pure ()
                Nothing → killThread (tsThreadId ts)
