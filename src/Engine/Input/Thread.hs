{-# LANGUAGE Strict #-}
-- | Input thread: owns the OS thread lifecycle and the per-tick
--   drain/sleep loop. #787 split the event-processing/dispatch logic
--   this facade used to own inline out into sibling modules so this
--   file stays a thin thread-loop entrypoint:
--
--     * Queue draining + top-level per-event routing lives in
--       'Engine.Input.Thread.Dispatch'; its 'processInputs' and
--       'processInput' entrypoints are re-exported here.
--     * Per-domain dispatch lives in 'Engine.Input.Thread.Keyboard',
--       'Engine.Input.Thread.Char', 'Engine.Input.Thread.Mouse', and
--       'Engine.Input.Thread.Scroll', reached only through 'Dispatch'
--       — none of those modules are re-exported.
--
--   'startInputThread' and @inputTick@ are defined in this facade
--   itself, not re-exported from elsewhere; the loop skeleton they plug
--   into is 'Engine.Core.Thread.startWorkerThread' (#1147).
module Engine.Input.Thread
  ( startInputThread
  , processInputs
  , processInput
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef (writeIORef, readIORef)
import Engine.Core.Log (logDebug, logError, LogCategory(..))
-- #892 (E4): `inputStateRef` through the input capability's
-- worker-safe view, the logger/lifecycle/input-thread-started flag
-- through `core-init` (#889), and `saveBarrierRef` as an explicit
-- narrow value — the SS7.3 cross-capability read into
-- `save-load-coordination`, which has no record of its own (SS7.8's
-- own row is empty; its modules are permanent SS6.1 exceptions). The
-- opaque `EngineEnv` is still threaded into @inputTick@/
-- 'processInputs', which hand it on to not-yet-narrowed callees.
import Engine.Core.State (EngineEnv, saveBarrierRef)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.InputView
    (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Thread
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, ownerGated)
import Engine.Input.Thread.Dispatch (processInputs, processInput)

startInputThread ∷ EngineEnv → IO ThreadState
startInputThread env = startWorkerThread WorkerSpec
    { wsName        = "Input"
    , wsLoggerRef   = ccLoggerRef (toCoreCapability env)
    , wsCategory    = CatInput
    , wsLifecycleRef = ccLifecycleRef (toCoreCapability env)
    , wsCrashSink   = workerCrashStderrSink
    , wsStartingMsg = "Starting input thread..."
      -- This worker has never logged a post-fork line.
    , wsStartedMsg  = Nothing
    , wsFailMsg     = "Failed starting input thread: "
    , wsFailLevel   = WorkerFailError
    , wsFailFatal   = "Input thread start failure."
    , wsStartup     = \_ → noRefusal $
        -- Only a REAL boot path (Graphical/Offscreen/Preview) ever
        -- calls this — App.Headless never does (no GLFW window to
        -- poll) — so this is the single source of truth
        -- saveWorldFn/handleLoadStaged consult to decide whether
        -- SaveInput belongs in a transaction's owner set (it must
        -- not be a hard requirement headless boot can never
        -- satisfy).
        writeIORef (ccInputThreadActiveRef (toCoreCapability env)) True
    , wsTick        = \_ → inputTick env
    , wsOnStop      = \_ → do
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        logDebug logger CatInput "Input thread stopping..."
    , wsOnCrash     = \_ e → do
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        logError logger CatInput $ "Input thread crashed: " <> tshow e
      -- The lifecycle write this line used to precede belongs to the
      -- shared loop now, ahead of the log (#2283).
    , wsOnCrashCleanup = \_ _ → pure ()
    }

inputTick ∷ EngineEnv → IO (Maybe ())
inputTick env = do
    -- Issue #763: Input joins the save
    -- barrier's owner set as SaveInput so a load publish can
    -- actually quiesce it, same as every other owner —
    -- previously Input was not a SaveOwner at all, so it kept
    -- draining inputQueue and dispatching fresh Lua/gameplay
    -- messages for the ENTIRE captureLocked window, well past
    -- the one-time luaQueue flush
    -- 'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'
    -- performs before queuing WorldLoadPublish. Gating this the
    -- same way Unit/Combat/Simulation already are closes that:
    -- a pre-load input event still sitting in inputQueue at the
    -- lock boundary is left there (and discarded by
    -- World.Load.Publish's queue flush) rather than being
    -- dispatched against the replacement session.
    -- #2221: 'ownerGated', not 'captureLocked' — this owner stays
    -- parked from its own final-pass acknowledgement until capture
    -- completes, so a pre-transaction input event cannot be dispatched
    -- in the gap between that acknowledgement and the boundary.
    locked ← ownerGated (saveBarrierRef env) SaveInput
    unless locked $ do
        inpSt ← readIORef (ivInputStateRef (toInputViewCapability env))
        -- processInputs publishes to inputStateRef after each
        -- event it processes (#697) — no batch write here.
        _ ← processInputs env inpSt
        pure ()
    acknowledgeCurrent (saveBarrierRef env) SaveInput
    threadDelay 16666  -- ~60 FPS
    pure (Just ())
