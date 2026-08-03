-- | The one definition of the engine's main-loop shape, shared by all
--   three boot modes (issue #1022).
--
--   'Engine.Loop.mainLoop' (windowed), 'Engine.Loop.mainLoopOffscreen'
--   (#650) and 'Engine.Loop.Headless.headlessLoop' used to be three
--   hand-written loops. The lifecycle dispatch, the startup handshake
--   and — most importantly — the save-barrier handshake below were
--   duplicated between them, so a change to the barrier protocol had to
--   find and correctly update two copies of the same code and only one
--   copy of the reasoning behind it. Each mode now supplies a
--   'LoopMode' describing only what genuinely differs, and
--   'runLoopMode' drives the rest.
module Engine.Loop.Mode
  ( LoopMode(..)
  , runLoopMode
  , runStartupHandshake
  , runGatedByCaptureLock
  , frameBudgetMicros
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Core.Monad
import Engine.Core.State (EngineEnv, EngineLifecycle(..), lifecycleRef
                         , inputQueue, saveBarrierRef)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logInfoM, logWarnM, logDebugM)
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, captureLocked)
import Engine.Scripting.Lua.Message (processLuaMessages, discardLuaMessagesForActiveLoad)

-- | Everything that genuinely differs between the three main loops.
--   Everything ELSE — the lifecycle dispatch, the startup handshake
--   ('runStartupHandshake') and the save-barrier-gated Lua drain
--   ('runGatedByCaptureLock') — is identical in all three and lives in
--   this module only.
--
--   The surviving differences, in tick order:
--
--   @
--                    windowed             offscreen            headless
--   lmPollEvents     GLFW.pollEvents      --                   --
--   lmCameraUpdates  pan/zoom/mouse-drag  pan/zoom/mouse-drag  --
--   lmExitRequested  windowShouldClose    -- (engine.quit)     -- (engine.quit)
--   lmEndOfTick      draw, timing         draw, sleep, timing  sleep
--   @
--
--   The window is the reason those columns differ at all: only the
--   windowed mode has GLFW events to pump and a close button to honour,
--   only the two rendering modes have a camera to integrate and a frame
--   to draw, and only the two non-windowed modes need to pace
--   themselves ('frameBudgetMicros') because no vsync'd present does it
--   for them.
--
--   The log lines are per-mode too: headless has always described
--   itself differently ("Headless engine starting...") and has never
--   logged a running line at all, so those strings are fields rather
--   than something this module invents.
data LoopMode σ = LoopMode
  { lmStartingLog   ∷ Text
    -- ^ Debug line for the 'EngineStarting' tick.
  , lmRunningLog    ∷ Maybe Text
    -- ^ Debug line logged just before the 'EngineRunning' transition;
    --   'Nothing' for headless, which has never logged one.
  , lmShutdownLog   ∷ Text
    -- ^ Info line logged when the loop decides to stop.
  , lmCleaningUpLog ∷ Text
    -- ^ Debug line for a 'CleaningUp' tick.
  , lmStoppedLog    ∷ Text
    -- ^ Debug line for an 'EngineStopped' tick.
  , lmPollEvents    ∷ EngineM σ ()
    -- ^ Pump the window system. Windowed only — offscreen and headless
    --   receive input solely through the injection verbs (#644).
  , lmCameraUpdates ∷ EngineM σ ()
    -- ^ Per-tick camera integration, run only on an UNLOCKED tick (see
    --   'runGatedByCaptureLock'). Windowed and offscreen; headless has
    --   no camera to integrate and never ran these even before #763.
  , lmExitRequested ∷ EngineM σ Bool
    -- ^ A mode-specific reason to shut down, checked alongside the
    --   lifecycle. Windowed reports the GLFW close button; offscreen
    --   and headless exit only via @engine.quit@ (i.e. the lifecycle),
    --   so they answer 'False'.
  , lmEndOfTick     ∷ EngineM σ ()
    -- ^ Everything the mode does after deciding to keep running, in its
    --   own order: windowed draws then updates timing (the swapchain
    --   present paces it), offscreen draws, sleeps 'frameBudgetMicros'
    --   then updates timing, headless only sleeps.
  }

-- | The ~60 fps frame budget the two non-windowed modes sleep each
--   tick. The windowed loop needs none — its vsync'd present blocks.
frameBudgetMicros ∷ Int
frameBudgetMicros = 16666

-- | Settle time before the engine leaves 'EngineStarting', giving the
--   worker threads a moment to come up.
startupSettleMicros ∷ Int
startupSettleMicros = 100000

-- | Drive one lifecycle tick of @mode@'s loop, recursing while the
--   engine is meant to keep running.
runLoopMode ∷ LoopMode σ → EngineM σ ()
runLoopMode mode = do
    env ← ask
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    case lifecycle of
        EngineStarting → do
            runStartupHandshake mode env
            runLoopMode mode
        EngineRunning  → runLoopTick mode env
        CleaningUp     → logDebugM CatSystem (lmCleaningUpLog mode)
        EngineStopped  → logDebugM CatSystem (lmStoppedLog mode)

-- | The startup handshake every mode performs exactly once: settle,
--   flush whatever reached 'inputQueue' before the engine was ready,
--   and transition to 'EngineRunning'.
--
--   The flush is REPORTED rather than silently discarded. Headless used
--   to discard it, on the assumption that a boot with no input thread
--   has nothing to flush — but 'Engine.Scripting.Lua.Thread.Dispatch'\'s
--   @LuaInjectFollowup@ writes 'inputQueue' from the input injection
--   verbs (#644), so the queue is not structurally empty in any mode
--   and the same condition deserves the same warning everywhere.
runStartupHandshake ∷ LoopMode σ → EngineEnv → EngineM σ ()
runStartupHandshake mode env = do
    logDebugM CatSystem (lmStartingLog mode)
    liftIO $ threadDelay startupSettleMicros

    flushed ← liftIO $ Q.flushQueue (inputQueue env)
    when (not $ null flushed) $
        logWarnM CatThread $ "Unexpected inputs during startup: "
                                 <> (T.pack (show (length flushed)) <> " events flushed")

    maybe (pure ()) (logDebugM CatSystem) (lmRunningLog mode)
    liftIO $ writeIORef (lifecycleRef env) EngineRunning

-- | Gate the mode's camera updates and Lua-to-engine message
--   processing on the save barrier's capture lock, and genuinely
--   PARTICIPATE in the barrier as a 'SaveRender' owner (issue #763 — see
--   below). A load transaction's publish
--   ("World.Load.Publish.publishStagedSession")
--   writes cameraRef/worldQuadsRef/etc. entirely inside that window, so
--   a held pan/drag computed against the pre-load camera/input state
--   must not land moments after publish already wrote the replacement
--   camera, and a stale Lua-to-engine message (scene mutations,
--   sprite/text changes, destroys) must not run against the
--   freshly-published session.
--
--   The first attempt at this fix only READ 'captureLocked' as a
--   point-in-time pre-check, skipping this tick's work when locked —
--   but this thread was not a real 'Engine.Save.Barrier.SaveOwner' at
--   all, so nothing ever waited for it: the barrier could reach the
--   snapshot boundary and publish in the gap between the check and the
--   camera/message work it gated, exactly the race a real owner
--   (Unit/Building/Combat/Simulation, see e.g. 'Unit.Thread') never has
--   — those threads' own per-tick 'acknowledgeCurrent' calls are what
--   'waitForOwners' blocks on before the barrier is ever allowed to
--   reach the snapshot boundary in the first place. Adding 'SaveRender'
--   as a genuine owner (acknowledged unconditionally below, mirroring
--   'Unit.Thread'\'s "check locked, do unlocked work if not locked,
--   always ack" shape) closes the window structurally instead of by
--   timing: the publish literally cannot happen until this thread has
--   already acknowledged the end of its own last unlocked tick, so its
--   camera/message work can never be concurrent with the ref swap.
--
--   EVERY load-capable mode acknowledges 'SaveRender', headless
--   included: 'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'
--   inserts it unconditionally, because a headless boot runs
--   'Engine.Loop.Headless.headlessLoop' — which drains the very same
--   'luaToEngineQueue' through the very same 'processLuaMessages', and
--   so would otherwise sit outside the publication boundary. A plain
--   SAVE is what omits it ('Engine.Scripting.Lua.API.Save.saveOwnerSet'
--   never adds it), and 'acknowledgeCurrent' is a no-op whenever the
--   current transaction's owner set excludes 'SaveRender' — so this
--   call is safe unconditionally, every tick, in every mode.
--
--   'lmEndOfTick' still runs every tick regardless (called by
--   'runLoopTick' after this), so a rendering mode keeps presenting
--   throughout.
runGatedByCaptureLock ∷ LoopMode σ → EngineEnv → EngineM σ ()
runGatedByCaptureLock mode env = do
    locked ← liftIO $ captureLocked (saveBarrierRef env)
    if locked
        then do
            discarded ← liftIO $ discardLuaMessagesForActiveLoad env
            when (discarded > 0) $
                logWarnM CatLua $ "Load publication discarded "
                    <> T.pack (show discarded) <> " stale Lua-to-engine message(s)"
        else do
            lmCameraUpdates mode
            processLuaMessages
    liftIO $ acknowledgeCurrent (saveBarrierRef env) SaveRender

-- | One 'EngineRunning' tick: pump the window system, do the gated
--   work, then either shut down or run the mode's end-of-tick work and
--   go round again.
runLoopTick ∷ LoopMode σ → EngineEnv → EngineM σ ()
runLoopTick mode env = do
    lmPollEvents mode
    runGatedByCaptureLock mode env

    exitRequested ← lmExitRequested mode
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    if exitRequested ∨ lifecycle ≢ EngineRunning
        then do
            logInfoM CatSystem (lmShutdownLog mode)
            liftIO $ writeIORef (lifecycleRef env) CleaningUp
        else do
            lmEndOfTick mode
            runLoopMode mode
