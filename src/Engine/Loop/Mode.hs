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
  , frameBudgetMicros
    -- * The save-barrier handshake, exported for its gate
  , runGatedByCaptureLock
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.Monad
import Engine.Core.State (EngineEnv, EngineLifecycle(..), lifecycleRef
                         , inputQueue, saveBarrierRef)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logInfoM, logWarnM, logDebugM)
import Engine.Loop.Timing (primeFrameTiming)
import Engine.Save.Barrier
    (SaveOwner(..), acknowledgeCurrent, captureLocked, ownerGated)
import Engine.Scripting.Lua.Message (processLuaMessages, discardLuaMessagesForActiveLoad)

-- | Everything that genuinely differs between the three main loops.
--   Everything ELSE — the lifecycle dispatch, the startup handshake
--   ('runStartupHandshake') and the save-barrier-gated Lua drain
--   (@runGatedByCaptureLock@) — is identical in all three and lives in
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
    -- ^ Debug line belonging to the 'EngineStarting' → 'EngineRunning'
    --   transition, and so logged only when that promotion actually
    --   commits ('runStartupHandshake'); 'Nothing' for headless, which
    --   has never logged one.
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
    --   @runGatedByCaptureLock@). Windowed and offscreen; headless has
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
                                 <> (tshow (length flushed) <> " events flushed")

    promoted ← liftIO $ promoteToRunning env
    when promoted $
        maybe (pure ()) (logDebugM CatSystem) (lmRunningLog mode)

    -- The render loop's initial monotonic sample (#2204), taken here
    -- because this is the one step every mode runs exactly once before
    -- its first 'EngineRunning' tick: the first frame's delta is then
    -- measured from this instant, never from the zero
    -- 'Engine.Core.Defaults.defaultEngineState' starts with.
    primeFrameTiming

-- | Promote a STARTING engine to 'EngineRunning', leaving any lifecycle
--   another thread has already advanced exactly as it found it:
--
--   > EngineStarting → EngineRunning
--   > EngineRunning  → EngineRunning
--   > CleaningUp     → CleaningUp
--   > EngineStopped  → EngineStopped
--
--   This used to be an unconditional @writeIORef ... EngineRunning@,
--   which silently discarded a shutdown requested during startup
--   (issue #1283). @engine.quit()@ is a debug-console BUILT-IN: it runs
--   on the per-connection client thread, writes 'CleaningUp' and acks
--   @"shutting down"@ without ever touching the Lua thread
--   ('Engine.Scripting.Lua.Thread.Console.debugBuiltin'). The console
--   prints its @READY@ marker when the LISTENER binds
--   ('Engine.Scripting.Lua.DebugServer'), which happens before the
--   remaining workers start and before this handshake runs — so a
--   client following the documented "wait for READY, then send"
--   contract lands inside the window where this write was still
--   pending, and the engine went on running with no pending quit and
--   no way to be stopped through its only control surface.
--
--   The vulnerable interval is NOT just 'startupSettleMicros': it spans
--   every boot step between the READY print and this promotion, so
--   widening the delay is not what fixes it. Monotonicity is.
--
--   The read and the write must be ONE atomic step. The console thread
--   can advance the lifecycle between a separate read and write, which
--   is the very interleaving being defended against.
--
--   Answers whether the transition COMMITTED, decided inside that same
--   atomic step for the same reason: 'lmRunningLog' describes this
--   transition, so a handshake the promotion refused must not go on to
--   announce that the engine is running (issue #1263). Re-reading the
--   lifecycle afterwards to find out would reintroduce the race in the
--   report rather than the write.
promoteToRunning ∷ EngineEnv → IO Bool
promoteToRunning env =
    atomicModifyIORef' (lifecycleRef env) $ \cur →
        if cur ≡ EngineStarting
            then (EngineRunning, True)
            else (cur, False)

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
--   Merely READING 'captureLocked' and skipping the tick when it is
--   held would not be enough. A thread that only reads the lock is not
--   an 'Engine.Save.Barrier.SaveOwner', so the barrier has nothing to
--   wait for on its behalf. Registering 'SaveRender' in the load
--   transaction's owner set is what makes the barrier wait for this
--   thread at all: 'Engine.Save.Barrier.waitForOwners' returns only
--   once every owner in that set has acknowledged the required
--   quiescence passes, and the transaction moves on to the snapshot
--   boundary and the publish only after it returns. That is why the
--   'acknowledgeCurrent' below is UNCONDITIONAL — it is this thread's
--   half of that handshake — and why the function has the same "check
--   locked, do unlocked work if not locked, always ack" shape every
--   other owner uses (Unit/Building/Combat/Simulation, see e.g.
--   'Unit.Thread').
--
--   Owner participation establishes that WAIT; it is the per-tick gate
--   read below, not the handshake, that decides whether THIS tick's
--   gated work runs. Acknowledgment and the transaction's later move to
--   the snapshot boundary are still separate steps — so that gate is
--   'Engine.Save.Barrier.ownerGated', not 'captureLocked' (#2221).
--   'captureLocked' alone would read False in the window between this
--   thread's own final-pass acknowledgment and the boundary (which
--   needs every OTHER owner's final ack plus the initiator's
--   'reachSnapshot'), letting this thread begin another unlocked tick
--   that could still be running when the publish writes cameraRef /
--   worldQuadsRef. 'ownerGated' parks this owner from its own final
--   acknowledgment until capture completes or the barrier is aborted,
--   so no such tick is ever started.
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
--   The park and the DISCARD are deliberately gated on different
--   things. Parking this thread's own work is safe at any point,
--   because nothing is destroyed by it: whatever stays in
--   'luaToEngineQueue' is still there afterwards, whichever way the
--   transaction ends. Flushing that queue is NOT safe that early — it
--   is irreversible, and a load that fails at ANY point before the
--   publish (another owner times out, 'applyLuaLoad' raises) leaves the
--   OLD session live and unchanged by contract, so its queued scene/UI
--   work must still be there to run. The boundary is the first moment
--   the publish is actually committed to, so the flush waits for
--   'captureLocked' while the park does not.
--
--   'lmEndOfTick' still runs every tick regardless (called by
--   'runLoopTick' after this), so a rendering mode keeps presenting
--   throughout.
runGatedByCaptureLock ∷ LoopMode σ → EngineEnv → EngineM σ ()
runGatedByCaptureLock mode env = do
    locked ← liftIO $ ownerGated (saveBarrierRef env) SaveRender
    if locked
        then do
            atBoundary ← liftIO $ captureLocked (saveBarrierRef env)
            when atBoundary $ do
                discarded ← liftIO $ discardLuaMessagesForActiveLoad env
                when (discarded > 0) $
                    logWarnM CatLua $ "Load publication discarded "
                        <> tshow discarded <> " stale Lua-to-engine message(s)"
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
