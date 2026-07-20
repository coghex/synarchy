{-# LANGUAGE CPP, UnicodeSyntax #-}
module Engine.Loop
  ( mainLoop
  , mainLoopOffscreen
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logInfoM, logWarnM, logDebugM)
import Engine.Core.Error.Exception (ExceptionType(..), SystemError(..))
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Loop.Timing (updateFrameTiming)
import Engine.Loop.Frame (drawFrame)
import Engine.Loop.Camera (updateCameraPanning, updateCameraMouseDrag
                          , updateCameraZoom)
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, captureLocked)
import Engine.Scripting.Lua.Message (processLuaMessages, discardLuaMessagesForActiveLoad)

mainLoop ∷ EngineM ε σ ()
mainLoop = do
    env ← ask
    _state ← gets graphicsState
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    case lifecycle of
        EngineStarting → handleEngineStarting env mainLoop
        EngineRunning  → handleEngineRunning
        CleaningUp     → logDebugM CatSystem "Engine is cleaning up"
        EngineStopped  → logDebugM CatSystem "Engine has stopped"

-- | Offscreen (#650) main loop: the windowed loop minus GLFW — no
--   event polling (input arrives only through the inject verbs, #644)
--   and no window-close check (engine.quit is the only exit). With no
--   vsync'd present to pace frames, each iteration sleeps a ~60 fps
--   budget so an offscreen instance doesn't spin the GPU — several may
--   run in parallel (that is this mode's point).
mainLoopOffscreen ∷ EngineM ε σ ()
mainLoopOffscreen = do
    env ← ask
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    case lifecycle of
        EngineStarting → handleEngineStarting env mainLoopOffscreen
        EngineRunning  → handleEngineRunningOffscreen
        CleaningUp     → logDebugM CatSystem "Engine is cleaning up"
        EngineStopped  → logDebugM CatSystem "Engine has stopped"

handleEngineStarting ∷ EngineEnv → EngineM ε σ () → EngineM ε σ ()
handleEngineStarting env continue = do
    logDebugM CatSystem "Engine starting..."
    liftIO $ threadDelay 100000

    flushed ← liftIO $ Q.flushQueue (inputQueue env)
    when (not $ null flushed) $
        logWarnM CatThread $ "Unexpected inputs during startup: "
                                 <> (T.pack (show (length flushed)) <> " events flushed")

    logDebugM CatSystem "Engine running"
    liftIO $ writeIORef (lifecycleRef env) EngineRunning
    continue

-- | Gate camera updates and Lua-to-engine message processing on the
--   save barrier's capture lock, and genuinely PARTICIPATE in the
--   barrier as a 'SaveRender' owner (round 15 review, issue #763,
--   revised twice — see below). A load transaction's publish
--   ("World.Load.Publish.publishStagedSession") writes cameraRef/
--   worldQuadsRef/etc. entirely inside that window, so a held pan/drag
--   computed against the pre-load camera/input state must not land
--   moments after publish already wrote the replacement camera, and a
--   stale Lua-to-engine message (scene mutations, sprite/text changes,
--   destroys) must not run against the freshly-published session.
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
--   'SaveRender' is included in a load's owner set only when a render
--   loop is actually running (see
--   'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged' — headless
--   boots run neither 'mainLoop' nor 'mainLoopOffscreen' at all, so
--   nothing would ever acknowledge it there); 'acknowledgeCurrent' is a
--   no-op when the current transaction's owner set doesn't include
--   'SaveRender' (a plain save, or a headless load), so this call is
--   safe unconditionally. 'drawFrame' still runs every tick regardless
--   (called by both callers after this), so the window/offscreen
--   target keeps presenting throughout.
runGatedByCaptureLock ∷ EngineEnv → EngineM ε σ ()
runGatedByCaptureLock env = do
    locked ← liftIO $ captureLocked (saveBarrierRef env)
    if locked
        then do
            discarded ← liftIO $ discardLuaMessagesForActiveLoad env
            when (discarded > 0) $
                logWarnM CatLua $ "Load publication discarded "
                    <> T.pack (show discarded) <> " stale Lua-to-engine message(s)"
        else do
            updateCameraPanning
            updateCameraZoom
            updateCameraMouseDrag
            processLuaMessages
    liftIO $ acknowledgeCurrent (saveBarrierRef env) SaveRender

handleEngineRunning ∷ EngineM ε σ ()
handleEngineRunning = do
    state ← gets graphicsState
    window ← case glfwWindow state of
        Nothing → logAndThrowM CatGraphics
                    (ExSystem (GLFWError "handleEngineRunning: "))
                    "No GLFW window available"
        Just w  → pure w

    let Window glfwWin = window

    GLFW.pollEvents
    env ← ask
    runGatedByCaptureLock env

    shouldClose ← GLFW.windowShouldClose glfwWin
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    if shouldClose ∨ lifecycle ≢ EngineRunning
        then do
            logInfoM CatSystem "Engine shutting down..."
            liftIO $ writeIORef (lifecycleRef env) CleaningUp
        else unless (lifecycle ≡ CleaningUp) $ do
            drawFrame
            updateFrameTiming
            mainLoop

handleEngineRunningOffscreen ∷ EngineM ε σ ()
handleEngineRunningOffscreen = do
    env ← ask
    runGatedByCaptureLock env

    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    if lifecycle ≢ EngineRunning
        then do
            logInfoM CatSystem "Engine shutting down..."
            liftIO $ writeIORef (lifecycleRef env) CleaningUp
        else do
            drawFrame
            -- Frame pacing: no swapchain present blocks offscreen, so
            -- sleep the ~60 fps budget the windowed vsync provides.
            liftIO $ threadDelay 16666
            updateFrameTiming
            mainLoopOffscreen
