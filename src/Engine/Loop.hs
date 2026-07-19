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
import Engine.Save.Barrier (captureLocked)
import Engine.Scripting.Lua.Message (processLuaMessages)

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
--   save barrier's capture lock (round 15 review, issue #763; revised
--   after the original fix's discard-side-effect turned out to belong
--   here instead — see below). A load transaction's publish
--   ("World.Load.Publish.publishStagedSession") writes cameraRef/
--   worldQuadsRef/etc. entirely inside that window. This thread is not
--   itself a save-barrier owner ('Engine.Save.Barrier.SaveOwner' has no
--   main/render member), so it can only observe the lock
--   opportunistically, per tick, via 'captureLocked' — unlike a real
--   owner, merely SKIPPING this tick's work isn't enough on its own:
--   anything already sitting in 'luaToEngineQueue' was queued against
--   the session about to be replaced (or, if queued during the
--   unlocked staging window before the lock even began, is stale the
--   instant this publish lands) and would otherwise still be sitting
--   there to run against the REPLACEMENT session on the next unlocked
--   tick. Discarding it here — on this thread, the sole consumer of
--   'luaToEngineQueue' via 'processLuaMessages' — closes that window
--   without racing a foreign thread's flush of the same queue: the
--   original round 15 fix put this discard on the WORLD thread instead
--   (inside 'World.Load.Publish.discardStaleQueues'), which raced this
--   thread's own drain and was observed to leave a load transaction's
--   publish-side work permanently stuck rather than merely losing one
--   message — see that module's haddock for the corrected division of
--   labour. Camera updates are simply skipped: a held pan/drag computed
--   against the pre-load camera/input state must not land moments
--   after publish already wrote the replacement camera. 'drawFrame'
--   still runs every tick regardless (called by both callers after
--   this), so the window/offscreen target keeps presenting throughout.
runGatedByCaptureLock ∷ EngineEnv → EngineM ε σ ()
runGatedByCaptureLock env = do
    locked ← liftIO $ captureLocked (saveBarrierRef env)
    if locked
        then do
            stale ← liftIO $ Q.flushQueue (luaToEngineQueue env)
            unless (null stale) $
                logWarnM CatLua $
                    "Save/load publish discarded " <> T.pack (show (length stale))
                    <> " stale Lua-to-engine message(s) queued against the "
                    <> "session being replaced"
        else do
            updateCameraPanning
            updateCameraZoom
            updateCameraMouseDrag
            processLuaMessages

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
