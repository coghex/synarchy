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
    updateCameraPanning
    updateCameraZoom
    updateCameraMouseDrag
    processLuaMessages

    shouldClose ← GLFW.windowShouldClose glfwWin
    env ← ask
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
    updateCameraPanning
    updateCameraZoom
    updateCameraMouseDrag
    processLuaMessages

    env ← ask
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
