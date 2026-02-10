{-# LANGUAGE CPP #-}
module Engine.Loop
  ( mainLoop
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
import Engine.Input.Event (handleInputEvents)
import Engine.Loop.Timing (updateFrameTiming)
import Engine.Loop.Frame (drawFrame)
import Engine.Loop.Camera (updateCameraPanning)
import Engine.Scripting.Lua.Message (processLuaMessages)

-- | Main engine loop
mainLoop ∷ EngineM ε σ ()
mainLoop = do
    env ← ask
    state ← gets graphicsState
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    case lifecycle of
        EngineStarting → handleEngineStarting env
        EngineRunning  → handleEngineRunning
        CleaningUp     → logDebugM CatSystem "Engine is cleaning up"
        EngineStopped  → logDebugM CatSystem "Engine has stopped"

-- | Handle engine starting state
handleEngineStarting ∷ EngineEnv → EngineM ε σ ()
handleEngineStarting env = do
    logDebugM CatSystem "Engine starting..."
    liftIO $ threadDelay 100000
    
    -- Clear the input queue before entering main loop
    flushed ← liftIO $ Q.flushQueue (inputQueue env)
    when (not $ null flushed) $
        logWarnM CatThread $ "Unexpected inputs during startup: "
                                 <> (T.pack (show (length flushed)) <> " events flushed")
    
    logDebugM CatSystem "Engine running"
    liftIO $ writeIORef (lifecycleRef env) EngineRunning
    mainLoop

-- | Handle engine running state
handleEngineRunning ∷ EngineM ε σ ()
handleEngineRunning = do
    state ← gets graphicsState
    window ← case glfwWindow state of
        Nothing → logAndThrowM CatGraphics
                    (ExSystem (GLFWError "handleEngineRunning: "))
                    "No GLFW window available"
        Just w  → pure w
    
    let Window glfwWin = window
    
   
    -- Process events
    GLFW.pollEvents
    handleInputEvents
    updateCameraPanning
    processLuaMessages
    
    -- Check for shutdown
    shouldClose ← GLFW.windowShouldClose glfwWin
    env ← ask
    lifecycle ← liftIO $ readIORef (lifecycleRef env)
    
    if shouldClose || lifecycle ≢ EngineRunning
        then do
            logInfoM CatSystem "Engine shutting down..."
            liftIO $ writeIORef (lifecycleRef env) CleaningUp
        else unless (lifecycle ≡ CleaningUp) $ do
            drawFrame
            updateFrameTiming
            mainLoop
