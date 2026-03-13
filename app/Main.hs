{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (readIORef, writeIORef)
import System.Environment (setEnv, getArgs)
import Engine.Core.Init (initializeEngine, initializeEngineHeadless
                        , EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..)
                         , graphicsState, glfwWindow)
import Engine.Core.Types (EngineConfig(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Log (LogCategory(..), shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Headless (headlessLoop)
import Engine.Loop.Shutdown (shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Backend (startLuaThread)
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)

main ∷ IO ()
main = do
  -- Suppress macOS noise
  setEnv "NSLog_Disabled" "YES"
  -- Enable MoltenVK argument buffers for higher descriptor limits (bindless)
  -- Value 2 = use argument buffers when Metal Tier 2 is available (Apple Silicon)
  setEnv "MVK_CONFIG_USE_METAL_ARGUMENT_BUFFERS" "2"
#ifdef DEVELOPMENT
  setEnv "VK_LOADER_DEBUG" "none"
  setEnv "VK_LOADER_MESSAGE_LEVEL" "error"
  setEnv "VK_LOADER_LOG_LEVEL" "0"
#endif

  args ← getArgs
  let headless = "--headless" `elem` args
      port = parsePort args

  if headless
    then runHeadless port
    else runGraphical port

-- | Parse --port N from args, defaulting to 8008
parsePort ∷ [String] → Maybe Int
parsePort [] = Nothing
parsePort ("--port":n:_) = case reads n of
    [(p, "")] → Just p
    _         → Nothing
parsePort (_:rest) = parsePort rest

-- | Run engine with full graphics (GLFW window + Vulkan)
runGraphical ∷ Maybe Int → IO ()
runGraphical mPort = do
  -- Initialize engine
  EngineInitResult env envVar stateVar ← initializeEngine

  -- Apply port override if given
  let env' = case mPort of
        Just p  → env { engineConfig = (engineConfig env) { ecDebugPort = p } }
        Nothing → env

  -- Fork worker threads
  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'

  -- Load video configuration
  videoConfig ← readIORef (videoConfigRef env')

  -- Define main engine action
  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine..."
        -- Create window
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
        modify $ \s → s { graphicsState = (graphicsState s) {
                            glfwWindow = Just window } }

        -- Setup input callbacks
        let Window glfwWin = window
        liftIO $ setupCallbacks glfwWin (lifecycleRef env') (inputQueue env')

        -- Initialize Vulkan
        _ ← initializeVulkan window

        -- Run main loop
        mainLoop

        -- Shutdown
        shutdownEngine window unitThreadState worldThreadState
                              inputThreadState luaThreadState
        logDebugM CatSystem "Engine shutdown complete."

  -- Run engine
  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread inputThreadState
        shutdownThread luaThreadState
        shutdownThread worldThreadState
    Right _ → pure ()

-- | Run engine in headless mode (no window, no GPU)
--   Starts Lua, world, and unit threads. Debug console on configurable port.
--   Useful for automated testing, CI, and scripted world generation.
runHeadless ∷ Maybe Int → IO ()
runHeadless mPort = do
  EngineInitResult env envVar stateVar ← initializeEngineHeadless

  -- Apply port override if given
  let env' = case mPort of
        Just p  → env { engineConfig = (engineConfig env) { ecDebugPort = p } }
        Nothing → env

  -- No input thread needed — no window to receive events
  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine (headless)..."
        headlessLoop
        -- Shutdown threads
        logInfoM CatSystem "Headless engine shutting down..."
        liftIO $ shutdownThread unitThreadState
        liftIO $ shutdownThread worldThreadState
        liftIO $ shutdownThread luaThreadState
        -- Shut down logger
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped
        logDebugM CatSystem "Headless engine shutdown complete."

  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread luaThreadState
        shutdownThread worldThreadState
    Right _ → pure ()
