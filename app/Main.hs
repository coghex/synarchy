{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException)
import System.Environment (setEnv)
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), graphicsState, glfwWindow)
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Error.Exception
import Engine.Graphics.Config (loadVideoConfig)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Shutdown (shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Backend (startLuaThread)

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

  -- Initialize engine
  EngineInitResult env envVar stateVar ← initializeEngine
  
  -- Load video config for window creation
  videoConfig ← loadVideoConfig "config/video.yaml"
  
  -- Fork worker threads
  inputThreadState ← startInputThread env
  luaThreadState   ← startLuaThread env
  
  -- Define main engine action
  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        -- Create window
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
        modify $ \s → s { graphicsState = (graphicsState s) {
                            glfwWindow = Just window } }
        
        -- Setup input callbacks
        let Window glfwWin = window
        liftIO $ setupCallbacks glfwWin (lifecycleRef env) (inputQueue env)
        
        -- Initialize Vulkan
        _ ← initializeVulkan window
        
        -- Run main loop
        mainLoop
        
        -- Shutdown
        shutdownEngine window inputThreadState luaThreadState
        logDebug "Engine shutdown complete."
  
  -- Run engine
  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread inputThreadState
        shutdownThread luaThreadState
    Right _ → pure ()
