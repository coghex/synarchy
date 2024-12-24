module Main where

import UPrelude
import Control.Exception (displayException)
import Control.Monad (void)
import qualified Data.Text as T
import System.Exit ( exitFailure )
import Engine.Core.Monad (runEngineM, EngineM')
import Engine.Core.Types
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Concurrent.Var
import Engine.Graphics.Types
import Engine.Graphics.Window.GLFW (initializeGLFW, terminateGLFW
                                   , createWindow, destroyWindow, createWindowSurface)
import Engine.Graphics.Window.Types (WindowConfig(..))
import Engine.Graphics.Vulkan.Instance (createVulkanInstance)
import Engine.Graphics.Vulkan.Device (createVulkanDevice, pickPhysicalDevice, DeviceQueues(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Vulkan.Core10
import Control.Monad.IO.Class (liftIO)

defaultEngineConfig ∷ EngineConfig
defaultEngineConfig = EngineConfig 
  { windowWidth  = 800
  , windowHeight = 600
  , enableVSync  = True
  , enableDebug  = True
  }

defaultGraphicsConfig ∷ GraphicsConfig
defaultGraphicsConfig = GraphicsConfig 
  { gcAppName   = T.pack "Vulkan Device Test"
  , gcDebugMode = True
  , gcWidth     = 800
  , gcHeight    = 600
  }

defaultWindowConfig ∷ WindowConfig
defaultWindowConfig = WindowConfig 
  { wcWidth     = 800
  , wcHeight    = 600
  , wcTitle     = T.pack "Vulkan Test"
  , wcResizable = True
  }

defaultEngineState ∷ EngineState
defaultEngineState = EngineState
  { frameCount    = 0
  , engineRunning = True
  , currentTime   = 0.0
  , deltaTime     = 0.0
  }

main ∷ IO ()
main = do
  putStrLn "Starting Vulkan device test..."
  
  -- Initialize engine environment and state
  envVar ←   atomically $ newVar (undefined ∷ EngineEnv)
  stateVar ← atomically $ newVar defaultEngineState
  
  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        -- Initialize GLFW
        initializeGLFW
        
        -- Create window using the correct WindowConfig structure
        window ← GLFW.createWindow defaultWindowConfig
        
        -- Create Vulkan instance
        (vkInstance, _debugMessenger) ← createVulkanInstance defaultGraphicsConfig
        
        -- Create surface
        surface ← createWindowSurface window vkInstance
        
        -- Select physical device and create logical device
        physicalDevice ← pickPhysicalDevice vkInstance surface
        (device, queues) ← createVulkanDevice vkInstance physicalDevice surface
        
        -- Print some info about the device
        props ← liftIO $ getPhysicalDeviceProperties physicalDevice
        liftIO $ do
          putStrLn $ "Selected device: " ++ show (deviceName props)
          --putStrLn $ "Graphics queue: " ++ show (graphicsQueue queues)
          --putStrLn $ "Present queue: " ++ show (presentQueue queues)
        
        -- Cleanup
        GLFW.destroyWindow window
        terminateGLFW
        
  
  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → putStrLn $ displayException err
    Right _  → putStrLn "Test completed successfully!"

checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus (Right ()) = pure (Right ())
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure
