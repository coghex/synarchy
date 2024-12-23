module Main where

import UPrelude
import Control.Concurrent.STM (newTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (modify')
import qualified Data.Text as T
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Core.Error.Exception
import Engine.Concurrent.Var
import Engine.Event.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Instance
import qualified Engine.Graphics.Window.GLFW as GLFW

-- | Initial graphics configuration
initialGraphicsConfig ∷ GraphicsConfig
initialGraphicsConfig = GraphicsConfig
  { gcAppName   = "Synarchy"
  , gcWidth     = 800
  , gcHeight    = 600
  , gcDebugMode = True
  }

-- | Initial engine environment
initialEnv ∷ EngineEnv
initialEnv = EngineEnv
  { engineConfig = EngineConfig
      { windowWidth  = 800
      , windowHeight = 600
      , enableVSync  = True
      , enableDebug  = True
      }
  , vulkanInstance = undefined  -- Will be set during initialization
  , vulkanDevice   = undefined
  }

-- | Initial engine state
initialState ∷ EngineState
initialState = EngineState
  { frameCount    = 0
  , engineRunning = True
  , currentTime   = 0
  , deltaTime     = 0
  }

-- | Test Vulkan instance creation and destruction
testVulkan ∷ EngineM ε σ T.Text
testVulkan = do
  -- Create Vulkan instance
  liftIO $ putStrLn "Creating Vulkan instance..."
  instance' ← createVulkanInstance initialGraphicsConfig
  
  -- Do some basic testing
  liftIO $ putStrLn "Vulkan instance created successfully"
  
  -- Clean up
  liftIO $ putStrLn "Cleaning up Vulkan instance..."
  destroyVulkanInstance instance'
  
  pure "Vulkan test completed successfully"

-- | Run the engine monad
runTest ∷ IO ()
runTest = do
  -- Create initial TVars for environment and state
  envVar ← newTVarIO initialEnv
  stateVar ← newTVarIO initialState
  
  -- Run the Vulkan test within the EngineM monad
  result ← runEngineM
    (do
      -- Initialize GLFW (needed for Vulkan extension info)
      GLFW.initializeGLFW
      -- Run the test
      result ← testVulkan
      -- Cleanup GLFW
      GLFW.terminateGLFW
      pure result
    ) 
    envVar 
    stateVar 
    $ \case
      Left err → do
        putStrLn $ "Error: " ⧺ show err
        pure $ Left err
      Right val → do
        putStrLn $ "Success: " ⧺ show val
        pure $ Right val
  pure ()

main ∷ IO ()
main = do
  putStrLn "Starting Vulkan test..."
  runTest
  putStrLn "Test complete"
