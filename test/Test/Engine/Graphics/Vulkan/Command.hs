module Test.Engine.Graphics.Vulkan.Command (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Core.Error.Exception
import Engine.Concurrent.Var
import Engine.Graphics.Types
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Command
import qualified Data.Text as T
import Vulkan.Core10

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    it "creates command pool and allocates command buffers" $ \env → do
      result ← runTest env $ do
        -- Create test configuration
        let config = GraphicsConfig
              { gcAppName   = T.pack "Command Test"
              , gcWidth     = 1
              , gcHeight    = 1
              , gcDebugMode = False
              , gcMaxFrames = 2
              }
        
        -- Initialize Vulkan instance and device
        (inst, _) ← createVulkanInstance config
        surface ← createWindowSurface window inst
        pdev ← pickPhysicalDevice inst surface
        (device, queues) ← createVulkanDevice inst pdev surface
        
        -- Test command pool creation
        cmdPool ← createVulkanCommandPool device queues
        
        -- Test command buffer allocation
        cmdBuffers ← allocateVulkanCommandBuffers device cmdPool (gcMaxFrames config)
        
        pure $ do
          V.length cmdBuffers `shouldBe` gcMaxFrames config
      
      case result of
        Left err → expectationFailure $ show err
        Right test → test

-- | Initialize test environment
initTestEnv ∷ IO (Var EngineEnv, Var EngineState)
initTestEnv = do
  envVar ← atomically $ newVar (undefined ∷ EngineEnv)
  stateVar ← atomically $ newVar defaultEngineState
  pure (envVar, stateVar)

defaultEngineState ∷ EngineState
defaultEngineState = EngineState
  { frameCount    = 0
  , engineRunning = True
  , currentTime   = 0.0
  , deltaTime     = 0.0
  , logFunc       = \_ _ _ _ → pure ()
  }

-- | Run a test with the engine monad
runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
  runEngineM test envVar stateVar return
