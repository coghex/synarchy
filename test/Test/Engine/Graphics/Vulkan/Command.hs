-- In test/Test/Engine/Graphics/Vulkan/Command.hs
module Test.Engine.Graphics.Vulkan.Command (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Concurrent.Var
import Engine.Core.Error.Exception
import Vulkan.Zero

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    it "creates command pool and buffers with correct count" $ \env → do
      result ← runTest env $ do
        let config = GraphicsConfig
              { gcAppName   = T.pack "Command Test"
              , gcWidth     = 1
              , gcHeight    = 1
              , gcDebugMode = False
              , gcMaxFrames = 2
              }
        -- Create necessary Vulkan objects
        (inst, _) ← createVulkanInstance config
        surface ← createWindowSurface window inst
        pdev ← pickPhysicalDevice inst surface
        (device, queues) ← createVulkanDevice inst pdev surface
        
        -- Create command collection
        cmdCollection ← createVulkanCommandCollection device queues 2
        
        pure $ do
          -- Test command pool creation
          vccCommandPool cmdCollection `shouldSatisfy` (/= zero)
          -- Test command buffer count
          length (vccCommandBuffers cmdCollection) `shouldBe` 2
          -- Test that command buffers are valid
          all (/= zero) (vccCommandBuffers cmdCollection) `shouldBe` True
      
      case result of
        Left err → expectationFailure $ show err
        Right test → test

    it "successfully records command buffers" $ \env → do
      result ← runTest env $ do
        let config = GraphicsConfig
              { gcAppName   = T.pack "Command Test"
              , gcWidth     = 1
              , gcHeight    = 1
              , gcDebugMode = False
              , gcMaxFrames = 2
              }
        -- Create necessary Vulkan objects
        (inst, _) ← createVulkanInstance config
        surface ← createWindowSurface window inst
        pdev ← pickPhysicalDevice inst surface
        (device, queues) ← createVulkanDevice inst pdev surface
        
        -- Create command collection
        cmdCollection ← createVulkanCommandCollection device queues 2
        
        -- Test command buffer recording
        let cmdBuf = V.head $ vccCommandBuffers cmdCollection
        beginVulkanCommandBuffer cmdBuf
        endVulkanCommandBuffer cmdBuf

        pure ()
        
      case result of
        Left err → expectationFailure $ show err
        Right _ → pure () -- Success if we get here

-- | Initialize test environment (following your convention)
initTestEnv ∷ IO (Var EngineEnv, Var EngineState)
initTestEnv = do
  envVar ← atomically $ newVar (undefined ∷ EngineEnv)
  stateVar ← atomically $ newVar defaultEngineState
  pure (envVar, stateVar)

defaultEngineState ∷ EngineState
defaultEngineState = EngineState
  { frameCount      = 0
  , engineRunning   = True
  , currentTime     = 0.0
  , deltaTime       = 0.0
  , logFunc         = \_ _ _ _ → pure ()  -- Null logger for tests
  , textureState    = (TexturePoolState zero zero, V.empty)
  , descriptorState = Nothing
  }

-- | Run a test with the engine monad (following your convention)
runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
  runEngineM test envVar stateVar return
