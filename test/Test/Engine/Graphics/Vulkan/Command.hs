-- test/Test/Engine/Graphics/Vulkan/Command.hs
module Test.Engine.Graphics.Vulkan.Command (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Framebuffer
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Pipeline
import Engine.Graphics.Vulkan.Swapchain
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Vertex
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Concurrent.Var
import Engine.Core.Error.Exception
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Core10

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    -- Original tests
    it "creates command pool and buffers with correct count" $ \env → do
      result ← runTest env $ do
        let config = defaultConfig
        -- Create necessary Vulkan objects
        (inst, _) ← createVulkanInstance config
        surface ← createWindowSurface window inst
        pdev ← pickPhysicalDevice inst surface
        (device, queues) ← createVulkanDevice inst pdev surface
        
        -- Create command collection
        cmdCollection ← createVulkanCommandCollection device queues 2
        
        pure $ do
          vccCommandPool cmdCollection `shouldSatisfy` (/= zero)
          length (vccCommandBuffers cmdCollection) `shouldBe` 2
          all (/= zero) (vccCommandBuffers cmdCollection) `shouldBe` True
      
      case result of
        Left err → expectationFailure $ show err
        Right test → test

    it "successfully records command buffers" $ \env → do
      result ← runTest env $ do
        let config = defaultConfig
        (inst, _) ← createVulkanInstance config
        surface ← createWindowSurface window inst
        pdev ← pickPhysicalDevice inst surface
        (device, queues) ← createVulkanDevice inst pdev surface
        
        cmdCollection ← createVulkanCommandCollection device queues 2
        let cmdBuf = V.head $ vccCommandBuffers cmdCollection
        
        beginVulkanCommandBuffer cmdBuf
        endVulkanCommandBuffer cmdBuf
        pure ()
        
      case result of
        Left err → expectationFailure $ show err
        Right _ → pure ()

    -- New draw command tests
    describe "draw command testing" $ do
        it "creates render pass successfully" $ \env → do
          result ← runTest env $ do
            let config = defaultConfig
            (inst, _) ← createVulkanInstance config
            surface ← createWindowSurface window inst
            pdev ← pickPhysicalDevice inst surface
            (device, queues) ← createVulkanDevice inst pdev surface
            
            renderPass ← createVulkanRenderPass device FORMAT_B8G8R8A8_SRGB
            pure $ renderPass `shouldSatisfy` (/= zero)
          
          case result of
            Left err → expectationFailure $ show err
            Right test → test

        it "creates pipeline successfully" $ \env → do
          result ← runTest env $ do
            let config = defaultConfig
            (inst, _) ← createVulkanInstance config
            surface ← createWindowSurface window inst
            pdev ← pickPhysicalDevice inst surface
            (device, queues) ← createVulkanDevice inst pdev surface
            
            renderPass ← createVulkanRenderPass device FORMAT_B8G8R8A8_SRGB
            descriptorLayout ← createVulkanDescriptorSetLayout device
            
            (pipeline, layout) ← createVulkanRenderPipeline 
              device renderPass (Extent2D 1 1) descriptorLayout
            pure $ do
              pipeline `shouldSatisfy` (/= zero)
              layout `shouldSatisfy` (/= zero)
          
          case result of
            Left err → expectationFailure $ show err
            Right test → test

        it "records draw commands with pipeline" $ \env → do
          result ← runTest env $ do
            let config = defaultConfig
            (inst, _) ← createVulkanInstance config
            surface ← createWindowSurface window inst
            pdev ← pickPhysicalDevice inst surface
            (device, queues) ← createVulkanDevice inst pdev surface
            
            renderPass ← createVulkanRenderPass device FORMAT_B8G8R8A8_SRGB
            descriptorLayout ← createVulkanDescriptorSetLayout device
            (pipeline, _) ← createVulkanRenderPipeline 
              device renderPass (Extent2D 1 1) descriptorLayout
            
            cmdCollection ← createVulkanCommandCollection device queues 2
            let cmdBuf = V.head $ vccCommandBuffers cmdCollection
            
            -- Create a basic framebuffer
            swapInfo ← createVulkanSwapchain pdev device queues surface
            imageViews ← createSwapchainImageViews device swapInfo
            framebuffers ← createVulkanFramebuffers 
              device renderPass swapInfo imageViews
            
            beginVulkanCommandBuffer cmdBuf
            
            let renderPassInfo = zero
                  { renderPass = renderPass
                  , framebuffer = V.head framebuffers
                  , renderArea = zero
                      { offset = zero
                      , extent = Extent2D 1 1
                      }
                  }
            
            cmdBeginRenderPass cmdBuf renderPassInfo SUBPASS_CONTENTS_INLINE
            cmdBindPipeline cmdBuf PIPELINE_BIND_POINT_GRAPHICS pipeline
            cmdEndRenderPass cmdBuf
            endVulkanCommandBuffer cmdBuf
            
            pure ()
          
          case result of
            Left err → expectationFailure $ show err
            Right _ → pure ()

        it "records command buffer with vertex data" $ \env → do
            result ← runTest env $ do
                let config = defaultConfig
                (device, pdev, queues) ← initializeVulkan config window
                
                -- Create vertex buffer
                cmdPool ← createVulkanCommandPool device queues
                (vBuffer, vMemory) ← createVertexBuffer 
                    device pdev (graphicsQueue queues) cmdPool
                
                -- Create command buffer
                cmdBuf ← allocateVulkanCommandBuffer device cmdPool
                
                -- Record commands
                beginVulkanCommandBuffer cmdBuf
                cmdBindVertexBuffers cmdBuf 0 
                    (V.singleton vBuffer) 
                    (V.singleton 0)
                cmdDraw cmdBuf 6 1 0 0
                endVulkanCommandBuffer cmdBuf
                
                pure True
            
            case result of
                Left err → expectationFailure $ show err
                Right success → success `shouldBe` True

-- Helper functions
defaultConfig ∷ GraphicsConfig
defaultConfig = GraphicsConfig
    { gcAppName   = T.pack "Command Test"
    , gcWidth     = 1
    , gcHeight    = 1
    , gcDebugMode = False
    , gcMaxFrames = 2
    }

initializeVulkan ∷ GraphicsConfig → Window 
                 → EngineM ε σ (Device, PhysicalDevice, DevQueues)
initializeVulkan config window = do
    (inst, _) ← createVulkanInstance config
    surface ← createWindowSurface window inst
    pdev ← pickPhysicalDevice inst surface
    (device, queues) ← createVulkanDevice inst pdev surface
    pure (device, pdev, queues)

-- | Initialize test environment
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

-- | Run a test with the engine monad
runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
  runEngineM test envVar stateVar return
