module Test.Engine.Graphics.Vulkan.Framebuffer (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Swapchain
import Engine.Graphics.Vulkan.Image
import Engine.Graphics.Vulkan.Framebuffer
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Concurrent.Var
import Engine.Core.Error.Exception
import Vulkan.Core10
import Vulkan.Zero

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    describe "Framebuffer Creation" $ do
        it "creates framebuffers for swapchain images" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Framebuffer Test"
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
                swapInfo ← createVulkanSwapchain pdev device queues surface
                renderPass ← createVulkanRenderPass device (siSwapImgFormat swapInfo)
                
                -- Create image views for swapchain images
                imageViews ← createSwapchainImageViews device (siSwapImgFormat swapInfo) (siSwapImgs swapInfo)
                
                -- Test framebuffer creation
                framebuffers ← createFramebuffers device renderPass swapInfo imageViews
                pure framebuffers
            
            case result of
                Left err → expectationFailure $ show err
                Right framebuffers → do
                    V.length framebuffers `shouldBe` V.length (siSwapImgs swapInfo)
                    V.all (/= zero) framebuffers `shouldBe` True

-- Helper functions

defaultTestConfig ∷ GraphicsConfig
defaultTestConfig = GraphicsConfig
    { gcAppName   = T.pack "Framebuffer Test"
    , gcWidth     = 1
    , gcHeight    = 1
    , gcDebugMode = False
    , gcMaxFrames = 2
    }

-- Modify initializeVulkan to return queue information
initializeVulkan ∷ GraphicsConfig → Window 
                 → EngineM ε σ (Device, PhysicalDevice, DevQueues)
initializeVulkan config window = do
    (inst, _) ← createVulkanInstance config
    surface ← createWindowSurface window inst
    pdev ← pickPhysicalDevice inst surface
    (device, queues) ← createVulkanDevice inst pdev surface
    pure (device, pdev, queues)

-- Test environment setup (same as before)
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
    , textureState  = (TexturePoolState zero zero, V.empty)
    , descriptorState = Nothing
    }

-- | Run a test with the engine monad
runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
    runEngineM test envVar stateVar return
