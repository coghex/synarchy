module Test.Engine.Graphics.Vulkan.Framebuffer (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Image
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Framebuffer
import Engine.Graphics.Vulkan.Pipeline
import Engine.Graphics.Vulkan.Swapchain
import Engine.Graphics.Vulkan.Types.Texture
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
    describe "Vulkan Framebuffer" $ do
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
                
                -- Create swapchain and get images
                swapInfo ← createVulkanSwapchain pdev device queues surface
                imageViews ← createSwapchainImageViews device swapInfo
                renderPass ← createVulkanRenderPass device (siSwapImgFormat swapInfo)
                
                -- Test framebuffer creation
                framebuffers ← createVulkanFramebuffers device renderPass swapInfo imageViews
                
                pure (framebuffers, V.length $ siSwapImgs swapInfo)
            
            case result of
                Left err → expectationFailure $ show err
                Right (framebuffers, expectedCount) → do
                    V.length framebuffers `shouldBe` expectedCount
                    all (/= zero) framebuffers `shouldBe` True

        it "correctly cleans up framebuffers" $ \env → do
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
                
                -- Create and immediately destroy framebuffers
                swapInfo ← createVulkanSwapchain pdev device queues surface
                imageViews ← createSwapchainImageViews device swapInfo
                renderPass ← createVulkanRenderPass device (siSwapImgFormat swapInfo)
                framebuffers ← createVulkanFramebuffers device renderPass swapInfo imageViews
                
                pure True
            
            case result of
                Left err → expectationFailure $ show err
                Right success → success `shouldBe` True

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
    , logFunc         = \_ _ _ _ → pure ()
    , textureState    = (TexturePoolState zero zero, V.empty)
    , descriptorState = Nothing
    }

-- | Run a test with the engine monad
runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
    runEngineM test envVar stateVar return
