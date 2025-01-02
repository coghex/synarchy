-- test/Test/Engine/Graphics/Vulkan/Texture.hs
module Test.Engine.Graphics.Vulkan.Texture (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Bits ((.|.))
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Image
import Engine.Graphics.Vulkan.Sampler
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
    describe "Vulkan Image" $ do
        it "creates image with correct properties" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Image Test"
                      , gcWidth     = 1
                      , gcHeight    = 1
                      , gcDebugMode = False
                      , gcMaxFrames = 2
                      }
                
                -- Create necessary Vulkan objects
                (inst, _) ← createVulkanInstance config
                surface ← createWindowSurface window inst
                pdev ← pickPhysicalDevice inst surface
                (device, _) ← createVulkanDevice inst pdev surface
                
                -- Test image creation
                let imageSize = (32, 32)  -- Test with small texture
                vulkanImage ← createVulkanImage device pdev imageSize
                    FORMAT_R8G8B8A8_SRGB
                    IMAGE_TILING_OPTIMAL
                    (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                    MEMORY_PROPERTY_DEVICE_LOCAL_BIT

                -- Create image view
                imageView ← createVulkanImageView device vulkanImage
                    FORMAT_R8G8B8A8_SRGB IMAGE_ASPECT_COLOR_BIT

                pure (vulkanImage, imageView)
            
            case result of
                Left err → expectationFailure $ show err
                Right (image, view) → do
                    viImage image `shouldSatisfy` (/= zero)
                    viMemory image `shouldSatisfy` (/= zero)
                    view `shouldSatisfy` (/= zero)

    describe "Vulkan Sampler" $ do
        it "creates sampler with anisotropy" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Sampler Test"
                      , gcWidth     = 1
                      , gcHeight    = 1
                      , gcDebugMode = False
                      , gcMaxFrames = 2
                      }
                
                -- Create necessary Vulkan objects
                (inst, _) ← createVulkanInstance config
                surface ← createWindowSurface window inst
                pdev ← pickPhysicalDevice inst surface
                (device, _) ← createVulkanDevice inst pdev surface
                
                -- Create sampler
                sampler ← createVulkanSampler device pdev
                
                -- Get physical device properties to verify anisotropy
                props ← getPhysicalDeviceProperties pdev
                
                pure (sampler, maxSamplerAnisotropy $ limits props)
            
            case result of
                Left err → expectationFailure $ show err
                Right (sampler, maxAnisotropy) → do
                    sampler `shouldSatisfy` (/= zero)
                    maxAnisotropy `shouldSatisfy` (> 0)

        it "properly cleans up resources" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Cleanup Test"
                      , gcWidth     = 1
                      , gcHeight    = 1
                      , gcDebugMode = False
                      , gcMaxFrames = 2
                      }
                
                -- Create and immediately cleanup resources
                (inst, _) ← createVulkanInstance config
                surface ← createWindowSurface window inst
                pdev ← pickPhysicalDevice inst surface
                (device, _) ← createVulkanDevice inst pdev surface
                
                -- Create image and view
                vulkanImage ← createVulkanImage device pdev (32, 32)
                    FORMAT_R8G8B8A8_SRGB
                    IMAGE_TILING_OPTIMAL
                    (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                    MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                
                imageView ← createVulkanImageView device vulkanImage
                    FORMAT_R8G8B8A8_SRGB IMAGE_ASPECT_COLOR_BIT
                
                -- Create sampler
                sampler ← createVulkanSampler device pdev
                
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
