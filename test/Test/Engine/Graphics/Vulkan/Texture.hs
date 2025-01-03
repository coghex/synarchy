module Test.Engine.Graphics.Vulkan.Texture (spec) where

import UPrelude
import Test.Hspec
import Control.Monad.State (gets, modify)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Bits ((.|.))
import Data.Word (Word32)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Codec.Picture (generateImage, PixelRGBA8(..), writePng)
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Image
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Core.Resource
import Engine.Concurrent.Var
import Engine.Core.Error.Exception
import Vulkan.Core10
import Vulkan.Zero

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    describe "Texture Creation and Management" $ do
        it "creates texture descriptor set layout" $ \env → do
            result ← runTest env $ do
                let config = defaultTestConfig
                (device, _, _) ← initializeVulkan config window
                
                -- Test descriptor set layout creation
                layout ← createTextureDescriptorSetLayout device
                pure layout
            
            case result of
                Left err → expectationFailure $ show err
                Right layout → layout `shouldSatisfy` (/= zero)

        it "creates texture descriptor pool" $ \env → do
            result ← runTest env $ do
                let config = defaultTestConfig
                (device, _, _) ← initializeVulkan config window
                
                -- Test descriptor pool creation
                pool ← createTextureDescriptorPool device
                pure pool
            
            case result of
                Left err → expectationFailure $ show err
                Right pool → pool `shouldSatisfy` (/= zero)

        it "creates texture sampler" $ \env → do
            result ← runTest env $ do
                let config = defaultTestConfig
                (device, pdev, _) ← initializeVulkan config window
                
                -- Test sampler creation
                sampler ← createTextureSampler device pdev
                pure sampler
            
            case result of
                Left err → expectationFailure $ show err
                Right sampler → sampler `shouldSatisfy` (/= zero)

        it "handles complete texture loading pipeline" $ \env → do
            result ← runTest env $ do
                let config = defaultTestConfig
                (device, pdev, queues) ← initializeVulkan config window
                
                -- Create test image data
                let testImagePath = "test-texture.png"
                    imageSize = (64, 64)
                createTestImage testImagePath imageSize
                
                -- Initialize command resources
                cmdPool ← createVulkanCommandPool device queues
                
                -- Test complete texture creation pipeline
                textureData ← createTextureWithDescriptor 
                    device pdev cmdPool (graphicsQueue queues) testImagePath
                
                pure textureData
            
            case result of
                Left err → expectationFailure $ show err
                Right texData → do 
                    tdImageView texData `shouldSatisfy` (/= zero)
                    tdSampler texData `shouldSatisfy` (/= zero)
                    tdMipLevels texData `shouldSatisfy` (> 0)
                    tdDescriptorSet texData `shouldSatisfy` (/= zero)

        it "handles texture state management" $ \env → do
            result ← runTest env $ do
                let config = defaultTestConfig
                (device, pdev, _) ← initializeVulkan config window
                
                -- Create pool and layout
                pool ← createTextureDescriptorPool device
                layout ← createTextureDescriptorSetLayout device
                
                -- Create initial texture state
                let poolState = TexturePoolState pool layout
                    initialState = (poolState, V.empty)
                
                -- Update engine state
                modify $ \s → s { textureState = initialState }
                
                -- Get current state
                state ← gets textureState
                pure state
            
            case result of
                Left err → expectationFailure $ show err
                Right (poolState, textures) → do
                    tpsDescPool poolState `shouldSatisfy` (/= zero)
                    tpsLayout poolState `shouldSatisfy` (/= zero)
                    V.length textures `shouldBe` 0

        it "handles texture transitions correctly" $ \env → do
            result ← runTest env $ do
                let config = defaultTestConfig
                (device, pdev, queues) ← initializeVulkan config window
                cmdPool ← createVulkanCommandPool device queues
                
                -- Create test image
                image ← createVulkanImage device pdev (64, 64)
                    FORMAT_R8G8B8A8_SRGB
                    IMAGE_TILING_OPTIMAL
                    (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                    MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                
                -- Test layout transitions
                runCommandsOnce device cmdPool (graphicsQueue queues) $ \cmdBuf → do
                    transitionImageLayout image FORMAT_R8G8B8A8_SRGB
                        Undef_TransDst 1 cmdBuf
                    transitionImageLayout image FORMAT_R8G8B8A8_SRGB
                        TransDst_ShaderRO 1 cmdBuf
                
                pure True
            
            case result of
                Left err → expectationFailure $ show err
                Right success → success `shouldBe` True

-- Helper functions

defaultTestConfig ∷ GraphicsConfig
defaultTestConfig = GraphicsConfig
    { gcAppName   = T.pack "Texture Test"
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

defaultQueues ∷ Queue → Word32 → DevQueues
defaultQueues queue famIdx = DevQueues
    { graphicsQueue  = queue
    , presentQueue   = queue  -- Using same queue for graphics and present
    , graphicsFamIdx = famIdx
    , presentFamIdx  = famIdx
    }

createTestImage ∷ FilePath → (Int, Int) → EngineM ε σ ()
createTestImage path (width, height) = do
    let img = generateImage generatePixel width height
        generatePixel x y = PixelRGBA8 
            (fromIntegral x) 
            (fromIntegral y) 
            128 
            255
    liftIO $ writePng path img

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

runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
    runEngineM test envVar stateVar return
