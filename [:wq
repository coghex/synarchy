-- test/Test/Engine/Graphics/Vulkan/Vertex.hs
module Test.Engine.Graphics.Vulkan.Vertex (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import Foreign.Storable (sizeOf)
import Engine.Graphics.Vulkan.Vertex
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types.Texture
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero
import Engine.Graphics.Vulkan.Instance (createVulkanInstance)
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Window.GLFW (createWindowSurface)
import Engine.Graphics.Window.Types
import Engine.Core.Error.Exception
import Engine.Concurrent.Var

spec ∷ Window → Spec
spec window = do
    pureTests
    vulkanTests window

pureTests ∷ Spec
pureTests = describe "Vertex Pure Tests" $ do
    it "has correct memory layout" $ do
        -- Create sample instances instead of using undefined
        let vec2Sample = Vec2 0 0
            vec4Sample = Vec4 0 0 0 0
            vertexSample = Vertex (Vec2 0 0) (Vec2 0 0) (Vec4 0 0 0 0)
        
        sizeOf vec2Sample `shouldBe` 8    -- 2 floats * 4 bytes
        sizeOf vec4Sample `shouldBe` 16   -- 4 floats * 4 bytes
        sizeOf vertexSample `shouldBe` 32 -- (2 * Vec2 + Vec4) * 4 bytes

    it "generates correct vertex descriptions" $ do
        let bindingDesc = getVertexBindingDescription
            attrDescs = getVertexAttributeDescriptions
        
        -- Test binding description using pattern matching
        let Vk.VertexInputBindingDescription{..} = bindingDesc
        binding `shouldBe` 0
        stride `shouldBe` 32  -- size of Vertex
        
        -- Test attribute descriptions
        V.length attrDescs `shouldBe` 3
        
        -- Position attribute
        let Vk.VertexInputAttributeDescription{..} = attrDescs V.! 0
        location `shouldBe` 0
        binding `shouldBe` 0
        format `shouldBe` Vk.FORMAT_R32G32_SFLOAT
        offset `shouldBe` 0
        
        -- TexCoord attribute
        let Vk.VertexInputAttributeDescription{..} = attrDescs V.! 1
        location `shouldBe` 1
        binding `shouldBe` 0
        format `shouldBe` Vk.FORMAT_R32G32_SFLOAT
        offset `shouldBe` 8
        
        -- Color attribute
        let Vk.VertexInputAttributeDescription{..} = attrDescs V.! 2
        location `shouldBe` 2
        binding `shouldBe` 0
        format `shouldBe` Vk.FORMAT_R32G32B32A32_SFLOAT
        offset `shouldBe` 16

    it "creates quad vertices with correct data" $ do
        length quadVertices `shouldBe` 6  -- 2 triangles
        
        -- Test first vertex
        let firstVertex = head quadVertices
        pos firstVertex `shouldBe` Vec2 (-0.5) (-0.5)
        tex firstVertex `shouldBe` Vec2 0 0
        color firstVertex `shouldBe` Vec4 1 0 0 1

vulkanTests ∷ Window → Spec
vulkanTests window = describe "Vertex Vulkan Tests" $ do
    it "creates vertex buffer successfully" $ do
        env ← initTestEnv
        result ← runTest env $ do
            let config = defaultTestConfig
            (device, pdev, queues) ← initializeVulkan config window
            cmdPool ← createVulkanCommandPool device queues
            
            -- Test vertex buffer creation
            (vBuffer, vMemory) ← createVertexBuffer 
                device pdev (graphicsQueue queues) cmdPool
            
            pure (vBuffer, vMemory)
        
        case result of
            Left err → expectationFailure $ show err
            Right (buffer, memory) → do
                buffer `shouldSatisfy` (/= zero)
                memory `shouldSatisfy` (/= zero)

-- Helper functions remain the same...
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
    , vertexBuffer    = Nothing
    }

defaultTestConfig ∷ GraphicsConfig
defaultTestConfig = GraphicsConfig
    { gcAppName   = "Vertex Test"
    , gcWidth     = 1
    , gcHeight    = 1
    , gcDebugMode = False
    , gcMaxFrames = 2
    }

initializeVulkan ∷ GraphicsConfig → Window 
                 → EngineM ε σ (Vk.Device, Vk.PhysicalDevice, DevQueues)
initializeVulkan config window = do
    (inst, _) ← createVulkanInstance config
    surface ← createWindowSurface window inst
    pdev ← pickPhysicalDevice inst surface
    (device, queues) ← createVulkanDevice inst pdev surface
    pure (device, pdev, queues)

runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
    runEngineM test envVar stateVar return
