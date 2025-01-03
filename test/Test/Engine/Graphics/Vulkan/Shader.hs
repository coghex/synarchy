module Test.Engine.Graphics.Vulkan.Shader (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Shader
import Engine.Graphics.Vulkan.ShaderCode
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Concurrent.Var
import Engine.Core.Error.Exception
import Vulkan.Core10
import Vulkan.CStruct.Extends
import Vulkan.Zero

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    describe "Vulkan Shader" $ do
        it "compiles vertex shader successfully" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Shader Test"
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
                
                -- Create shader module from vertex shader code
                vertModule ← createShaderModule device zero 
                    { code = vertexShaderCode 
                    } Nothing

                pure vertModule
            
            case result of
                Left err → expectationFailure $ show err
                Right vertModule → vertModule `shouldSatisfy` (/= zero)

        it "compiles fragment shader successfully" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Shader Test"
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
                
                -- Create shader module from fragment shader code
                fragModule ← createShaderModule device zero 
                    { code = fragmentShaderCode 
                    } Nothing

                pure fragModule
            
            case result of
                Left err → expectationFailure $ show err
                Right fragModule → fragModule `shouldSatisfy` (/= zero)

        it "creates shader stages with proper configuration" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Shader Test"
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
                
                -- Create shader stages
                shaderStages ← createVulkanShaderStages device

                pure shaderStages
            
            case result of
                Left err → expectationFailure $ show err
                Right stages → do
                    -- Should have exactly 2 shader stages (vertex and fragment)
                    V.length stages `shouldBe` 2
                    -- Check stage configuration
                    let vertStage = V.head stages
                        fragStage = V.last stages
                    -- Verify shader stage flags
                    stageFromStruct vertStage `shouldBe` SHADER_STAGE_VERTEX_BIT
                    stageFromStruct fragStage `shouldBe` SHADER_STAGE_FRAGMENT_BIT
                    -- Verify entry point names
                    nameFromStruct vertStage `shouldBe` "main"
                    nameFromStruct fragStage `shouldBe` "main"
        it "shader code is non-empty" $ \_ → do
            BS.length vertexShaderCode `shouldSatisfy` (> 0)
            BS.length fragmentShaderCode `shouldSatisfy` (> 0)

-- Helper functions for the tests
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

runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
    runEngineM test envVar stateVar return

-- Helper functions to extract info from shader stage structs
stageFromStruct ∷ SomeStruct PipelineShaderStageCreateInfo → ShaderStageFlagBits
stageFromStruct (SomeStruct (info ∷ PipelineShaderStageCreateInfo a)) = 
    let PipelineShaderStageCreateInfo{..} = info
    in stage

nameFromStruct ∷ SomeStruct PipelineShaderStageCreateInfo → String
nameFromStruct (SomeStruct (info ∷ PipelineShaderStageCreateInfo a)) = 
    let PipelineShaderStageCreateInfo{..} = info
    in BSC.unpack name

-- Helper functions to check shader code content
containsAttribute ∷ String → BS.ByteString → Bool
containsAttribute attr code = 
    BS.isInfixOf (BSC.pack attr) code

containsBinding ∷ String → BS.ByteString → Bool
containsBinding binding code = 
    BS.isInfixOf (BSC.pack binding) code
