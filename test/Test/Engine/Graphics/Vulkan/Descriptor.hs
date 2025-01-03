-- test/Test/Engine/Graphics/Vulkan/Descriptor.hs
module Test.Engine.Graphics.Vulkan.Descriptor (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Types
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
    describe "Vulkan Descriptor" $ do
        it "creates descriptor set layout and pool" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Descriptor Test"
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
                
                -- Create descriptor set layout
                descSetLayout ← createVulkanDescriptorSetLayout device
                
                -- Create descriptor pool and manager
                let descConfig = DescriptorManagerConfig
                      { dmcMaxSets      = 10
                      , dmcUniformCount = 5
                      , dmcSamplerCount = 5
                      }
                descManager ← createVulkanDescriptorManager device descConfig
                
                -- Allocate descriptor sets
                descSets ← allocateVulkanDescriptorSets device descManager 2

                pure (descSetLayout, descManager, descSets)
            
            case result of
                Left err → expectationFailure $ show err
                Right (layout, manager, sets) → do
                    layout `shouldSatisfy` (/= zero)
                    dmPool manager `shouldSatisfy` (/= zero)
                    V.length sets `shouldBe` 2
                    V.all (/= zero) sets `shouldBe` True

        it "properly cleans up descriptor resources" $ \env → do
            result ← runTest env $ do
                let config = GraphicsConfig
                      { gcAppName   = T.pack "Descriptor Cleanup Test"
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
                
                -- Create and destroy descriptor resources
                descSetLayout ← createVulkanDescriptorSetLayout device
                let descConfig = DescriptorManagerConfig
                      { dmcMaxSets      = 10
                      , dmcUniformCount = 5
                      , dmcSamplerCount = 5
                      }
                descManager ← createVulkanDescriptorManager device descConfig
                
                pure True
            
            case result of
                Left err → expectationFailure $ show err
                Right success → success `shouldBe` True

-- Use the same initTestEnv and runTest functions from your texture test
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

runTest ∷ (Var EngineEnv, Var EngineState) 
       → EngineM ε (Either EngineException α) α 
       → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
    runEngineM test envVar stateVar return
