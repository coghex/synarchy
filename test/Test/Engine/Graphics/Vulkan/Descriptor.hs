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
    it "creates descriptor set layout with correct bindings" $ \env → do
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
        descriptorSetLayout ← createVulkanDescriptorSetLayout device

        pure descriptorSetLayout
      
      case result of
        Left err → expectationFailure $ show err
        Right layout → layout `shouldSatisfy` (/= zero)

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
