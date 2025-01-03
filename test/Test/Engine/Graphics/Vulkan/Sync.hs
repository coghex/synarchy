-- In test/Engine/Graphics/Vulkan/SyncSpec.hs
module Test.Engine.Graphics.Vulkan.Sync (spec) where

import UPrelude
import Test.Hspec
import System.Info (os)
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Vector as V
import qualified Vulkan as Vk
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Sync
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Concurrent.Var
import Engine.Core.Error.Exception
import Vulkan.Zero

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    it "creates sync objects with correct count" $ \env → do
      result ← runTest env $ do
        let config = GraphicsConfig
              { gcAppName = "Test"
              , gcWidth = 1
              , gcHeight = 1
              , gcDebugMode = False
              , gcMaxFrames = 2
              }
        -- We'll need the device from earlier initialization steps
        (inst, _) ← createVulkanInstance config
        surface ← createWindowSurface window inst
        pdev ← pickPhysicalDevice inst surface
        (device, _) ← createVulkanDevice inst pdev surface
        syncObjs ← createSyncObjects device config
        
        pure $ do
          V.length (imageAvailableSemaphores syncObjs) `shouldBe` 2
          V.length (renderFinishedSemaphores syncObjs) `shouldBe` 2
          V.length (inFlightFences syncObjs) `shouldBe` 2
      
      case result of
        Left err → expectationFailure $ show err
        Right test → test

-- | Initialize test environment (same as in SwapchainSpec)
initTestEnv ∷ IO (Var EngineEnv, Var EngineState)
initTestEnv = do
  envVar ← atomically $ newVar (undefined ∷ EngineEnv)
  stateVar ← atomically $ newVar defaultEngineState
  -- Initialize other necessary components
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
runTest ∷ (Var EngineEnv, Var EngineState) → EngineM ε (Either EngineException α) α → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
  runEngineM test envVar stateVar return
