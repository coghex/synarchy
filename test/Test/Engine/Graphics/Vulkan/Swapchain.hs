module Test.Engine.Graphics.Vulkan.Swapchain (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (runInBoundThread, forkOS, putMVar, takeMVar, newEmptyMVar)
import Control.Exception (displayException)
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import System.Info ( os )
import System.Exit ( exitFailure )
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Core.Error.Exception
import Engine.Concurrent.Var
import Engine.Graphics.Types
import Engine.Graphics.Window.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Engine.Graphics.Window.Types
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Swapchain
import Vulkan.Zero

spec ∷ Window → Spec
spec window = before initTestEnv $ do
    it "creates and queries swapchain support" $ \env → do
      result ← runTest env $ do
        (inst, _) ← createVulkanInstance GraphicsConfig
          { gcAppName = T.pack "Test"
          , gcWidth = 1
          , gcHeight = 1
          , gcDebugMode = True
          , gcMaxFrames = 2
          }
        surface ← createWindowSurface window inst
        pdev ← pickPhysicalDevice inst surface
        support ← querySwapchainSupport pdev surface
        
        pure support
      
      case result of
        Left err → expectationFailure $ show err
        Right support → do
          length (formats support) `shouldSatisfy` (> 0)
          length (presentModes support) `shouldSatisfy` (> 0)

defaultState ∷ EngineState
defaultState = EngineState
  { frameCount = 0
  , engineRunning = True
  , currentTime = 0.0
  , deltaTime = 0.0
  , logFunc       = \_ _ _ _ → pure ()
  }

-- | Initialize test environment
initTestEnv ∷ IO (Var EngineEnv, Var EngineState)
initTestEnv = do
  envVar ← atomically $ newVar (undefined ∷ EngineEnv)
  stateVar ← atomically $ newVar defaultState
  liftIO $ GLFW.init
  liftIO $ do
    GLFW.windowHint $ GLFW.WindowHint'Visible False
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  pure (envVar, stateVar)

-- | Run a test with the engine monad
runTest ∷ (Var EngineEnv, Var EngineState) → EngineM ε (Either EngineException α) α → IO (Either EngineException α)
runTest (envVar, stateVar) test = 
  runEngineM test envVar stateVar return
