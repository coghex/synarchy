-- test/Test/Engine/Graphics/Vulkan/Instance.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Vulkan.Instance (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State
import Engine.Core.Monad
import Data.IORef (newIORef)
import Engine.Graphics.Base
import Engine.Graphics.Vulkan.Instance
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
  (pattern EXT_DEBUG_UTILS_EXTENSION_NAME)
import qualified Control.Concurrent.STM as STM

-- | Main test specification for Vulkan Instance functionality
spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "Vulkan Instance" $ do
        it "can enumerate available extensions" $ do
            runEngineTest env state $ do
                exts <- getAvailableExtensions
                liftIO $ exts `shouldSatisfy` not . null

        it "can create instance with debug mode disabled" $ do
            runEngineTest env state $ do
                let config = defaultGraphicsConfig 
                        { gcDebugMode = False
                        , gcAppName = "VulkanTest" }
                (inst, dbgMessenger) <- createVulkanInstance config InstanceForWindow
                liftIO $ do
                    instanceHandle inst `shouldNotBe` nullPtr
                    dbgMessenger `shouldBe` Nothing

        -- Production enables VK_EXT_debug_utils only when the driver
        -- offers it (#1402), so the messenger tracks observed
        -- availability rather than debug mode alone. Which extensions
        -- that decision enables from which availability is pinned
        -- without a driver in Test.Headless.Graphics.InstancePlan.
        it "creates a debug messenger in debug mode iff debug utils exist" $ do
            runEngineTest env state $ do
                let config = defaultGraphicsConfig 
                        { gcDebugMode = True
                        , gcAppName = "VulkanTest" }
                exts <- getAvailableExtensions
                (inst, dbgMessenger) <- createVulkanInstance config InstanceForWindow
                liftIO $ do
                    instanceHandle inst `shouldNotBe` nullPtr
                    isJust dbgMessenger `shouldBe`
                        (EXT_DEBUG_UTILS_EXTENSION_NAME `elem` exts)

        it "can create and destroy instance multiple times" $ do
            runEngineTest env state $ do
                let config = defaultGraphicsConfig 
                        { gcDebugMode = False
                        , gcAppName = "VulkanTest" }
                -- First creation
                (inst1, _dbg1) <- createVulkanInstance config InstanceForWindow
                liftIO $ instanceHandle inst1 `shouldNotBe` nullPtr
                -- Second creation
                (inst2, _dbg2) <- createVulkanInstance config InstanceForWindow
                liftIO $ instanceHandle inst2 `shouldNotBe` nullPtr
                pure ()

    where
        -- Helper functions
        runEngineTest ∷ ∀ α. EngineEnv → EngineState → EngineM EngineState α → IO α
        runEngineTest env state action = do
            stateRef ← newIORef state
            let env' = env { engineStateRef = stateRef }
            mvar ← STM.atomically $ STM.newTVar Nothing

            let cont result = case result of
                    Right v → do
                        STM.atomically $ STM.writeTVar mvar (Just v)
                        pure state
                    Left err → error $ "Engine error: " ⧺ show err

            _ ← unEngineM action env' cont
            result ← STM.atomically $ STM.readTVar mvar
            case result of
                Just v → pure v
                Nothing → error "No result produced"

-- | Default graphics configuration for testing
defaultGraphicsConfig ∷ GraphicsConfig
defaultGraphicsConfig = GraphicsConfig
    { gcDebugMode = False
    , gcAppName = "VulkanTest"
    , gcMaxFrames = 2
    }
