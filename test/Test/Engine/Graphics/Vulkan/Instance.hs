-- test/Test/Engine/Graphics/Vulkan/Instance.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Vulkan.Instance (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import Engine.Core.State
import Engine.Core.Monad
import Engine.Core.Var
import Data.IORef (newIORef)
import Engine.Graphics.Base
import Engine.Graphics.Vulkan.Instance
import Vulkan.Core10
import Vulkan.Extensions.VK_KHR_portability_enumeration
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2

-- | Main test specification for Vulkan Instance functionality
spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "Vulkan Instance" $ do
        it "can enumerate available extensions" $ do
            runEngineTest env state $ do
                exts <- getAvailableExtensions
                liftIO $ do
                    exts `shouldSatisfy` not . null
                    exts `shouldSatisfy` hasRequiredExtensions

        it "can create instance with debug mode disabled" $ do
            runEngineTest env state $ do
                let config = defaultGraphicsConfig 
                        { gcDebugMode = False
                        , gcAppName = "VulkanTest" }
                (inst, dbgMessenger) <- createVulkanInstance config InstanceForWindow
                liftIO $ do
                    instanceHandle inst `shouldNotBe` nullPtr
                    dbgMessenger `shouldBe` Nothing

        it "can create instance with debug mode enabled" $ do
            runEngineTest env state $ do
                let config = defaultGraphicsConfig 
                        { gcDebugMode = True
                        , gcAppName = "VulkanTest" }
                (inst, dbgMessenger) <- createVulkanInstance config InstanceForWindow
                liftIO $ do
                    instanceHandle inst `shouldNotBe` nullPtr
                    dbgMessenger `shouldSatisfy` isJust

        it "includes required macOS extensions" $ do
            runEngineTest env state $ do
                exts <- getAvailableExtensions
                liftIO $ do
                    exts `shouldSatisfy` 
                        hasExtension KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
                    exts `shouldSatisfy` 
                        hasExtension KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME

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
        runEngineTest ∷ ∀ ε α. EngineEnv → EngineState → EngineM ε EngineState α → IO α
        runEngineTest env state action = do
            stateRef ← newIORef state
            let env' = env { engineStateRef = stateRef }
            mvar ← atomically $ newVar Nothing
            
            let cont result = case result of
                    Right v → do
                        atomically $ writeVar mvar (Just v)
                        pure state
                    Left err → error $ "Engine error: " ⧺ show err
            
            _ ← unEngineM action env' cont
            result ← atomically $ readVar mvar
            case result of
                Just v → pure v
                Nothing → error "No result produced"

        hasRequiredExtensions ∷ [BS.ByteString] → Bool
        hasRequiredExtensions exts = all (`elem` exts)
            [ KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
            , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ]

        hasExtension ∷ BS.ByteString → [BS.ByteString] → Bool
        hasExtension ext = elem ext

-- | Default graphics configuration for testing
defaultGraphicsConfig ∷ GraphicsConfig
defaultGraphicsConfig = GraphicsConfig
    { gcDebugMode = False
    , gcAppName = "VulkanTest"
    , gcWidth = 800
    , gcHeight = 600
    , gcMaxFrames = 2
    }
