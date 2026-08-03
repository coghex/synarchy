-- test/Test/Engine/Graphics/Vulkan/Device.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Engine.Graphics.Vulkan.Device where

import UPrelude
import Test.Hspec
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Vulkan.Device
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Monad
import Data.IORef (newIORef)
import Engine.Graphics.Window.Types
import Vulkan.Core10
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Engine.Graphics.Window.GLFW as GLFW
import qualified Control.Concurrent.STM as STM

-- Helper function to run engine tests
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

spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "Vulkan Device" $ do
        it "can create a Vulkan device" $ do
            runEngineTest env state $ do
                (inst, _) ← createVulkanInstance defaultGraphicsConfig InstanceForWindow
                (_, physDevs) ← enumeratePhysicalDevices inst
                liftIO $ V.length physDevs `shouldSatisfy` (> 0)
                
                let physDev = V.head physDevs
                
                -- Create a surface for device creation
                let win = case glfwWindow (graphicsState state) of
                        Just (Window w) → w
                        Nothing → error "Device spec: no GLFW window in state"
                surface ← GLFW.createWindowSurface (Window win) inst
                
                (device, _) ← createVulkanDevice inst physDev (Just surface)
                liftIO $ device `shouldSatisfy` (/= zero)
