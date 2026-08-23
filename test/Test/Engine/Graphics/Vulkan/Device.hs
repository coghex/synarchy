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
import Test.Engine.Graphics.Vulkan.Helpers (withTestInstance)
import Data.IORef (newIORef)
import Engine.Graphics.Window.Types
import Vulkan.Zero
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
            runEngineTest env state $
                -- The instance scope encloses the surface and device
                -- scopes, both of which register their own destruction
                -- with allocResource, so the unwind is device, then
                -- surface, then instance.
                withTestInstance defaultGraphicsConfig InstanceForWindow $ \(inst, _) → do
                    -- The surface comes FIRST: pickPhysicalDevice rates
                    -- present support against it (findQueueFamilies asks
                    -- each queue family whether it can present to this
                    -- surface), so selecting before it exists would rate a
                    -- different question than production asks.
                    let win = case glfwWindow (graphicsState state) of
                            Just (Window w) → w
                            Nothing → error "Device spec: no GLFW window in state"
                    surface ← GLFW.createWindowSurface (Window win) inst

                    -- The selection production performs -- both boot paths
                    -- go through pickPhysicalDevice
                    -- (Engine.Graphics.Vulkan.Init), never enumeration
                    -- order. Taking the first enumerated adapter here
                    -- produced a false failure on any machine whose
                    -- first adapter lacks a graphics or present queue
                    -- family, is bindless-incapable, or lacks
                    -- VK_KHR_swapchain, while another adapter would boot
                    -- the game (#1576).
                    --
                    -- This also restores createVulkanDevice's stated
                    -- precondition: it documents that its caller only
                    -- hands it devices whose probe succeeded.
                    --
                    -- A machine with NO Vulkan device at all still fails
                    -- here with the cause named: pickPhysicalDevice throws
                    -- DeviceCreationFailed "Failed to find GPUs with
                    -- Vulkan support" on an empty inventory, which is what
                    -- the former nonempty assertion signalled. No second
                    -- enumeration is kept to reproduce it.
                    physDev ← pickPhysicalDevice inst (Just surface)

                    -- Deliberately no assertion about WHICH adapter was
                    -- chosen: vendor, device type and enumeration position
                    -- are all machine facts, and the ranking policy itself
                    -- is pinned deterministically headless by
                    -- Test.Headless.Graphics.BindlessFeatures'
                    -- "physical-device selection" examples over
                    -- scoreDevice. This spec proves only that the
                    -- policy-selected live adapter can create a device.
                    (device, _) ← createVulkanDevice inst physDev (Just surface)
                    liftIO $ device `shouldSatisfy` (/= zero)
