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
import Engine.Core.Base
import Engine.Core.Monad
import Engine.Core.Var
import Engine.Graphics.Window.Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (catchError)
import Vulkan.Core10
import Vulkan.Zero
import qualified Data.Vector as V
import qualified Engine.Graphics.Window.GLFW as GLFW

-- Helper function to run engine tests
runEngineTest ∷ ∀ ε α. EngineEnv → EngineState → EngineM ε EngineState α → IO α
runEngineTest env state action = do
    stateVar ← atomically $ newVar state
    envVar ← atomically $ newVar env
    mvar ← atomically $ newVar Nothing
    
    let cont result = case result of
            Right v → do
                atomically $ writeVar mvar (Just v)
                pure state
            Left err → error $ "Engine error: " ⧺ show err
    
    _ ← unEngineM action envVar stateVar cont
    result ← atomically $ readVar mvar
    case result of
        Just v → pure v
        Nothing → error "No result produced"

spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "Vulkan Device" $ do
        it "can create a Vulkan device" $ do
            runEngineTest env state $ do
                (inst, _) ← createVulkanInstance defaultGraphicsConfig
                (_, physDevs) ← enumeratePhysicalDevices inst
                liftIO $ V.length physDevs `shouldSatisfy` (> 0)
                
                let physDev = V.head physDevs
                
                -- Create a surface for device creation
                let (Just (Window win)) = glfwWindow (graphicsState state)
                surface ← GLFW.createWindowSurface (Window win) inst
                
                (device, _) ← createVulkanDevice inst physDev surface
                liftIO $ device `shouldSatisfy` (/= zero)
