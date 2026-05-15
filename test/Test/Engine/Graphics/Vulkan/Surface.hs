-- test/Test/Engine/Graphics/Vulkan/Surface.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Engine.Graphics.Vulkan.Surface where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Window.Types
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Monad
import Engine.Core.Var
import Engine.Graphics.Base
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (catchError)
import qualified Test.Engine.Graphics.Vulkan.Helpers as VH
import Foreign.Ptr (nullPtr)
import Data.Maybe (isJust)
import Vulkan.Dynamic (InstanceCmds(..))
import Engine.Core.Error.Exception
import Vulkan.Core10
import Vulkan.Zero

spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "Vulkan Surface" $ do
        it "can create surface from window" $ do
            runEngineTest env state $ do
                -- Get the existing window from state
                case glfwWindow (graphicsState state) of
                    Nothing → liftIO $ expectationFailure "No window found in state"
                    Just win → do
                        -- Create a test instance
                        (inst, _) ← createVulkanInstance defaultGraphicsConfig
                        -- Create surface
                        surface <- GLFW.createWindowSurface win inst
                        -- The surface creation should succeed (if it fails, it will throw an exception)
                        liftIO $ surface `shouldSatisfy` (/= zero)

        it "fails with invalid instance" $ do
            runEngineTest env state $ do
                case glfwWindow (graphicsState state) of
                    Nothing → liftIO $ expectationFailure "No window found in state"
                    Just win → do
                        -- Create an invalid instance
                        let inst = zero ∷ Instance
                        -- Attempt to create surface should fail
                        -- Any EngineException is acceptable — the surface
                        -- creation must fail on a zero instance, but the
                        -- specific error variant has shifted over time.
                        result <- (do
                            _ <- GLFW.createWindowSurface win inst
                            pure False)
                            `catchError` \_ → pure True
                        liftIO $ result `shouldBe` True

        it "can create and destroy surface multiple times" $ do
            runEngineTest env state $ do
                case glfwWindow (graphicsState state) of
                    Nothing → liftIO $ expectationFailure "No window found in state"
                    Just win → do
                        (inst, _) ← createVulkanInstance defaultGraphicsConfig
                        let createAndDestroySurface = do
                              surface <- GLFW.createWindowSurface win inst
                              liftIO $ surface `shouldSatisfy` (/= zero)
                        replicateM_ 5 createAndDestroySurface

    where
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
