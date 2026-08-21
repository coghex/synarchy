-- test/Test/Engine/Graphics/Vulkan/Surface.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Engine.Graphics.Vulkan.Surface where

import UPrelude
import Test.Hspec
import Engine.Graphics.Vulkan.Instance
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Monad
import Test.Engine.Graphics.Vulkan.Helpers (withTestInstance)
import Data.IORef (newIORef)
import Vulkan.Core10
import Vulkan.Zero
import qualified Control.Concurrent.STM as STM

spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "Vulkan Surface" $ do
        it "can create surface from window" $ do
            runEngineTest env state $ do
                -- Get the existing window from state
                case glfwWindow (graphicsState state) of
                    Nothing → liftIO $ expectationFailure "No window found in state"
                    Just win →
                        -- Create a test instance, destroyed when this
                        -- scope exits. The surface's own allocResource
                        -- nests inside, so it is destroyed first.
                        withTestInstance defaultGraphicsConfig InstanceForWindow $ \(inst, _) → do
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
                    Just win →
                        withTestInstance defaultGraphicsConfig InstanceForWindow $ \(inst, _) → do
                            let createAndDestroySurface = do
                                  surface <- GLFW.createWindowSurface win inst
                                  liftIO $ surface `shouldSatisfy` (/= zero)
                            replicateM_ 5 createAndDestroySurface

    where
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
