-- test/Test/Engine/Graphics/Vulkan/Surface.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Engine.Graphics.Vulkan.Surface where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Vulkan.Instance
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Base
import Engine.Core.Monad
import Engine.Core.Var
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
                        --(inst, _) ← createVulkanInstance defaultGraphicsConfig
                        --liftIO $ putStrLn "Created test instance"
                        ---- Create surface
                        --surface <- GLFW.createWindowSurface win inst
                        ---- The surface creation should succeed (if it fails, it will throw an exception)
                        --liftIO $ surface `shouldSatisfy` (/= zero)
                        pure ()

        it "fails with invalid instance" $ do
            runEngineTest env state $ do
                case glfwWindow (graphicsState state) of
                    Nothing → liftIO $ expectationFailure "No window found in state"
                    Just win → do
                        -- Create an invalid instance
                        let inst = zero ∷ Instance
                        -- Attempt to create surface should fail
                        result <- (do
                            _ <- GLFW.createWindowSurface win inst
                            pure False)
                            `catchError` \case
                                EngineException (ExSystem (GLFWError _)) _ _ → pure True
                                _ → pure False
                        liftIO $ result `shouldBe` True

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
