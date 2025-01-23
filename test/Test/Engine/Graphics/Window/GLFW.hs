{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Window.GLFW (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Graphics.Base
import Engine.Core.State
import Engine.Core.Monad
import Engine.Core.Base
import Engine.Core.Var
import qualified Graphics.UI.GLFW as GLFW
import Control.Exception (SomeException, catch)

-- | Main test specification for GLFW functionality
spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "GLFW Window" $ do
        it "creates a window with specified dimensions" $ do
            let config = WindowConfig 
                    { wcWidth = 800
                    , wcHeight = 600
                    , wcTitle = "Test Window"
                    , wcResizable = True
                    }
            window <- runEngineTest env state $ createWindow config
            case window of
                Window win -> do
                    (width, height) <- GLFW.getWindowSize win
                    width `shouldBe` 800
                    height `shouldBe` 600

        it "supports Vulkan" $ do
            result <- runEngineTest env state vulkanSupported
            result `shouldBe` True

        it "returns required Vulkan extensions" $ do
            exts <- runEngineTest env state getRequiredInstanceExtensions
            length exts `shouldSatisfy` (> 0)

    where
        runEngineTest ∷ ∀ ε α. EngineEnv → EngineState → EngineM ε EngineState α → IO α
        runEngineTest env state action = do
            stateVar ← atomically $ newVar state
            envVar ← atomically $ newVar env
            let cont result = case result of
                    Right v → pure v
                    Left err → error $ "Engine error: " ⧺ show err
            unEngineM action envVar stateVar cont
              `catch` \(e ∷ SomeException) → do
                liftIO $ putStrLn $ "Exception caught: " ⧺ show e
                pure state
