-- test/Test/Engine/Graphics/Window/GLFW.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Window.GLFW (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.Types (Window(..))
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.State
import Engine.Core.Base

-- | Main test specification for GLFW functionality
spec ∷ EngineEnv → EngineState → Spec
spec env state = 
    case glfwWindow (graphicsState state) of
        Just win@(Window glfwWin) → do
            describe "GLFW Window" $ do
                it "has correct dimensions" $ do
                    (width, height) <- GLFW.getWindowSize glfwWin
                    width `shouldBe` 800
                    height `shouldBe` 600
        Nothing → describe "GLFW Window" $ 
            it "exists" $ expectationFailure "Window not found in state"
