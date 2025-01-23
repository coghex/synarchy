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
import Control.Monad (when)
import Data.Maybe (isJust)

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

                it "has correct framebuffer size" $ do
                    (fbWidth, fbHeight) <- GLFW.getFramebufferSize glfwWin
                    fbWidth `shouldSatisfy` (> 0)
                    fbHeight `shouldSatisfy` (> 0)

                it "supports window position queries" $ do
                    (x, y) <- GLFW.getWindowPos glfwWin
                    x `shouldSatisfy` (>= 0)
                    y `shouldSatisfy` (>= 0)

            describe "GLFW Monitor" $ do
                it "can get primary monitor" $ do
                    monitor <- GLFW.getPrimaryMonitor
                    monitor `shouldSatisfy` isJust

                it "can get monitor name" $ do
                    monitor <- GLFW.getPrimaryMonitor
                    case monitor of
                        Just m -> do
                            name <- GLFW.getMonitorName m
                            name `shouldSatisfy` not . null
                        Nothing -> expectationFailure "No primary monitor found"

            describe "GLFW Vulkan Support" $ do
                it "has Vulkan support" $ do
                    supported <- GLFW.vulkanSupported
                    supported `shouldBe` True

            describe "GLFW Time" $ do
                it "can get time" $ do
                    time <- GLFW.getTime
                    time `shouldSatisfy` isJust
                    case time of
                        Just t -> t `shouldSatisfy` (>= 0)
                        Nothing -> expectationFailure "Could not get GLFW time"

                it "can set time" $ do
                    GLFW.setTime 0
                    time <- GLFW.getTime
                    case time of
                        Just t -> t `shouldSatisfy` (>= 0)
                        Nothing -> expectationFailure "Could not get GLFW time after set"

        Nothing → describe "GLFW Window" $ 
            it "exists" $ expectationFailure "Window not found in state"
