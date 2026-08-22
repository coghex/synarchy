-- test/Test/Engine/Graphics/Window/GLFW.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Window.GLFW (spec) where

import UPrelude
import Control.Exception (finally)
import Test.Hspec
import Engine.Graphics.Window.Types (Window(..))
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.State

-- | Main test specification for GLFW functionality
spec ∷ EngineEnv → EngineState → Spec
spec _env state = 
    case glfwWindow (graphicsState state) of
        Just _win@(Window glfwWin) → do
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

                -- Assert against the value the setter was GIVEN (#1400): a
                -- fresh process clock already reads near 0, so the old
                -- `setTime 0` + `>= 0` pair passed just as happily when the
                -- setter did nothing. The clock counts up from the value it
                -- was set to, so a distinctive target far above any plausible
                -- suite runtime, bounded on both sides, distinguishes a real
                -- set from a no-op and from a wrong-but-larger one.
                -- The clock is process-global and this example moves it, so
                -- the reset to a neutral origin runs on EVERY exit -- a failed
                -- read or assertion must not leave the later examples (and any
                -- other reader in this process) on a shifted clock.
                it "can set time" $ do
                    let target    = 12345 ∷ Double
                        tolerance = 1     ∷ Double
                    (do GLFW.setTime target
                        time <- GLFW.getTime
                        case time of
                            Just t -> do
                                t `shouldSatisfy` (>= target)
                                t `shouldSatisfy` (<= target + tolerance)
                            Nothing -> expectationFailure "Could not get GLFW time after set")
                        `finally` GLFW.setTime 0

        Nothing → describe "GLFW Window" $ 
            it "exists" $ expectationFailure "Window not found in state"
