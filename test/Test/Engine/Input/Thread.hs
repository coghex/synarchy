module Test.Engine.Input.Thread (spec) where

import UPrelude
import Test.Hspec
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Thread (shouldTrackKeyStateWhileTextFocused)

spec ∷ Spec
spec = do
  describe "Engine.Input.Thread" $ do
    describe "shouldTrackKeyStateWhileTextFocused" $ do
      it "keeps held modifier presses visible while text input is focused" $ do
        shouldTrackKeyStateWhileTextFocused GLFW.Key'LeftShift
          GLFW.KeyState'Pressed `shouldBe` True
        shouldTrackKeyStateWhileTextFocused GLFW.Key'RightControl
          GLFW.KeyState'Pressed `shouldBe` True
        shouldTrackKeyStateWhileTextFocused GLFW.Key'LeftAlt
          GLFW.KeyState'Pressed `shouldBe` True

      it "still suppresses ordinary typing keys while text input is focused" $ do
        shouldTrackKeyStateWhileTextFocused GLFW.Key'A
          GLFW.KeyState'Pressed `shouldBe` False
        shouldTrackKeyStateWhileTextFocused GLFW.Key'B
          GLFW.KeyState'Repeating `shouldBe` False

      it "always records releases so keys cannot stick down" $ do
        shouldTrackKeyStateWhileTextFocused GLFW.Key'A
          GLFW.KeyState'Released `shouldBe` True
        shouldTrackKeyStateWhileTextFocused GLFW.Key'LeftShift
          GLFW.KeyState'Released `shouldBe` True
