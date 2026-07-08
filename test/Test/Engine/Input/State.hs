module Test.Engine.Input.State (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.State
  (shouldTrackKeyStateWhileTextFocused, updateWindowState, heldButtonReleases)
import Engine.Input.Types

-- | An input state with a key and a mouse button logically held down,
--   as if a drag / held modifier were in progress.
heldState ∷ InputState
heldState = defaultInputState
    { inpKeyStates = Map.fromList
        [ (GLFW.Key'LeftShift, defaultKeyState { keyPressed = True }) ]
    , inpMouseBtns = Map.fromList
        [ (GLFW.MouseButton'1, True) ]
    , inpMouseRoutes = Map.fromList
        [ (GLFW.MouseButton'1, ClickGame) ]
    }

allReleased ∷ InputState → Bool
allReleased s = Map.null (inpKeyStates s)
              ∧ Map.null (inpMouseBtns s)
              ∧ Map.null (inpMouseRoutes s)

spec ∷ Spec
spec = do
  describe "Engine.Input.State" $ do
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

    describe "updateWindowState" $ do
      it "clears held key/button state when focus is lost" $ do
        let s = updateWindowState heldState (WindowFocus False)
        inpWindowFocused s `shouldBe` False
        allReleased s `shouldBe` True

      it "clears held key/button state when minimized" $ do
        allReleased (updateWindowState heldState (WindowMinimize True))
          `shouldBe` True

      it "keeps held state on focus gain (only flips the flag)" $ do
        let s = updateWindowState heldState { inpWindowFocused = False }
                                  (WindowFocus True)
        inpWindowFocused s `shouldBe` True
        allReleased s `shouldBe` False

      it "leaves state untouched on restore from minimize" $ do
        allReleased (updateWindowState heldState (WindowMinimize False))
          `shouldBe` False

    describe "heldButtonReleases" $ do
      it "synthesizes a release per held button at the last cursor pos" $ do
        let s = heldState { inpMousePos = (12.0, 34.0) }
        heldButtonReleases s
          `shouldBe` [(GLFW.MouseButton'1, 12.0, 34.0, ClickSwallowed)]

      it "routes every release as swallowed (cancel, not commit)" $ do
        let s = heldState
              { inpMousePos    = (5.0, 6.0)
              , inpMouseBtns   = Map.fromList
                  [ (GLFW.MouseButton'1, True), (GLFW.MouseButton'2, True) ]
              , inpMouseRoutes = Map.fromList
                  [ (GLFW.MouseButton'1, ClickGame)
                  , (GLFW.MouseButton'2, ClickUI) ]
              }
        -- Map.toList yields ascending button order ('1 before '2);
        -- the original ClickGame/ClickUI routes are deliberately
        -- replaced with ClickSwallowed.
        heldButtonReleases s `shouldBe`
          [ (GLFW.MouseButton'1, 5.0, 6.0, ClickSwallowed)
          , (GLFW.MouseButton'2, 5.0, 6.0, ClickSwallowed) ]

      it "emits nothing when no button is held" $
        heldButtonReleases defaultInputState `shouldBe` []
