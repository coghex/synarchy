-- | #644 input.* injection — the pure, GPU-free half: key/button/
--   modifier name resolution (must stay on the canonical keybind
--   vocabulary, never a hand-rolled set), the framebuffer→window
--   coordinate conversion (the retina/DPI trap that silently poisons
--   every harness click if wrong), and the synthesized event
--   sequences' shape (every click carries its cursor move, every
--   press pairs with a release, modifiers bracket the action).
module Test.Headless.Input.Inject (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Inject
import Engine.Input.Types (InputEvent(..))
import Test.Hspec

spec ∷ Spec
spec = do
    describe "resolveButton" $ do
        it "maps left/right/middle (case-insensitive)" $ do
            resolveButton "left"   `shouldBe` Just GLFW.MouseButton'1
            resolveButton "Right"  `shouldBe` Just GLFW.MouseButton'2
            resolveButton "MIDDLE" `shouldBe` Just GLFW.MouseButton'3
        it "rejects unknown buttons" $
            resolveButton "back" `shouldBe` Nothing

    describe "resolveKeyName" $ do
        it "resolves the canonical keyToText vocabulary" $ do
            resolveKeyName "Space" `shouldBe` Just GLFW.Key'Space
            resolveKeyName "Enter" `shouldBe` Just GLFW.Key'Enter
            resolveKeyName "W"     `shouldBe` Just GLFW.Key'W
            resolveKeyName "F5"    `shouldBe` Just GLFW.Key'F5
        it "resolves a merged modifier to its left physical key" $
            resolveKeyName "Shift" `shouldBe` Just GLFW.Key'LeftShift
        it "resolves side-specific modifier aliases" $ do
            resolveKeyName "RightShift" `shouldBe` Just GLFW.Key'RightShift
            resolveKeyName "LeftCtrl"   `shouldBe` Just GLFW.Key'LeftControl
        it "rejects names outside the vocabulary" $ do
            resolveKeyName "NotAKey" `shouldBe` Nothing
            resolveKeyName "Unknown" `shouldBe` Nothing

    describe "resolveMods" $ do
        it "maps shift+ctrl to keys and flags" $
            case resolveMods ["shift", "ctrl"] of
                Nothing → expectationFailure "expected Just"
                Just (ks, m) → do
                    ks `shouldBe` [GLFW.Key'LeftShift, GLFW.Key'LeftControl]
                    GLFW.modifierKeysShift m `shouldBe` True
                    GLFW.modifierKeysControl m `shouldBe` True
                    GLFW.modifierKeysAlt m `shouldBe` False
        it "is case-insensitive and rejects unknown names" $ do
            fst ⊚ resolveMods ["SHIFT"] `shouldBe` Just [GLFW.Key'LeftShift]
            resolveMods ["shift", "hyper"] `shouldBe` Nothing
        it "no mods → no keys, no flags" $
            resolveMods [] `shouldBe` Just ([], noMods)

    describe "fbToWindow" $ do
        it "is identity at 1x DPI" $
            fbToWindow (1280, 720) (1280, 720) (100, 50)
                `shouldBe` Just (100, 50)
        it "halves coordinates on a 2x retina framebuffer" $
            fbToWindow (1280, 720) (2560, 1440) (200, 100)
                `shouldBe` Just (100, 50)
        it "refuses degenerate sizes (minimized/headless)" $ do
            fbToWindow (0, 720) (2560, 1440) (10, 10) `shouldBe` Nothing
            fbToWindow (1280, 720) (0, 0) (10, 10) `shouldBe` Nothing

    describe "event sequences" $ do
        let pos = (100, 50) ∷ (Double, Double)
            shiftMod = ([GLFW.Key'LeftShift]
                       , noMods { GLFW.modifierKeysShift = True })

        it "click = move, press, release at the same position" $
            clickSequence pos GLFW.MouseButton'1 ([], noMods) `shouldBe`
                [ InputCursorMove 100 50
                , InputMouseEvent GLFW.MouseButton'1 pos
                    GLFW.MouseButtonState'Pressed
                , InputMouseEvent GLFW.MouseButton'1 pos
                    GLFW.MouseButtonState'Released
                ]

        it "modifiers bracket a click (down before move, up after release)" $ do
            let evs = clickSequence pos GLFW.MouseButton'1 shiftMod
            case (evs, reverse evs) of
                (InputKeyEvent k1 s1 _ : _, InputKeyEvent k2 s2 _ : _) → do
                    (k1, s1) `shouldBe`
                        (GLFW.Key'LeftShift, GLFW.KeyState'Pressed)
                    (k2, s2) `shouldBe`
                        (GLFW.Key'LeftShift, GLFW.KeyState'Released)
                _ → expectationFailure "expected modifier key events at both ends"
            length evs `shouldBe` 5

        it "mouseDown holds its modifiers; mouseUp releases them after" $ do
            let downs = mouseDownSequence pos GLFW.MouseButton'2 shiftMod
                ups   = mouseUpSequence pos GLFW.MouseButton'2 shiftMod
            downs `shouldBe`
                [ InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed
                    (snd shiftMod)
                , InputCursorMove 100 50
                , InputMouseEvent GLFW.MouseButton'2 pos
                    GLFW.MouseButtonState'Pressed
                ]
            ups `shouldBe`
                [ InputCursorMove 100 50
                , InputMouseEvent GLFW.MouseButton'2 pos
                    GLFW.MouseButtonState'Released
                , InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Released
                    noMods
                ]

        it "key tap = press then release carrying the modifier flags" $
            keyTapSequence GLFW.Key'L ([], noMods) `shouldBe`
                [ InputKeyEvent GLFW.Key'L GLFW.KeyState'Pressed noMods
                , InputKeyEvent GLFW.Key'L GLFW.KeyState'Released noMods
                ]

        it "keyDown/keyUp split a hold into matching halves" $ do
            keyDownSequence GLFW.Key'W ([], noMods) `shouldBe`
                [InputKeyEvent GLFW.Key'W GLFW.KeyState'Pressed noMods]
            keyUpSequence GLFW.Key'W ([], noMods) `shouldBe`
                [InputKeyEvent GLFW.Key'W GLFW.KeyState'Released noMods]

        it "type emits one char event per character" $
            typeSequence "Hi!" `shouldBe`
                [InputCharEvent 'H', InputCharEvent 'i', InputCharEvent '!']

        it "scroll and move are single events" $ do
            scrollSequence 0 (-3) `shouldBe` [InputScrollEvent 0 (-3)]
            moveSequence (7, 9) `shouldBe` [InputCursorMove 7 9]
