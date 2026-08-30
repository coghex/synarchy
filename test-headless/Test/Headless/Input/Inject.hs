-- | #644 input.* injection — the pure, GPU-free half: key/button/
--   modifier name resolution (must stay on the canonical keybind
--   vocabulary, never a hand-rolled set), the framebuffer→window
--   coordinate conversion (the retina/DPI trap that silently poisons
--   every harness click if wrong), and the synthesized event
--   sequences' shape (every click carries its cursor move, every
--   press pairs with a release, modifiers bracket the action — with
--   the releases riding an 'InputFollowup' fence (#697) so they are
--   processed only after the action's Lua callbacks have run).
--
--   #1927 split the modifier story in two. A TAP's modifiers still
--   live entirely in one sequence and are asserted here as before. A
--   SPLIT hold's now cross two independent verb calls, so its builders
--   only CLAIM ('InputGestureHold') and END ('InputGestureEnd') — what
--   is actually released is resolved by the input thread against the
--   live ownership record, and is therefore asserted as STATE, by the
--   env-driven "Input.Inject ownership" group
--   ("Test.Headless.Input.InjectOwnership"), not as a list shape here.
module Test.Headless.Input.Inject (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Inject
import Engine.Input.Types (InputEvent(..), InjectGesture(..))
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

        it "modifiers bracket a click (down before move, fenced up after release)" $ do
            let evs = clickSequence pos GLFW.MouseButton'1 shiftMod
            case (evs, reverse evs) of
                (InputKeyEvent k1 s1 _ : _, InputFollowup ups : _) → do
                    (k1, s1) `shouldBe`
                        (GLFW.Key'LeftShift, GLFW.KeyState'Pressed)
                    ups `shouldBe`
                        [ InputKeyEvent GLFW.Key'LeftShift
                            GLFW.KeyState'Released noMods ]
                _ → expectationFailure
                    "expected modifier press first and a followup fence last"
            length evs `shouldBe` 5

        it "mouseDown claims its modifiers under the gesture; mouseUp ends that claim (#1927)" $ do
            let downs = mouseDownSequence pos GLFW.MouseButton'2 shiftMod
                ups   = mouseUpSequence pos GLFW.MouseButton'2
            downs `shouldBe`
                [ InputGestureHold (GestureMouse GLFW.MouseButton'2)
                    [GLFW.Key'LeftShift] (snd shiftMod)
                , InputCursorMove 100 50
                , InputMouseEvent GLFW.MouseButton'2 pos
                    GLFW.MouseButtonState'Pressed
                ]
            -- The up half carries no modifier list of its own: what it
            -- releases is the DOWN half's claim, resolved by the input
            -- thread (which is where the ownership record lives). The
            -- state-level proof of that is Input.Inject ownership.
            ups `shouldBe`
                [ InputCursorMove 100 50
                , InputMouseEvent GLFW.MouseButton'2 pos
                    GLFW.MouseButtonState'Released
                , InputGestureEnd (GestureMouse GLFW.MouseButton'2) noMods
                ]

        it "no inline modifier release outside the fence (#697)" $ do
            -- The whole point of the fence: a release drained in the
            -- same batch as the action is already the published state
            -- by the time the Lua callbacks run, so a synthetic
            -- shift-click reads as a plain click. Every mod-bearing
            -- sequence must carry its releases ONLY inside the fence.
            let inlineRelease ev = case ev of
                    InputKeyEvent _ GLFW.KeyState'Released _ → True
                    _                                        → False
                sequences =
                    [ clickSequence pos GLFW.MouseButton'1 shiftMod
                    , keyTapSequence GLFW.Key'W shiftMod
                    ]
            forM_ sequences $ \evs → case reverse evs of
                (InputFollowup ups : rest) → do
                    -- keyUpSequence's own key release is legitimate
                    -- inline; only MODIFIER releases must be fenced.
                    filter inlineRelease
                        [ e | e ← reverse rest
                        , case e of
                            InputKeyEvent k _ _ → k ≡ GLFW.Key'LeftShift
                            _                   → False ]
                        `shouldBe` []
                    ups `shouldSatisfy` all inlineRelease
                _ → expectationFailure "expected a trailing followup fence"

        it "a split hold's release is fenced by the input thread, not inline (#1927)" $ do
            -- Same #697 property for the split halves, which no longer
            -- carry their own fence: neither half may release a
            -- modifier inline. The DOWN half claims (no release at
            -- all); the UP half only asks the input thread to end the
            -- claim, and THAT is what fences (Input.Inject ownership
            -- drives the real thread and asserts the resulting state).
            let splitSeqs =
                    [ mouseDownSequence pos GLFW.MouseButton'1 shiftMod
                    , mouseUpSequence pos GLFW.MouseButton'1
                    , keyDownSequence GLFW.Key'W shiftMod
                    , keyUpSequence GLFW.Key'W
                    ]
                modifierRelease ev = case ev of
                    InputKeyEvent k GLFW.KeyState'Released _ →
                        k ≡ GLFW.Key'LeftShift
                    _ → False
            forM_ splitSeqs $ \evs →
                filter modifierRelease evs `shouldBe` []

        it "multiple modifiers release in reverse order inside one fence" $ do
            let mm = ( [GLFW.Key'LeftShift, GLFW.Key'LeftControl]
                     , noMods { GLFW.modifierKeysShift = True
                              , GLFW.modifierKeysControl = True } )
                evs = clickSequence pos GLFW.MouseButton'1 mm
            take 2 evs `shouldBe`
                [ InputKeyEvent GLFW.Key'LeftShift
                    GLFW.KeyState'Pressed (snd mm)
                , InputKeyEvent GLFW.Key'LeftControl
                    GLFW.KeyState'Pressed (snd mm)
                ]
            case reverse evs of
                (InputFollowup ups : _) → ups `shouldBe`
                    [ InputKeyEvent GLFW.Key'LeftControl
                        GLFW.KeyState'Released noMods
                    , InputKeyEvent GLFW.Key'LeftShift
                        GLFW.KeyState'Released noMods
                    ]
                _ → expectationFailure "expected a trailing followup fence"

        it "plain (modifier-free) sequences carry no fence" $ do
            let plainSeqs =
                    [ clickSequence pos GLFW.MouseButton'1 ([], noMods)
                    , mouseDownSequence pos GLFW.MouseButton'1 ([], noMods)
                    , mouseUpSequence pos GLFW.MouseButton'1
                    , keyTapSequence GLFW.Key'A ([], noMods)
                    , keyDownSequence GLFW.Key'A ([], noMods)
                    , keyUpSequence GLFW.Key'A
                    ]
                isFence ev = case ev of
                    InputFollowup _ → True
                    _               → False
            forM_ plainSeqs $ \evs →
                filter isFence evs `shouldBe` []

        it "a modifier-free split hold claims nothing (#1927)" $ do
            -- The up half always asks the input thread to end its
            -- gesture, but a hold that claimed nothing must resolve to
            -- nothing there — no fence, no release, no Lua message —
            -- which is what keeps plain split holds behaving exactly
            -- as they did before #1927.
            let isClaim ev = case ev of
                    InputGestureHold{} → True
                    _                  → False
            filter isClaim (mouseDownSequence pos GLFW.MouseButton'1 ([], noMods))
                `shouldBe` []
            filter isClaim (keyDownSequence GLFW.Key'A ([], noMods))
                `shouldBe` []

        it "key tap = press then release carrying the modifier flags" $
            keyTapSequence GLFW.Key'L ([], noMods) `shouldBe`
                [ InputKeyEvent GLFW.Key'L GLFW.KeyState'Pressed noMods
                , InputKeyEvent GLFW.Key'L GLFW.KeyState'Released noMods
                ]

        it "modded key tap holds the modifier and fences its release" $
            keyTapSequence GLFW.Key'L shiftMod `shouldBe`
                [ InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed
                    (snd shiftMod)
                , InputKeyEvent GLFW.Key'L GLFW.KeyState'Pressed
                    (snd shiftMod)
                , InputKeyEvent GLFW.Key'L GLFW.KeyState'Released
                    (snd shiftMod)
                , InputFollowup
                    [ InputKeyEvent GLFW.Key'LeftShift
                        GLFW.KeyState'Released noMods ]
                ]

        it "keyDown/keyUp split a hold into matching halves" $ do
            keyDownSequence GLFW.Key'W ([], noMods) `shouldBe`
                [InputKeyEvent GLFW.Key'W GLFW.KeyState'Pressed noMods]
            keyUpSequence GLFW.Key'W `shouldBe`
                [ InputKeyEvent GLFW.Key'W GLFW.KeyState'Released noMods
                , InputGestureEnd (GestureKey GLFW.Key'W) noMods
                ]

        it "keyDown claims its modifiers under the gesture (#1927)" $
            keyDownSequence GLFW.Key'W shiftMod `shouldBe`
                [ InputGestureHold (GestureKey GLFW.Key'W)
                    [GLFW.Key'LeftShift] (snd shiftMod)
                , InputKeyEvent GLFW.Key'W GLFW.KeyState'Pressed
                    (snd shiftMod)
                ]

        it "type emits one char event per character" $
            typeSequence "Hi!" `shouldBe`
                [InputCharEvent 'H', InputCharEvent 'i', InputCharEvent '!']

        it "scroll and move are single events" $ do
            scrollSequence 0 (-3) `shouldBe` [InputScrollEvent 0 (-3)]
            moveSequence (7, 9) `shouldBe` [InputCursorMove 7 9]
