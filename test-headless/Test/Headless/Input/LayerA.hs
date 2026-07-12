{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | F4 (#730) Layer A regression coverage for the non-click input
--   families: keyboard, text (char aggregation), and scroll/z-slice
--   routing. Drives the REAL 'Engine.Input.Thread.processInputs'
--   against a live headless 'EngineEnv' (same technique as
--   'Test.Headless.Input.Followup' and 'Test.Headless.World.
--   ActionOutcome' — the harness starts neither the input nor the Lua
--   thread, so both the input queue and the action-outcome ring are
--   this test's to drive and drain by hand) and asserts both a
--   consumed and an ignored route for each family, per the issue's
--   acceptance criteria.
module Test.Headless.Input.LayerA (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Graphics.UI.GLFW as GLFW
import Data.Foldable (toList)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Test.Hspec
import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Input.Bindings (defaultKeyBindings)
import Engine.Input.Inject (noMods)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import qualified Engine.Core.Queue as Q
import UI.Focus (createFocusManager, FocusId(..), registerFocusTarget, setFocus)
import UI.Manager (createPage, createElement, addElementToPage, showPage,
                    setElementClickable, setElementOnClick, enableTextInput,
                    setElementFocus)
import UI.Types (UILayer(..), ElementHandle(..), emptyUIPageManager)

-- | Reset every piece of state this spec touches to a known-clean
--   baseline: no shell/UI focus, default keybinds, a non-degenerate
--   viewport (headless boots with zero sizes, which the scroll path
--   reads as a "degenerate viewport" drop unless a test overrides it),
--   and an empty outcome ring / Lua queue left by an earlier spec.
resetAll ∷ EngineEnv → IO ()
resetAll env = do
    writeIORef (inputStateRef env) defaultInputState
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)
    writeIORef (focusManagerRef env) createFocusManager
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (keyBindingsRef env) defaultKeyBindings
    _ ← atomicModifyIORef' (actionOutcomeRef env) $ \_ → (Seq.empty, ())
    drainLuaQueue env

drainLuaQueue ∷ EngineEnv → IO ()
drainLuaQueue env = go
  where
    go = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Just _  → go
            Nothing → pure ()

-- | One input-thread tick: drain the input queue through the REAL
--   'processInputs' (publishes to inputStateRef, same as the real
--   input thread's loop) — including its tail-of-drain char-batch
--   flush, so a lone pending character aggregate always surfaces by
--   the time this returns.
inputTick ∷ EngineEnv → IO ()
inputTick env = do
    st ← readIORef (inputStateRef env)
    _ ← processInputs env st
    pure ()

push ∷ EngineEnv → [InputEvent] → IO ()
push env = mapM_ (Q.writeQueue (inputQueue env))

drainOutcomes ∷ EngineEnv → IO [ActionOutcome]
drainOutcomes env = toList ⊚ atomicModifyIORef' (actionOutcomeRef env)
    (\buf → (Seq.empty, buf))

-- | A shell (console) text focus: registers one text-accepting target
--   and focuses it, so 'Engine.Input.Thread.getInputMode' reads
--   @TextInputMode fid@ exactly like a real open shell.
shellFocus ∷ EngineEnv → IO FocusId
shellFocus env = do
    let (fid, fm1) = registerFocusTarget True 0 createFocusManager
        fm2 = setFocus fid fm1
    writeIORef (focusManagerRef env) fm2
    pure fid

-- | A single clickable + text-input-enabled + UI-FOCUSED element at a
--   known screen rect (100,50)-(200,80) — shared by the UI-text-focus
--   keyboard/char cases and the scroll-over-a-UI-element case.
--   'showPage' and 'setElementFocus' are both load-bearing:
--   'UI.Manager.validateFocus' (what Thread.hs actually reads) only
--   returns 'Just' when the focused element's OWN page is visible.
focusedUIElement ∷ EngineEnv → IO ElementHandle
focusedUIElement env = do
    let (pageH, m1) = createPage "test_page" LayerHUD emptyUIPageManager
        m2 = showPage pageH m1
        (fieldH, m3) = createElement "field" 100 30 pageH m2
        m4 = addElementToPage pageH fieldH 100 50 m3
        m5 = setElementClickable fieldH True m4
        m6 = setElementOnClick fieldH "onFieldClick" m5
        m7 = enableTextInput fieldH m6
        m8 = setElementFocus fieldH m7
    writeIORef (uiManagerRef env) m8
    pure fieldH

spec ∷ SpecWith EngineEnv
spec = do
    describe "keyboard routing" $ do
        it "a bound gameplay key drains one accepted record naming the bound action" $ \env → do
            resetAll env
            push env [InputKeyEvent GLFW.Key'W GLFW.KeyState'Pressed noMods]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.key"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "moveUp"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "an unbound gameplay key still drains one accepted record (domain fallback)" $ \env → do
            resetAll env
            push env [InputKeyEvent GLFW.Key'F1 GLFW.KeyState'Pressed noMods]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.key"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "gameplay_key"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a bare modifier key press never gets its own record (#730 review)" $ \env → do
            resetAll env
            push env [InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed
                        (noMods { GLFW.modifierKeysShift = True })]
            inputTick env
            recs ← drainOutcomes env
            recs `shouldSatisfy` null

        it "a key's release produces no additional record beyond its press" $ \env → do
            resetAll env
            push env [InputKeyEvent GLFW.Key'W GLFW.KeyState'Pressed noMods]
            push env [InputKeyEvent GLFW.Key'W GLFW.KeyState'Released noMods]
            inputTick env
            recs ← drainOutcomes env
            length recs `shouldBe` 1

        it "a shell-text-focused editing key (Enter) drains one accepted record naming it and targeting the focus id" $ \env → do
            resetAll env
            fid ← shellFocus env
            push env [InputKeyEvent GLFW.Key'Enter GLFW.KeyState'Pressed noMods]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.key"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "submit"
                    aoTarget r `shouldBe` Just (unFocusId fid)
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a shell-text-focused key matching no editing action drains one noop record (ignored route)" $ \env → do
            resetAll env
            _ ← shellFocus env
            push env [InputKeyEvent GLFW.Key'K GLFW.KeyState'Pressed noMods]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.key"
                    aoOutcome r `shouldBe` "noop"
                    aoHandler r `shouldBe` Just "shell_text"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a UI-text-focused editing key (Backspace) drains one accepted record targeting the element" $ \env → do
            resetAll env
            fieldH ← focusedUIElement env
            push env [InputKeyEvent GLFW.Key'Backspace GLFW.KeyState'Pressed noMods]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "backspace"
                    aoTarget r `shouldBe` Just (unElementHandle fieldH)
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a UI-text-focused key matching no editing action drains one noop record (ignored route)" $ \env → do
            resetAll env
            _ ← focusedUIElement env
            push env [InputKeyEvent GLFW.Key'F2 GLFW.KeyState'Pressed noMods]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoOutcome r `shouldBe` "noop"
                    aoHandler r `shouldBe` Just "ui_text"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

    describe "text (char aggregation) routing" $ do
        it "a single shell-focused character drains one accepted aggregate record" $ \env → do
            resetAll env
            fid ← shellFocus env
            push env [InputCharEvent 'x']
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.type"
                    aoOutcome r `shouldBe` "accepted"
                    aoRequested r `shouldBe` Just 1
                    aoApplied r `shouldBe` Just 1
                    aoDropped r `shouldBe` Just 0
                    aoTarget r `shouldBe` Just (unFocusId fid)
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "the reserved backtick character drains one noop aggregate record (ignored route)" $ \env → do
            resetAll env
            _ ← shellFocus env
            push env [InputCharEvent '`']
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.type"
                    aoOutcome r `shouldBe` "noop"
                    aoRequested r `shouldBe` Just 1
                    aoApplied r `shouldBe` Just 0
                    aoDropped r `shouldBe` Just 1
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a character with no focused target drains one noop aggregate record (ignored route)" $ \env → do
            resetAll env
            push env [InputCharEvent 'x']
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoOutcome r `shouldBe` "noop"
                    aoApplied r `shouldBe` Just 0
                    aoDropped r `shouldBe` Just 1
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a synthetic multi-character run collapses into ONE aggregate record, not one per character" $ \env → do
            resetAll env
            _ ← shellFocus env
            push env (map InputCharEvent "abc")
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.type"
                    aoOutcome r `shouldBe` "accepted"
                    aoRequested r `shouldBe` Just 3
                    aoApplied r `shouldBe` Just 3
                    aoDropped r `shouldBe` Just 0
                _ → expectationFailure ("expected exactly one aggregate record, got " ⧺ show recs)

        it "a mixed run of applied and dropped characters aggregates as one truthful partial record" $ \env → do
            resetAll env
            _ ← shellFocus env
            push env [InputCharEvent 'a', InputCharEvent '`', InputCharEvent 'b']
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoOutcome r `shouldBe` "partial"
                    aoRequested r `shouldBe` Just 3
                    aoApplied r `shouldBe` Just 2
                    aoDropped r `shouldBe` Just 1
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

    describe "scroll routing" $ do
        it "shift+scroll drains one accepted z_slice record" $ \env → do
            resetAll env
            push env [InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed
                        (noMods { GLFW.modifierKeysShift = True })]
            inputTick env
            _ ← drainOutcomes env  -- the modifier press itself never records
            push env [InputScrollEvent 0 (-2)]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.scroll"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "z_slice"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "scroll on a degenerate viewport drains one noop record (ignored route)" $ \env → do
            resetAll env
            writeIORef (windowSizeRef env) (0, 0)
            writeIORef (framebufferSizeRef env) (0, 0)
            push env [InputScrollEvent 0 (-2)]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.scroll"
                    aoOutcome r `shouldBe` "noop"
                    aoHandler r `shouldBe` Just "degenerate_viewport"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "scroll over a UI element drains one accepted ui_scroll record targeting it" $ \env → do
            resetAll env
            fieldH ← focusedUIElement env
            push env [InputCursorMove 150 65]
            inputTick env
            _ ← drainOutcomes env
            push env [InputScrollEvent 0 (-2)]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.scroll"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "ui_scroll"
                    aoTarget r `shouldBe` Just (unElementHandle fieldH)
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "scroll over empty space drains one accepted game_scroll record" $ \env → do
            resetAll env
            push env [InputCursorMove 900 600]
            inputTick env
            _ ← drainOutcomes env
            push env [InputScrollEvent 0 (-2)]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.scroll"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "game_scroll"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

    -- #730 review rounds 2-3: a ClickUI-routed press (or a
    -- middle-button camera-drag press) defers its F4 record to the
    -- matching release — exactly one "input.click" or "input.drag",
    -- classified by movement against a 4px window-pixel threshold, and
    -- an interrupted-release resolution (focus loss / minimize) so a
    -- pending classification is never silently lost.
    describe "click/drag routing (deferred UI/camera-drag classification)" $ do
        it "a UI click with no movement drains one accepted input.click record" $ \env → do
            resetAll env
            _ ← focusedUIElement env
            push env [InputMouseEvent GLFW.MouseButton'1 (150, 65) GLFW.MouseButtonState'Pressed]
            push env [InputMouseEvent GLFW.MouseButton'1 (150, 65) GLFW.MouseButtonState'Released]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.click"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "onFieldClick"
                    aoTarget r `shouldBe` Nothing
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a UI press dragged past the threshold before release drains one accepted input.drag record instead" $ \env → do
            resetAll env
            _ ← focusedUIElement env
            push env [InputMouseEvent GLFW.MouseButton'1 (150, 65) GLFW.MouseButtonState'Pressed]
            push env [InputMouseEvent GLFW.MouseButton'1 (250, 165) GLFW.MouseButtonState'Released]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.drag"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "onFieldClick"
                _ → expectationFailure ("expected exactly one record (never both click and drag), got " ⧺ show recs)

        it "a middle-button press with no movement drains one accepted camera_drag input.click record" $ \env → do
            resetAll env
            push env [InputMouseEvent GLFW.MouseButton'3 (900, 600) GLFW.MouseButtonState'Pressed]
            push env [InputMouseEvent GLFW.MouseButton'3 (900, 600) GLFW.MouseButtonState'Released]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.click"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "camera_drag"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)

        it "a middle-button press dragged past the threshold drains one accepted camera_drag input.drag record" $ \env → do
            resetAll env
            push env [InputMouseEvent GLFW.MouseButton'3 (900, 600) GLFW.MouseButtonState'Pressed]
            push env [InputMouseEvent GLFW.MouseButton'3 (1000, 700) GLFW.MouseButtonState'Released]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.drag"
                    aoOutcome r `shouldBe` "accepted"
                    aoHandler r `shouldBe` Just "camera_drag"
                _ → expectationFailure ("expected exactly one record, got " ⧺ show recs)

        it "a focus-loss transition mid-press resolves the pending UI click as an interrupted noop instead of losing it (#730 review round 3)" $ \env → do
            resetAll env
            _ ← focusedUIElement env
            push env [InputMouseEvent GLFW.MouseButton'1 (150, 65) GLFW.MouseButtonState'Pressed]
            inputTick env
            _ ← drainOutcomes env  -- the press alone defers; nothing recorded yet
            push env [InputWindowEvent (WindowFocus False)]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.click"
                    aoOutcome r `shouldBe` "noop"
                    aoHandler r `shouldBe` Just "onFieldClick"
                    aoReason r `shouldBe` Just "release swallowed (focus loss / minimize)"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)
            -- The pending-click state itself is cleared too (not just
            -- recorded) — a later, unrelated press on the same button
            -- must not inherit a stale press position.
            afterState ← readIORef (inputStateRef env)
            inpPendingUIClick afterState `shouldBe` Map.empty

        it "a window-minimize transition mid-press resolves a pending middle-button camera-drag the same way" $ \env → do
            resetAll env
            push env [InputMouseEvent GLFW.MouseButton'3 (900, 600) GLFW.MouseButtonState'Pressed]
            inputTick env
            _ ← drainOutcomes env
            push env [InputWindowEvent (WindowMinimize True)]
            inputTick env
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoKind r `shouldBe` "input.click"
                    aoOutcome r `shouldBe` "noop"
                    aoHandler r `shouldBe` Just "camera_drag"
                    aoReason r `shouldBe` Just "release swallowed (focus loss / minimize)"
                _ → expectationFailure ("expected one record, got " ⧺ show recs)
