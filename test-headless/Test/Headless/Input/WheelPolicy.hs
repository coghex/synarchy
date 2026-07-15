{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #744 gate (Phase A, child A3 of #741): the unified wheel-routing
--   contract. An ordinary wheel event and a Shift-held one now share
--   ONE pipeline — framebuffer-coordinate conversion, the
--   degenerate-viewport guard, and #742/#743's modal-boundary +
--   scroll-capture policy — before either can become a gameplay
--   action (camera-zoom scroll or Shift z-slice paging). Most of the
--   modal/debug/visibility matrix reuses 'UI.InputOwnership''s
--   already-pure 'routeScroll'/'isGameplayBlocked' directly (the
--   acceptance explicitly wants routing testable without Vulkan, a
--   window, or a running Lua engine); a "wire integration" block
--   drives the real 'Engine.Input.Thread.processInputs' to prove the
--   Shift-key extraction, the degenerate-viewport guard, and the
--   z-slice/game-scroll/UI-scroll selection that only exist inside
--   'Engine.Input.Thread.Scroll''s IO dispatch, not as a standalone
--   pure function.
module Test.Headless.Input.WheelPolicy (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Test.Hspec
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Input.Bindings (defaultKeyBindings)
import Engine.Input.Inject (noMods, scrollSequence)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Focus (createFocusManager)
import UI.InputOwnership (routeScroll, isGameplayBlocked)
import UI.Manager
import UI.Types

-- * Pure fixtures

-- | Create a shown page on the given layer.
page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

-- | A scroll-capturing element with no click callback (#743's own
--   motivating case — a log/list panel background).
scrollCaptureAt ∷ Text → (Float, Float) → (Float, Float) → PageHandle
               → UIPageManager → (ElementHandle, UIPageManager)
scrollCaptureAt name (x, y) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementCapturesScroll eh True m2
    in (eh, m3)

-- | A purely visual, pass-through element: no flags, no callbacks.
visualAt ∷ Text → (Float, Float) → (Float, Float) → PageHandle
        → UIPageManager → (ElementHandle, UIPageManager)
visualAt name (x, y) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
    in (eh, m2)

-- | A passive child nested inside an existing element, at a RELATIVE
--   offset (per 'addChildElement').
childOf ∷ ElementHandle → Text → (Float, Float) → (Float, Float) → PageHandle
       → UIPageManager → (ElementHandle, UIPageManager)
childOf parentH name (relX, relY) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addChildElement parentH eh relX relY m1
    in (eh, m2)

pt ∷ (Float, Float)
pt = (50, 50)

shiftMods ∷ GLFW.ModifierKeys
shiftMods = noMods { GLFW.modifierKeysShift = True }

spec ∷ Spec
spec = do
    describe "routeScroll / isGameplayBlocked — modal + debug matrix (#744)" $ do
        it "a modal's own scroll-capturing element wins over an overlapping lower HUD one" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_loH, m2) = scrollCaptureAt "hudPanel" pt (100, 100) hudH m1
                (modalH, m3) = page "modal" LayerModal m2
                (hiH, m4) = scrollCaptureAt "modalPanel" pt (100, 100) modalH m3
            in routeScroll pt m4 `shouldBe` Just hiH

        it "empty modal space blocks the wheel from reaching a lower HUD scroll-capturing element" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = scrollCaptureAt "hudPanel" pt (100, 100) hudH m1
                (_modalH, m3) = page "modal" LayerModal m2
            in do
                routeScroll pt m3 `shouldBe` Nothing
                isGameplayBlocked m3 `shouldBe` True

        it "a debug-layer scroll target above a modal wins (synthetic page — no shipping debug panel registers one)" $
            let (debugH, m1) = page "shell" LayerDebug emptyUIPageManager
                (debugEh, m2) = scrollCaptureAt "debugPanel" pt (100, 100) debugH m1
                (_modalH, m3) = page "modal" LayerModal m2
            in routeScroll pt m3 `shouldBe` Just debugEh

        it "empty debug space passes through to a lower modal boundary but does not cross it" $
            let (_debugH, m1) = page "shell" LayerDebug emptyUIPageManager
                (_modalH, m2) = page "modal" LayerModal m1
            in do
                routeScroll pt m2 `shouldBe` Nothing
                isGameplayBlocked m2 `shouldBe` True

        it "empty debug space continues on to gameplay when no modal is visible" $
            let (_debugH, m1) = page "shell" LayerDebug emptyUIPageManager
            in do
                routeScroll pt m1 `shouldBe` Nothing
                isGameplayBlocked m1 `shouldBe` False

    describe "routeScroll visibility / detachment (#744, mirrors #743's own matrix)" $ do
        it "a hidden scroll-capturing element does not capture the wheel" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = scrollCaptureAt "panel" pt (100, 100) hudH m1
                m3 = setElementVisible eh False m2
            in routeScroll pt m3 `shouldBe` Nothing

        it "an element beneath a hidden ancestor does not capture the wheel" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (parentH, m2) = visualAt "parent" pt (100, 100) hudH m1
                (childH, m3) = childOf parentH "child" (0, 0) (100, 100) hudH m2
                m4 = setElementCapturesScroll childH True m3
                m5 = setElementVisible parentH False m4
            in routeScroll pt m5 `shouldBe` Nothing

        it "a detached element (never added to any page) does not capture the wheel" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = createElement "detached" 100 100 hudH m1
                m3 = setElementCapturesScroll eh True m2
            in routeScroll pt m3 `shouldBe` Nothing

        it "an element on a hidden (never-shown) page does not capture the wheel" $
            let (hudH, m1) = createPage "hud" LayerHUD emptyUIPageManager  -- NOT shown
                (_eh, m2) = scrollCaptureAt "panel" pt (100, 100) hudH m1
            in routeScroll pt m2 `shouldBe` Nothing

    describe "synthetic input shares the physical policy (#744)" $
        it "scrollSequence constructs the identical InputScrollEvent a physical wheel produces" $
            scrollSequence 3 (-5) `shouldBe` [InputScrollEvent 3 (-5)]

    -- Wire-level integration: the pieces above prove the pure modal/
    -- debug/visibility routing; this drives the real dispatch — Shift
    -- extraction from held-key state, the shared degenerate-viewport
    -- guard, DPI-scaled hit-testing, and the final
    -- z-slice/game-scroll/UI-scroll/modal-block classification — none
    -- of which exist as standalone pure functions.
    around withHeadlessEngine $
        describe "wire integration (Engine.Input.Thread.Scroll) — #744" $ do
            it "unmodified wheel over a scroll-capturing element routes to UI, shiftHeld=False, exactly one outcome" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (eh, m2) = scrollCaptureAt "panel" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env [InputCursorMove (realToFrac px) (realToFrac py), InputScrollEvent 0 1]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIScrollEvent eh 0 1 False)
                filter isAnyScrollFamilyMsg msgs `shouldSatisfy` ((≡ 1) ∘ length)
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "accepted"
                        aoHandler r `shouldBe` Just "ui_scroll"
                        aoTarget r `shouldBe` Just (unElementHandle eh)
                        aoWhereX r `shouldBe` Just (realToFrac px)
                        aoWhereY r `shouldBe` Just (realToFrac py)
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "Left-Shift-held wheel over a scroll-capturing element still routes to UI, shiftHeld=True (the element always wins)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (eh, m2) = scrollCaptureAt "panel" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env [InputCursorMove (realToFrac px) (realToFrac py)]
                push env [InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftMods]
                inputTick env
                _ ← drainOutcomes env
                push env [InputScrollEvent 0 1]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIScrollEvent eh 0 1 True)
                filter isAnyScrollFamilyMsg msgs `shouldSatisfy` ((≡ 1) ∘ length)

            it "Right-Shift-held wheel over a scroll-capturing element also reads shiftHeld=True (both Shift keys recognized)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (eh, m2) = scrollCaptureAt "panel" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env [InputCursorMove (realToFrac px) (realToFrac py)]
                push env [InputKeyEvent GLFW.Key'RightShift GLFW.KeyState'Pressed shiftMods]
                inputTick env
                _ ← drainOutcomes env
                push env [InputScrollEvent 0 1]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIScrollEvent eh 0 1 True)

            it "unowned unmodified wheel routes to game scroll, exactly one outcome" $ \env → do
                resetAll env
                push env [InputCursorMove 900 600, InputScrollEvent 0 (-2)]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaScrollEvent 0 (-2))
                filter isAnyScrollFamilyMsg msgs `shouldSatisfy` ((≡ 1) ∘ length)
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "accepted"
                        aoHandler r `shouldBe` Just "game_scroll"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "unowned Left-Shift wheel routes to z-slice" $ \env → do
                resetAll env
                push env [InputCursorMove 900 600]
                push env [InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftMods]
                inputTick env
                _ ← drainOutcomes env
                push env [InputScrollEvent 0 (-2)]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaZSliceScroll 0 (-2))
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "accepted"
                        aoHandler r `shouldBe` Just "z_slice"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "unowned Right-Shift wheel also routes to z-slice (both Shift keys recognized)" $ \env → do
                resetAll env
                push env [InputCursorMove 900 600]
                push env [InputKeyEvent GLFW.Key'RightShift GLFW.KeyState'Pressed shiftMods]
                inputTick env
                _ ← drainOutcomes env
                push env [InputScrollEvent 0 (-2)]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaZSliceScroll 0 (-2))

            it "empty modal space consumes an unmodified wheel — no scroll message reaches Lua at all" $ \env → do
                resetAll env
                let (_modalH, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                let (px, py) = pt
                push env [InputCursorMove (realToFrac px) (realToFrac py), InputScrollEvent 0 (-2)]
                inputTick env
                msgs ← drainLuaMsgs env
                filter isAnyScrollFamilyMsg msgs `shouldBe` []
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "noop"
                        aoHandler r `shouldBe` Just "ui_modal_block"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "empty modal space consumes a Shift-held wheel too — no z-slice reaches Lua (the pre-#744 bug this issue fixes)" $ \env → do
                resetAll env
                let (_modalH, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                let (px, py) = pt
                push env [InputCursorMove (realToFrac px) (realToFrac py)]
                push env [InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftMods]
                inputTick env
                _ ← drainOutcomes env
                push env [InputScrollEvent 0 (-2)]
                inputTick env
                msgs ← drainLuaMsgs env
                filter isAnyScrollFamilyMsg msgs `shouldBe` []
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "noop"
                        aoHandler r `shouldBe` Just "ui_modal_block"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "DPI-scaled coordinates select the correct scroll-capturing target" $ \env → do
                resetAll env
                -- 2x framebuffer-vs-window scale (like a Retina display).
                writeIORef (windowSizeRef env) (640, 360)
                writeIORef (framebufferSizeRef env) (1280, 720)
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (eh, m2) = scrollCaptureAt "panel" (100, 100) (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                -- Raw WINDOW-space cursor (60,60) scales to framebuffer
                -- (120,120) — inside the 100..200 box; the raw value
                -- itself would miss it entirely.
                push env [InputCursorMove 60 60, InputScrollEvent 0 1]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIScrollEvent eh 0 1 False)

            it "a degenerate viewport drops an unmodified wheel event entirely" $ \env → do
                resetAll env
                writeIORef (windowSizeRef env) (0, 0)
                writeIORef (framebufferSizeRef env) (0, 0)
                push env [InputScrollEvent 0 (-2)]
                inputTick env
                msgs ← drainLuaMsgs env
                filter isAnyScrollFamilyMsg msgs `shouldBe` []
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "noop"
                        aoHandler r `shouldBe` Just "degenerate_viewport"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "a degenerate viewport drops a Shift-held wheel event too (pre-#744 the Shift branch bypassed this guard)" $ \env → do
                resetAll env
                writeIORef (windowSizeRef env) (0, 0)
                writeIORef (framebufferSizeRef env) (0, 0)
                push env [InputKeyEvent GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftMods]
                inputTick env
                _ ← drainOutcomes env
                push env [InputScrollEvent 0 (-2)]
                inputTick env
                msgs ← drainLuaMsgs env
                filter isAnyScrollFamilyMsg msgs `shouldBe` []
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "noop"
                        aoHandler r `shouldBe` Just "degenerate_viewport"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "an injected (synthetic) scroll sequence reaches the identical decision as a raw physical scroll event" $ \env → do
                resetAll env
                push env [InputCursorMove 900 600]
                push env (scrollSequence 0 (-2))
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaScrollEvent 0 (-2))
                filter isAnyScrollFamilyMsg msgs `shouldSatisfy` ((≡ 1) ∘ length)

-- * Wire-integration helpers (mirrors Test.Headless.UI.InputOwnership /
--   Test.Headless.UI.ElementInputPolicy / Test.Headless.Input.LayerA)

resetAll ∷ EngineEnv → IO ()
resetAll env = do
    writeIORef (inputStateRef env) defaultInputState
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)
    writeIORef (focusManagerRef env) createFocusManager
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (keyBindingsRef env) defaultKeyBindings
    _ ← atomicModifyIORef' (actionOutcomeRef env) $ \_ → (Seq.empty, ())
    _ ← drainLuaMsgs env
    pure ()

inputTick ∷ EngineEnv → IO ()
inputTick env = do
    st ← readIORef (inputStateRef env)
    _ ← processInputs env st
    pure ()

push ∷ EngineEnv → [InputEvent] → IO ()
push env = mapM_ (Q.writeQueue (inputQueue env))

drainLuaMsgs ∷ EngineEnv → IO [LuaMsg]
drainLuaMsgs env = go []
  where
    go acc = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Just msg → go (msg : acc)
            Nothing  → pure (reverse acc)

drainOutcomes ∷ EngineEnv → IO [ActionOutcome]
drainOutcomes env = toList ⊚ atomicModifyIORef' (actionOutcomeRef env)
    (\buf → (Seq.empty, buf))

isUIScrollMsg ∷ LuaMsg → Bool
isUIScrollMsg (LuaUIScrollEvent _ _ _ _) = True
isUIScrollMsg _ = False

isGameScrollMsg ∷ LuaMsg → Bool
isGameScrollMsg (LuaScrollEvent _ _) = True
isGameScrollMsg _ = False

isZSliceMsg ∷ LuaMsg → Bool
isZSliceMsg (LuaZSliceScroll _ _) = True
isZSliceMsg _ = False

-- | Any of the three mutually-exclusive scroll-family broadcasts —
--   used to assert "at most/exactly one routed action" independent of
--   which of the three it was.
isAnyScrollFamilyMsg ∷ LuaMsg → Bool
isAnyScrollFamilyMsg m = isUIScrollMsg m ∨ isGameScrollMsg m ∨ isZSliceMsg m
