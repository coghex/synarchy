{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #745 gate (Phase B child B1 of #741): the discrete-control
--   release-activation contract. Most of this suite is pure
--   'UI.ControlActivation' coverage — the acceptance (and the issue's
--   own testability clause, confirmed by its canonical review) wants
--   the activation DECISION itself testable without Vulkan, a window,
--   or a running Lua engine. A "wire integration" block drives the
--   real 'Engine.Input.Thread.processInputs' to prove the same
--   contract holds through the actual press/release dispatch path —
--   mirrors "Test.Headless.UI.ElementInputPolicy"'s structure exactly.
module Test.Headless.UI.ControlActivation (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Test.Hspec
import Data.IORef (readIORef, writeIORef, atomicModifyIORef', newIORef)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Input.Bindings (defaultKeyBindings)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaMsg(..), LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Focus (createFocusManager)
import UI.ControlActivation
import UI.InputOwnership (PointerKind(..))
import UI.Manager
import UI.Types

-- * Pure fixtures

page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

clickableAt ∷ Text → (Float, Float) → (Float, Float) → Text → PageHandle
           → UIPageManager → (ElementHandle, UIPageManager)
clickableAt name (x, y) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
    in (eh, m4)

pt ∷ (Float, Float)
pt = (10, 10)

basePage ∷ (PageHandle, UIPageManager)
basePage = page "hud" LayerHUD emptyUIPageManager

spec ∷ Spec
spec = do
    describe "resolveActivation (pure, #745)" $ do
        it "activates when the release lands inside the still-eligible element" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
            resolveActivation (15, 15) m2 pending `shouldBe` Activate eh "btnClick"

        it "cancels when the release lands outside the element (drag away and release outside)" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
            resolveActivation (500, 500) m2 pending `shouldSatisfy` isCancel

        it "activates again once the release returns inside — only the FINAL position matters" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
            -- Intermediate movement (simulated here just by never
            -- consulting it) has no bearing — releasing back inside
            -- resolves exactly like a never-moved press.
            resolveActivation (20, 20) m2 pending `shouldBe` Activate eh "btnClick"

        it "cancels when the element was hidden between press and release" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
                m3 = setElementVisible eh False m2
            resolveActivation (15, 15) m3 pending `shouldSatisfy` isCancel

        it "cancels when the element was deleted between press and release" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
                m3 = deleteElement eh m2
            resolveActivation (15, 15) m3 pending `shouldSatisfy` isCancel

        it "cancels when the element was disabled (UI.setClickable false) between press and release" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
                m3 = setElementClickable eh False m2
            resolveActivation (15, 15) m3 pending `shouldSatisfy` isCancel

        it "cancels when the element was detached from its page between press and release" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
                m3 = removeFromPage hudH eh m2
            resolveActivation (15, 15) m3 pending `shouldSatisfy` isCancel

        it "cancels when a modal now covers the same point (page replaced by a modal mid-press)" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
                (modalH, m3) = page "modal" LayerModal m2
                (_modalEh, m4) = clickableAt "modalBg" (0, 0) (400, 400) "modalClick" modalH m3
            resolveActivation (15, 15) m4 pending `shouldSatisfy` isCancel

        it "activates the freshly re-resolved callback when it was reassigned mid-press" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
                m3 = setElementOnClick eh "btnClickV2" m2
            resolveActivation (15, 15) m3 pending `shouldBe` Activate eh "btnClickV2"

        it "right/middle click never invokes the primary left action — a right-button pending activation only resolves via right-click routing" $ do
            let (hudH, m1) = basePage
                (eh, m2') = createElement "rightOnly" 100 100 hudH m1
                m3 = addElementToPage hudH eh (fst pt) (snd pt) m2'
                m4 = setElementClickable eh True m3
                m5 = setElementOnRightClick eh "rightClick" m4
                pending = beginActivation PointerLeftClick eh m5
            -- No left-click callback registered — a left release over
            -- this element cannot activate it.
            resolveActivation (15, 15) m5 pending `shouldSatisfy` isCancel

        it "diagnostics: activationOutcomeName maps Activate/Cancel to the existing accepted/rejected vocabulary" $ do
            let (hudH, m1) = basePage
                (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                pending = beginActivation PointerLeftClick eh m2
            activationOutcomeName (resolveActivation (15, 15) m2 pending) `shouldBe` "accepted"
            activationOutcomeName (resolveActivation (999, 999) m2 pending) `shouldBe` "rejected"

    around withHeadlessEngine $
        describe "wire integration (Engine.Input.Thread) — #745" $ do
            it "an ordinary click fires exactly one LuaUIClickEvent at release, not at press" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                pressMsgs ← drainLuaMsgs env
                pressMsgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                pressMsgs `shouldSatisfy` elem (LuaUIPressBeginEvent eh "btnClick")
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released]
                inputTick env
                releaseMsgs ← drainLuaMsgs env
                releaseMsgs `shouldSatisfy` elem (LuaUIClickEvent eh "btnClick" 15 15)
                length (filter isUIClickEvent releaseMsgs) `shouldBe` 1
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoKind r `shouldBe` "input.click"
                        aoOutcome r `shouldBe` "accepted"
                        aoHandler r `shouldBe` Just "btnClick"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "press then drag outside and release outside never fires the click, and records rejected" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                push env [InputMouseEvent GLFW.MouseButton'1 (900, 900) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "rejected"
                        aoReason r `shouldSatisfy` isJust
                        aoHandler r `shouldBe` Just "btnClick"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "returning inside before release restores activation" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                -- Drag far outside (cursor movement only — the button
                -- stays held throughout, exactly like a real drag),
                -- then release back inside the control's bounds — only
                -- the final release position is ever consulted, so
                -- this activates.
                push env [InputCursorMove 900 900]
                push env [InputMouseEvent GLFW.MouseButton'1 (20, 20) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIClickEvent eh "btnClick" 20 20)

            it "a drag-activation control (slider knob) still fires immediately on press, unchanged from before #745" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2) = clickableAt "knob" pt (20, 20) "onSliderKnobClick" hudH m1
                    m3 = setElementDragActivation eh True m2
                writeIORef (uiManagerRef env) m3
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                pressMsgs ← drainLuaMsgs env
                pressMsgs `shouldSatisfy` elem (LuaUIClickEvent eh "onSliderKnobClick" 15 15)
                pressMsgs `shouldSatisfy` all (not ∘ isPressBeginEvent)
                -- Even released far away (mid-drag), it was never
                -- pending — F4 still records "accepted", exactly as
                -- every drag gesture did before #745.
                push env [InputMouseEvent GLFW.MouseButton'1 (900, 900) GLFW.MouseButtonState'Released]
                inputTick env
                recs ← drainOutcomes env
                case recs of
                    [r] → aoOutcome r `shouldBe` "accepted"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "each release activates at most one control and callback once (no duplicate firing)" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env
                    [ InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed
                    , InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                length (filter (≡ LuaUIClickEvent eh "btnClick" 15 15) msgs) `shouldBe` 1

            it "right-click and left-click pending activations are independent — a right release never fires the left callback" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2') = createElement "both" 100 100 hudH m1
                    m3 = addElementToPage hudH eh (fst pt) (snd pt) m2'
                    m4 = setElementClickable eh True m3
                    m5 = setElementOnClick eh "leftClick" m4
                    m6 = setElementOnRightClick eh "rightClick" m5
                writeIORef (uiManagerRef env) m6
                push env
                    [ InputMouseEvent GLFW.MouseButton'2 (15, 15) GLFW.MouseButtonState'Pressed
                    , InputMouseEvent GLFW.MouseButton'2 (15, 15) GLFW.MouseButtonState'Released
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIRightClickEvent eh "rightClick" 15 15)
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)

            it "route pairing: LuaMouseUpEvent still reports the press as \"ui\" even though the click itself is deferred" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env
                    [ InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed
                    , InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` any (≡ LuaMouseUpEvent GLFW.MouseButton'1 15 15 ClickUI)

            it "a focus-loss transition mid-press cancels the pending activation safely (no click, no crash)" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                push env [InputWindowEvent (WindowFocus False)]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                afterState ← readIORef (inputStateRef env)
                inpPendingActivation afterState `shouldBe` mempty

            it "restored-before-release: hiding then re-showing the SAME control mid-press still cancels (#745 review round 9)" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                mgr ← readIORef (uiManagerRef env)
                writeIORef (uiManagerRef env) (setElementVisible eh True (setElementVisible eh False mgr))
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → aoOutcome r `shouldBe` "rejected"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "restored-before-release: disabling then re-enabling (UI.setClickable) mid-press still cancels" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                mgr ← readIORef (uiManagerRef env)
                writeIORef (uiManagerRef env) (setElementClickable eh True (setElementClickable eh False mgr))
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → aoOutcome r `shouldBe` "rejected"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "restored-before-release: detaching then re-attaching to the SAME page/position mid-press still cancels" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                mgr ← readIORef (uiManagerRef env)
                let (px, py) = pt
                writeIORef (uiManagerRef env)
                    (addElementToPage hudH eh px py (removeFromPage hudH eh mgr))
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → aoOutcome r `shouldBe` "rejected"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "restored-before-release: hiding then re-showing the control's own PAGE mid-press still cancels (menu/modal interruption)" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                mgr ← readIORef (uiManagerRef env)
                writeIORef (uiManagerRef env) (showPage hudH (hidePage hudH mgr))
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → aoOutcome r `shouldBe` "rejected"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "restored-before-release: a SEPARATE modal page appearing then disappearing over the point still cancels (#745 review round 10)" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                mgr ← readIORef (uiManagerRef env)
                -- The modal page is created and shown, then hidden
                -- again — the PRESSED control's own page/element is
                -- never directly touched, only a different page.
                let (modalH, m3) = page "modal" LayerModal mgr
                    m4 = hidePage modalH m3
                writeIORef (uiManagerRef env) m4
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → aoOutcome r `shouldBe` "rejected"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "restored-before-release: hiding then re-showing an ANCESTOR (not the pressed control itself) still cancels" $ \env → do
                resetAll env
                let (hudH, m1) = basePage
                    (parentH, m2) = createElement "container" 200 200 hudH m1
                    m3 = addElementToPage hudH parentH 0 0 m2
                    (eh, m4) = createElement "btn" 100 100 hudH m3
                    m5 = addChildElement parentH eh (fst pt) (snd pt) m4
                    m6 = setElementClickable eh True m5
                    m7 = setElementOnClick eh "btnClick" m6
                writeIORef (uiManagerRef env) m7
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Pressed]
                inputTick env
                _ ← drainLuaMsgs env
                mgr ← readIORef (uiManagerRef env)
                writeIORef (uiManagerRef env)
                    (setElementVisible parentH True (setElementVisible parentH False mgr))
                push env [InputMouseEvent GLFW.MouseButton'1 (15, 15) GLFW.MouseButtonState'Released]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → aoOutcome r `shouldBe` "rejected"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

    around withHeadlessEngine $
        describe "Lua-facing UI API for drag-activation (#745)" $ do
            it "UI.setDragActivation is callable through the real Lua UI API and leaves the element otherwise clickable (no dedicated getter — the pointer-behavior effect is covered by the wire tests above)" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                out ← evalDebug ls
                    "local pg = UI.newPage('t1', 'hud'); \
                    \local el = UI.newElement('e1', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \UI.setClickable(el, true); \
                    \UI.setOnClick(el, 'x'); \
                    \UI.setDragActivation(el, true); \
                    \return UI.getElementInfo(el).clickable"
                out `shouldBe` "true"

-- * Wire-integration helpers (mirrors Test.Headless.UI.ElementInputPolicy)

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

isUIClickEvent ∷ LuaMsg → Bool
isUIClickEvent (LuaUIClickEvent _ _ _ _) = True
isUIClickEvent _ = False

isPressBeginEvent ∷ LuaMsg → Bool
isPressBeginEvent (LuaUIPressBeginEvent _ _) = True
isPressBeginEvent _ = False

isCancel ∷ ActivationOutcome → Bool
isCancel (Cancel _) = True
isCancel _ = False

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)
