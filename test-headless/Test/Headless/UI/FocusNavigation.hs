{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #745 gate (Phase B child B1 of #741): keyboard CONTROL focus and
--   Tab/Shift+Tab traversal — distinct from the pre-existing text-
--   input focus. Most of this suite is pure 'UI.FocusNavigation'
--   coverage (traversal/eligibility/repair testable without Vulkan, a
--   window, or a running Lua engine); a "wire integration" block
--   drives the real 'Engine.Input.Thread.processInputs', and a final
--   block proves keyboard activation reaches REAL loaded Lua modules
--   across distinct callback families via
--   @scripts/control_activation_fixture.lua@ (the review's testability
--   correction: decisions live engine-side and are pure-tested above;
--   cross-family activation coverage may use a real headless Lua
--   block).
module Test.Headless.UI.FocusNavigation (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import qualified HsLua as Lua
import Test.Hspec
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef', newIORef)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Concurrent.STM (atomically, modifyTVar')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Input.Bindings (defaultKeyBindings)
import Engine.Input.Inject (noMods)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Script (loadModuleRef)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsgs)
import Engine.Scripting.Lua.Types (LuaMsg(..), LuaBackendState(..), LuaScript(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Focus (createFocusManager, FocusId(..), registerFocusTarget, setFocus)
import UI.FocusNavigation
import UI.Manager
import UI.Types

-- * Pure fixtures

page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

focusableAt ∷ Text → Float → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
focusableAt name x pageH mgr =
    let (eh, m1) = createElement name 20 20 pageH mgr
        m2 = addElementToPage pageH eh x 0 m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh (name <> "Click") m3
    in (eh, m4)

textFieldAt ∷ Text → Float → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
textFieldAt name x pageH mgr =
    let (eh, m1) = createElement name 20 20 pageH mgr
        m2 = addElementToPage pageH eh x 0 m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh (name <> "Click") m3
        m5 = enableTextInput eh m4
    in (eh, m5)

spec ∷ Spec
spec = do
    describe "focusableElements / traversal (pure, #745)" $ do
        it "orders focusable controls by paint/read order" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
                (c, m4) = focusableAt "c" 60 hudH m3
            focusableElements m4 `shouldBe` [a, b, c]

        it "excludes a hidden control" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
                m4 = setElementVisible b False m3
            focusableElements m4 `shouldBe` [a]

        it "excludes a disabled (non-clickable) control" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
                m4 = setElementClickable b False m3
            focusableElements m4 `shouldBe` [a]

        it "excludes a detached control" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
                m4 = removeFromPage hudH b m3
            focusableElements m4 `shouldBe` [a]

        it "excludes a text-input field — it stays on the separate text-focus system" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (_field, m3) = textFieldAt "field" 30 hudH m2
            focusableElements m3 `shouldBe` [a]

        it "excludes a control on a page below the modal boundary, but keeps a debug-layer control reachable above it" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_below, m2) = focusableAt "below" 0 hudH m1
                (modalH, m3) = page "modal" LayerModal m2
                (inModal, m4) = focusableAt "inModal" 0 modalH m3
                (debugH, m5) = page "debug" LayerDebug m4
                (inDebug, m6) = focusableAt "inDebug" 0 debugH m5
            focusableElements m6 `shouldBe` [inModal, inDebug]

        it "excludes a non-steppable drag-activation control (a dead Tab stop with no keyboard action) but keeps a steppable one (review round 5)" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (track, m2') = focusableAt "track" 0 hudH m1
                m2 = setElementDragActivation track True m2'
                (knob, m3') = focusableAt "knob" 30 hudH m2
                m3 = setElementSteppable knob True (setElementDragActivation knob True m3')
            focusableElements m3 `shouldBe` [knob]

        it "next/prev traverse forward/backward and wrap at either end" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
                (c, m4) = focusableAt "c" 60 hudH m3
            nextFocus m4 Nothing `shouldBe` Just a
            nextFocus m4 (Just a) `shouldBe` Just b
            nextFocus m4 (Just c) `shouldBe` Just a
            prevFocus m4 Nothing `shouldBe` Just c
            prevFocus m4 (Just a) `shouldBe` Just c
            prevFocus m4 (Just c) `shouldBe` Just b

        it "an explicit tab index slots a control anywhere in the natural flow" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
                (c, m4) = focusableAt "c" 60 hudH m3
                -- Natural order is a,b,c (paint order); force c first.
                m5 = setElementTabIndex c (-1) m4
            focusableElements m5 `shouldBe` [c, a, b]

        it "an empty focusable set yields Nothing rather than a ghost" $ do
            nextFocus emptyUIPageManager Nothing `shouldBe` Nothing
            prevFocus emptyUIPageManager (Just (ElementHandle 1)) `shouldBe` Nothing

        it "focus repair: a stale (no longer eligible) current handle is treated like no focus — Tab lands on the first eligible control" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
                m4 = setElementClickable b False m3
            nextFocus m4 (Just b) `shouldBe` Just a

        it "validateControlFocus clears a hidden/deleted/disabled focus and keeps a still-eligible one" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
            validateControlFocus m2 (Just a) `shouldBe` Just a
            validateControlFocus m2 Nothing `shouldBe` Nothing
            validateControlFocus (setElementVisible a False m2) (Just a) `shouldBe` Nothing
            validateControlFocus (deleteElement a m2) (Just a) `shouldBe` Nothing
            validateControlFocus (setElementClickable a False m2) (Just a) `shouldBe` Nothing

        it "deleting or detaching a focused control proactively clears control focus, mirroring upmGlobalFocus's own hygiene (review round 3)" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                (b, m3) = focusableAt "b" 30 hudH m2
            getControlFocus (deleteElement a (setControlFocus a m3)) `shouldBe` Nothing
            getControlFocus (removeFromPage hudH b (setControlFocus b m3)) `shouldBe` Nothing
            getControlFocus (removeElement b (setControlFocus b m3)) `shouldBe` Nothing

        it "hiding a focused control's page proactively clears control focus (mirrors upmGlobalFocus's own hidePage hygiene); showing it again never resurrects it (review round 3)" $ do
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = focusableAt "a" 0 hudH m1
                m3 = setControlFocus a m2
            getControlFocus m3 `shouldBe` Just a
            -- hidePage clears it immediately — not lazily, at the next
            -- keyboard dispatch's validation pass.
            let m4 = hidePage hudH m3
            getControlFocus m4 `shouldBe` Nothing
            -- Showing the page again never restores it: nothing
            -- remembers "a was focused before the hide", so it stays
            -- cleared even though `a` is eligible again.
            let m5 = showPage hudH m4
            getControlFocus m5 `shouldBe` Nothing

    around withHeadlessEngine $
        describe "wire integration (Engine.Input.Thread) — #745" $ do
            it "clicking an eligible control also moves keyboard control focus to it, notifying Lua of each real transition (review rounds 1 & 2)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                    (b, m3) = focusableAt "b" 30 hudH m2
                writeIORef (uiManagerRef env) m3
                click env (5, 5)
                mgrAfterA ← readIORef (uiManagerRef env)
                getControlFocus mgrAfterA `shouldBe` Just a
                msgsA ← drainLuaMsgs env
                msgsA `shouldSatisfy` elem (LuaUIControlFocusChanged (Just a))
                -- Clicking the SAME already-focused control again is
                -- not a real transition — no redundant notification.
                click env (5, 5)
                msgsARepeat ← drainLuaMsgs env
                msgsARepeat `shouldSatisfy` all (not ∘ isControlFocusChanged)
                -- Clicking a DIFFERENT eligible control moves focus to
                -- it, not just the first Tab default.
                click env (35, 5)
                mgrAfterB ← readIORef (uiManagerRef env)
                getControlFocus mgrAfterB `shouldBe` Just b
                msgsB ← drainLuaMsgs env
                msgsB `shouldSatisfy` elem (LuaUIControlFocusChanged (Just b))
                -- Clicking empty space clears it (mirrors how the same
                -- click already clears the pre-existing text focus).
                click env (900, 900)
                mgrAfterMiss ← readIORef (uiManagerRef env)
                getControlFocus mgrAfterMiss `shouldBe` Nothing
                msgsMiss ← drainLuaMsgs env
                msgsMiss `shouldSatisfy` elem (LuaUIControlFocusChanged Nothing)

            it "clicking a text field clears control focus rather than leaving it stale" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                    (_field, m3) = textFieldAt "field" 30 hudH m2
                writeIORef (uiManagerRef env) (setControlFocus a m3)
                click env (35, 5)
                mgr ← readIORef (uiManagerRef env)
                getControlFocus mgr `shouldBe` Nothing

            it "Tab with no focus focuses the first eligible control; Shift+Tab focuses the last" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                    (_b, m3) = focusableAt "b" 30 hudH m2
                    (c, m4) = focusableAt "c" 60 hudH m3
                writeIORef (uiManagerRef env) m4
                pressKey env GLFW.Key'Tab noMods
                mgrAfterTab ← readIORef (uiManagerRef env)
                getControlFocus mgrAfterTab `shouldBe` Just a
                writeIORef (uiManagerRef env) m4
                pressKey env GLFW.Key'Tab (shiftMods)
                mgrAfterShiftTab ← readIORef (uiManagerRef env)
                getControlFocus mgrAfterShiftTab `shouldBe` Just c
                -- Tab is consumed here too — never also broadcast to
                -- gameplay onKeyDown (review round 2).
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isKeyDownEvent)

            it "Tab traverses forward across repeated presses, wrapping back to the first" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                    (_b, m3) = focusableAt "b" 30 hudH m2
                writeIORef (uiManagerRef env) m3
                pressKey env GLFW.Key'Tab noMods
                pressKey env GLFW.Key'Tab noMods
                pressKey env GLFW.Key'Tab noMods
                mgr ← readIORef (uiManagerRef env)
                -- a → b → a (wrap)
                getControlFocus mgr `shouldBe` Just a

            it "Escape clears control focus" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_a, m2) = focusableAt "a" 0 hudH m1
                writeIORef (uiManagerRef env) m2
                pressKey env GLFW.Key'Tab noMods
                mgrFocused ← readIORef (uiManagerRef env)
                getControlFocus mgrFocused `shouldSatisfy` isJust
                pressKey env GLFW.Key'Escape noMods
                mgrAfter ← readIORef (uiManagerRef env)
                getControlFocus mgrAfter `shouldBe` Nothing

            it "an invalidated focused control is repaired (validated) on the next keyboard dispatch, never left as a ghost" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                writeIORef (uiManagerRef env) (setControlFocus a m2)
                _ ← atomicModifyIORef' (uiManagerRef env) $ \mgr →
                    (setElementVisible a False mgr, ())
                pressKey env GLFW.Key'W noMods  -- any ordinary key dispatch validates
                mgr ← readIORef (uiManagerRef env)
                getControlFocus mgr `shouldBe` Nothing

            it "shell text focus takes priority — Tab does not move control focus while the shell has text focus" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_a, m2) = focusableAt "a" 0 hudH m1
                writeIORef (uiManagerRef env) m2
                _fid ← shellFocus env
                pressKey env GLFW.Key'Tab noMods
                mgr ← readIORef (uiManagerRef env)
                getControlFocus mgr `shouldBe` Nothing
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` any isTabPressed

            it "UI text-input focus also takes priority over control focus" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_a, m2) = focusableAt "a" 0 hudH m1
                    (field, m3) = textFieldAt "field" 30 hudH m2
                    m4 = setElementFocus field m3
                writeIORef (uiManagerRef env) m4
                pressKey env GLFW.Key'Tab noMods
                mgr ← readIORef (uiManagerRef env)
                getControlFocus mgr `shouldBe` Nothing

            it "Enter activates the focused control through its onClick callback (LuaUIClickEvent), withholds Enter from gameplay key-state polling, and never also broadcasts it to gameplay onKeyDown (review round 2)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                writeIORef (uiManagerRef env) (setControlFocus a m2)
                pressKey env GLFW.Key'Enter noMods
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIClickEvent a "aClick" 0 0)
                -- Mirrors the pre-existing text-focus precedent (its
                -- branches never reach the onKeyDown broadcast either)
                -- — a control-focus-consumed key must not ALSO reach
                -- gameplay's onKeyDown handlers.
                msgs `shouldSatisfy` all (not ∘ isKeyDownEvent)
                st ← readIORef (inputStateRef env)
                Map.lookup GLFW.Key'Enter (inpKeyStates st) `shouldBe` Nothing

            it "an unconsumed key (nothing control-focused) still reaches gameplay onKeyDown exactly as before #745" $ \env → do
                resetAll env
                pressKey env GLFW.Key'Enter noMods
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` any isKeyDownEvent

            it "Space also activates the focused control" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                writeIORef (uiManagerRef env) (setControlFocus a m2)
                pressKey env GLFW.Key'Space noMods
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIClickEvent a "aClick" 0 0)

            it "arrow keys step a steppable focused control (slider) and are withheld from camera-pan key-state polling" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2') = focusableAt "knob" 0 hudH m1
                    m2 = setElementSteppable a True m2'
                writeIORef (uiManagerRef env) (setControlFocus a m2)
                pressKey env GLFW.Key'Right noMods
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIStepEvent a 1)
                msgs `shouldSatisfy` all (not ∘ isKeyDownEvent)
                st ← readIORef (inputStateRef env)
                Map.lookup GLFW.Key'Right (inpKeyStates st) `shouldBe` Nothing

            it "arrow keys do NOT step (or get withheld from) a focused control that isn't steppable — ordinary camera-pan keeps working" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2) = focusableAt "a" 0 hudH m1
                writeIORef (uiManagerRef env) (setControlFocus a m2)
                pressKey env GLFW.Key'Right noMods
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isStepEvent)
                st ← readIORef (inputStateRef env)
                Map.lookup GLFW.Key'Right (inpKeyStates st) `shouldSatisfy` isJust

            it "a held steppable arrow stays suppressed through Repeating and Released — no leaked gameplay onKeyDown/onKeyUp or resumed key-state polling (review round 3)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (a, m2') = focusableAt "knob" 0 hudH m1
                    m2 = setElementSteppable a True m2'
                writeIORef (uiManagerRef env) (setControlFocus a m2)
                push env [InputKeyEvent GLFW.Key'Right GLFW.KeyState'Pressed noMods]
                inputTick env
                msgsPress ← drainLuaMsgs env
                msgsPress `shouldSatisfy` elem (LuaUIStepEvent a 1)
                msgsPress `shouldSatisfy` all (not ∘ isKeyDownEvent)
                st1 ← readIORef (inputStateRef env)
                inpControlFocusConsumedKeys st1 `shouldBe` Set.singleton GLFW.Key'Right
                -- Held: GLFW's own auto-repeat, a SEPARATE dispatch —
                -- must stay suppressed (no gameplay leak, no resumed
                -- camera-pan polling) even though it doesn't re-fire
                -- its own step (only a fresh Pressed does that).
                push env [InputKeyEvent GLFW.Key'Right GLFW.KeyState'Repeating noMods]
                inputTick env
                msgsRepeat ← drainLuaMsgs env
                msgsRepeat `shouldSatisfy` all (not ∘ isKeyDownEvent)
                st2 ← readIORef (inputStateRef env)
                Map.lookup GLFW.Key'Right (inpKeyStates st2) `shouldBe` Nothing
                -- Release: no unpaired LuaKeyUpEvent (its matching
                -- key-down never reached gameplay either), and the
                -- tracking set clears so a later, unrelated press on
                -- this key isn't permanently suppressed.
                push env [InputKeyEvent GLFW.Key'Right GLFW.KeyState'Released noMods]
                inputTick env
                msgsRelease ← drainLuaMsgs env
                msgsRelease `shouldSatisfy` all (not ∘ isKeyUpEvent)
                st3 ← readIORef (inputStateRef env)
                inpControlFocusConsumedKeys st3 `shouldSatisfy` Set.null

            it "Enter/Space never activates a drag-activation control's onClick (it would latch a keyboard-triggered drag); arrow stepping on the same control still works (review round 3)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (knob, m2') = focusableAt "knob" 0 hudH m1
                    m2 = setElementSteppable knob True (setElementDragActivation knob True m2')
                writeIORef (uiManagerRef env) (setControlFocus knob m2)
                pressKey env GLFW.Key'Enter noMods
                msgsEnter ← drainLuaMsgs env
                msgsEnter `shouldSatisfy` all (not ∘ isUIClickEvent)
                pressKey env GLFW.Key'Right noMods
                msgsArrow ← drainLuaMsgs env
                msgsArrow `shouldSatisfy` elem (LuaUIStepEvent knob 1)

            it "introspection: UI.getElementInfo reports controlFocused distinctly from the pre-existing text-focus field" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                -- Built entirely Lua-side (mirrors ElementInputPolicy's
                -- newBareLuaBackend pattern) so no Haskell-side handle
                -- ever needs interpolating into the Lua snippet.
                let setup ∷ Text
                    setup =
                        "local pg = UI.newPage('t1', 'hud'); \
                        \local el = UI.newElement('e1', 10, 10, pg); \
                        \UI.addToPage(pg, el, 0, 0); \
                        \UI.setClickable(el, true); \
                        \UI.setOnClick(el, 'x'); \
                        \UI.setControlFocus(el); \
                        \_G.__el = el; "
                _ ← evalDebug ls (setup `T.append` "return true")
                controlFocused ← evalDebug ls "return UI.getElementInfo(_G.__el).controlFocused"
                textFocused ← evalDebug ls "return UI.getElementInfo(_G.__el).focused"
                controlFocused `shouldBe` "true"
                textFocused `shouldBe` "false"

            it "Phase A regression: the modal boundary still blocks pointer clicks exactly as #742/#743 established" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_below, m2) = focusableAt "below" 0 hudH m1
                    (_modalH, m3) = page "modal" LayerModal m2
                writeIORef (uiManagerRef env) m3
                push env
                    [ InputMouseEvent GLFW.MouseButton'1 (10, 10) GLFW.MouseButtonState'Pressed
                    , InputMouseEvent GLFW.MouseButton'1 (10, 10) GLFW.MouseButtonState'Released
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)

    around withHeadlessEngine $
        describe "Lua-facing UI.hidePage control-focus notification (#745 review round 4)" $ do
            it "UI.hidePage(p) clears control focus on a control it owns and reports the transition, through the real Lua API" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                _ ← evalDebug ls
                    "local pg = UI.newPage('t1', 'hud'); \
                    \local el = UI.newElement('e1', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \UI.setClickable(el, true); \
                    \UI.setOnClick(el, 'x'); \
                    \UI.setControlFocus(el); \
                    \_G.__pg1 = pg; _G.__el1b = el; \
                    \return true"
                before ← evalDebug ls "return UI.getControlFocus() == _G.__el1b"
                before `shouldBe` "true"
                _ ← evalDebug ls "UI.hidePage(_G.__pg1); return true"
                after ← evalDebug ls "return UI.getControlFocus() == nil"
                after `shouldBe` "true"

            it "the notification itself reaches the engine-to-Lua queue as LuaUIControlFocusChanged Nothing" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                _ ← evalDebug ls
                    "local pg = UI.newPage('t2', 'hud'); \
                    \local el = UI.newElement('e2', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \UI.setClickable(el, true); \
                    \UI.setOnClick(el, 'x'); \
                    \UI.setControlFocus(el); \
                    \_G.__pg2 = pg; \
                    \return true"
                _ ← drainLuaMsgs env
                _ ← evalDebug ls "UI.hidePage(_G.__pg2); return true"
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIControlFocusChanged Nothing)

    around withHeadlessEngine $
        describe "keyboard activation across real Lua widget families (#745, review testability correction)" $ do
            it "Enter/Space activation reaches real loaded Lua callbacks across distinct widget-family names" $ \env → do
                resetAll env
                (ls, stateRef) ← newFixtureLuaBackend env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    families =
                        [ "onFixtureCheckboxClick", "onFixtureToggleClick"
                        , "onFixtureTabClick", "onFixtureListClick"
                        , "onFixtureDropdownClick"
                        ]
                    (handles, mFinal) = foldl'
                        (\(hs, m) (i, nm) →
                            let (eh, m') = createElement nm 20 20 hudH m
                                m2' = addElementToPage hudH eh (fromIntegral i * 25) 0 m'
                                m3' = setElementClickable eh True m2'
                                m4' = setElementOnClick eh nm m3'
                            in (hs ⧺ [eh], m4'))
                        ([], m1)
                        (zip [0 ∷ Int ..] families)
                writeIORef (uiManagerRef env) mFinal
                forM_ handles $ \h → do
                    mgr' ← readIORef (uiManagerRef env)
                    writeIORef (uiManagerRef env) (setControlFocus h mgr')
                    pressKey env GLFW.Key'Enter noMods
                    processLuaMsgs env ls stateRef
                out ← evalDebugFixture ls
                    "local M = package.loaded['scripts.control_activation_fixture']; \
                    \return M.state.checkboxClicks + M.state.toggleClicks \
                    \+ M.state.tabClicks + M.state.listClicks + M.state.dropdownClicks"
                -- Each of the 5 distinct callback names fired exactly
                -- once (5 * 1 = 5) — a miscount anywhere (a family
                -- never reached, or double-fired) would move this off
                -- 5 in either direction.
                out `shouldBe` "5"

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

pressKey ∷ EngineEnv → GLFW.Key → GLFW.ModifierKeys → IO ()
pressKey env key mods = do
    push env [InputKeyEvent key GLFW.KeyState'Pressed mods]
    inputTick env

click ∷ EngineEnv → (Double, Double) → IO ()
click env pos = do
    push env
        [ InputMouseEvent GLFW.MouseButton'1 pos GLFW.MouseButtonState'Pressed
        , InputMouseEvent GLFW.MouseButton'1 pos GLFW.MouseButtonState'Released
        ]
    inputTick env

shiftMods ∷ GLFW.ModifierKeys
shiftMods = noMods { GLFW.modifierKeysShift = True }

shellFocus ∷ EngineEnv → IO FocusId
shellFocus env = do
    let (fid, fm1) = registerFocusTarget True 0 createFocusManager
        fm2 = setFocus fid fm1
    writeIORef (focusManagerRef env) fm2
    pure fid

drainLuaMsgs ∷ EngineEnv → IO [LuaMsg]
drainLuaMsgs env = go []
  where
    go acc = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Just msg → go (msg : acc)
            Nothing  → pure (reverse acc)

isUIClickEvent ∷ LuaMsg → Bool
isUIClickEvent (LuaUIClickEvent _ _ _ _) = True
isUIClickEvent _ = False

isTabPressed ∷ LuaMsg → Bool
isTabPressed (LuaTabPressed _) = True
isTabPressed _ = False

isStepEvent ∷ LuaMsg → Bool
isStepEvent (LuaUIStepEvent _ _) = True
isStepEvent _ = False

isControlFocusChanged ∷ LuaMsg → Bool
isControlFocusChanged (LuaUIControlFocusChanged _) = True
isControlFocusChanged _ = False

isKeyDownEvent ∷ LuaMsg → Bool
isKeyDownEvent (LuaKeyDownEvent _ _) = True
isKeyDownEvent _ = False

isKeyUpEvent ∷ LuaMsg → Bool
isKeyUpEvent (LuaKeyUpEvent _) = True
isKeyUpEvent _ = False

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

-- | A real Lua backend with the FULL API registered AND
--   @scripts/control_activation_fixture.lua@ loaded as a real script
--   (so 'Engine.Scripting.Lua.Thread.Dispatch.processLuaMsgs'
--   broadcasts actually reach it) — mirrors
--   "Test.Headless.Input.Followup"'s @newTestLuaBackend@.
newFixtureLuaBackend ∷ EngineEnv → IO (LuaBackendState, IORef ThreadControl)
newFixtureLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    eRef ← Lua.runWith (lbsLuaState ls) $
        loadModuleRef "scripts/control_activation_fixture.lua"
    ref ← case eRef of
        Right r → pure r
        Left err → error $
            "failed to load scripts/control_activation_fixture.lua: " ⧺ T.unpack err
    atomically $ modifyTVar' (lbsScripts ls) $ Map.insert 1 LuaScript
        { scriptId        = 1
        , scriptPath      = "scripts/control_activation_fixture.lua"
        , scriptTickRate  = 1000000
        , scriptNextTick  = 1000000
        , scriptModuleRef = ref
        , scriptPaused    = False
        }
    pure (ls, stateRef)

evalDebugFixture ∷ LuaBackendState → Text → IO Text
evalDebugFixture = evalDebug
