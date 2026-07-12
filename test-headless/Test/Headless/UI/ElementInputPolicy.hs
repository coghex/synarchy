{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #743 gate: the element-level input-policy contract — independent
--   click-action, pointer-blocking, and scroll-capture on a 'UIElement'
--   (A2 of #741, building on A1's #742 page-level modal boundary). Most
--   of this suite is pure 'UI.InputOwnership'/'UI.Manager.Query'
--   coverage — the acceptance explicitly wants the routing decision
--   testable without Vulkan, a window, or a running Lua engine. A
--   "wire integration" block drives the real
--   'Engine.Input.Thread.processInputs' to prove the same contract
--   holds through the actual dispatch path, and a final block proves
--   the new policy is configurable/queryable through the real Lua @UI@
--   API.
module Test.Headless.UI.ElementInputPolicy (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Test.Hspec
import Data.IORef (readIORef, writeIORef, atomicModifyIORef', newIORef)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
import UI.InputOwnership
import UI.Manager
import UI.Types

-- * Pure fixtures

-- | Create a shown page on the given layer.
page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

-- | An ordinary callback-bearing clickable control (pre-#743 compat
--   shape): 'UI.setClickable' + 'UI.setOnClick'/'setOnRightClick'.
clickableAt ∷ Text → (Float, Float) → (Float, Float) → Text → PageHandle
           → UIPageManager → (ElementHandle, UIPageManager)
clickableAt name (x, y) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
    in (eh, m4)

-- | A right-click-ONLY clickable control — no left-click callback at
--   all, proving pointer-blocking applies uniformly across buttons
--   (requirements 7/8), not just to whichever button has a handler.
rightOnlyAt ∷ Text → (Float, Float) → (Float, Float) → Text → PageHandle
           → UIPageManager → (ElementHandle, UIPageManager)
rightOnlyAt name (x, y) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnRightClick eh cb m3
    in (eh, m4)

-- | A pointer-blocking element with NO callback at all (#743's own
--   motivating case — a migrated log-panel background).
blockingAt ∷ Text → (Float, Float) → (Float, Float) → PageHandle
          → UIPageManager → (ElementHandle, UIPageManager)
blockingAt name (x, y) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementBlocksPointer eh True m2
    in (eh, m3)

-- | A scroll-capturing element with no click callback.
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

spec ∷ Spec
spec = do
    describe "routePointer — independent click/pointer-block policy (#743)" $ do
        it "a callback-bearing clickable control still receives its click (compat)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
            in case routePointer PointerLeftClick pt m2 of
                RouteElement _ cb → cb `shouldBe` "btnClick"
                other → expectationFailure ("expected RouteElement, got " ⧺ show other)

        it "a pointer-blocking element with no callback consumes a left-click without a fake callback" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
            in routePointer PointerLeftClick pt m2 `shouldBe` RouteBlocked eh

        it "a pointer-blocking element with no callback consumes a right-click without a fake callback" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
            in routePointer PointerRightClick pt m2 `shouldBe` RouteBlocked eh

        it "a right-click-only control still blocks a LEFT click over it (uniform per-element blocking)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = rightOnlyAt "equipSlot" pt (100, 100) "onEquipSlotRightClick" hudH m1
            in routePointer PointerLeftClick pt m2 `shouldBe` RouteBlocked eh

        it "...but still fires its own right-click callback normally" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = rightOnlyAt "equipSlot" pt (100, 100) "onEquipSlotRightClick" hudH m1
            in case routePointer PointerRightClick pt m2 of
                RouteElement _ cb → cb `shouldBe` "onEquipSlotRightClick"
                other → expectationFailure ("expected RouteElement, got " ⧺ show other)

        it "right-click still falls back to an ordinary left-clickable control with no right-click handler (pre-#742/#743 parity)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
            in case routePointer PointerRightClick pt m2 of
                RouteConsumedNoHandler _ leftCb → leftCb `shouldBe` "btnClick"
                other → expectationFailure ("expected RouteConsumedNoHandler, got " ⧺ show other)

        it "a disabled clickable control (ueClickable=False) that also opts into pointer-blocking does not revive its stale callback" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = clickableAt "btn" pt (100, 100) "staleCallback" hudH m1
                m3 = setElementClickable eh False m2
                m4 = setElementBlocksPointer eh True m3
            in do
                routePointer PointerLeftClick pt m4 `shouldBe` RouteBlocked eh
                -- The right-click fallback to "some ordinary left-clickable
                -- control" must ALSO respect ueClickable=False, not just the
                -- primary callback check — else a disabled button would
                -- still swallow a right-click "on behalf of" its disabled
                -- left callback.
                routePointer PointerRightClick pt m4 `shouldBe` RouteBlocked eh

        it "a passive visual element consumes no pointer button" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = visualAt "deco" pt (100, 100) hudH m1
            in do
                routePointer PointerLeftClick pt m2 `shouldBe` RouteMiss
                routePointer PointerRightClick pt m2 `shouldBe` RouteMiss

        it "accumulated paint order picks the higher of two overlapping pointer-blocking elements" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_loH, m2) = blockingAt "lo" pt (100, 100) hudH m1
                (hiH, m3) = blockingAt "hi" pt (100, 100) hudH m2
                m4 = setElementZIndex hiH 10 m3
            in routePointer PointerLeftClick pt m4 `shouldBe` RouteBlocked hiH

    describe "isPointerSurfaceBlocked — middle-click (#743)" $ do
        it "is blocked by an element with the explicit pointer-blocking opt-in" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
            in isPointerSurfaceBlocked pt m2 `shouldBe` True

        it "is blocked by an ordinary callback-bearing clickable control (compat)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
            in isPointerSurfaceBlocked pt m2 `shouldBe` True

        it "is NOT blocked by a purely visual, pass-through element (#743 behavior change from pre-#742 'any sized element')" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = visualAt "deco" pt (100, 100) hudH m1
            in isPointerSurfaceBlocked pt m2 `shouldBe` False

    describe "routeScroll (#743)" $ do
        it "a scroll-capturing element with no click callback receives wheel input" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = scrollCaptureAt "panelBg" pt (100, 100) hudH m1
            in routeScroll pt m2 `shouldBe` Just eh

        it "a scroll-capturing container wins over its own passive child visual" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = scrollCaptureAt "container" pt (100, 100) hudH m1
                (_childH, m3) = childOf containerH "childDeco" (0, 0) (100, 100) hudH m2
            in routeScroll pt m3 `shouldBe` Just containerH

        it "a non-capturing element on top does not block a lower scroll-capturing one" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (targetH, m2) = scrollCaptureAt "panelBg" pt (100, 100) hudH m1
                (topH, m3) = visualAt "decoOnTop" pt (100, 100) hudH m2
                m4 = setElementZIndex topH 10 m3
            in routeScroll pt m4 `shouldBe` Just targetH

        it "a higher scroll-capturing surface wins over an overlapping lower one" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_loH, m2) = scrollCaptureAt "lo" pt (100, 100) hudH m1
                (hiH, m3) = scrollCaptureAt "hi" pt (100, 100) hudH m2
                m4 = setElementZIndex hiH 10 m3
            in routeScroll pt m4 `shouldBe` Just hiH

        it "returns Nothing when nothing in scope captures scroll" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = visualAt "deco" pt (100, 100) hudH m1
            in routeScroll pt m2 `shouldBe` Nothing

        it "never crosses the #742 modal boundary to reach a lower scroll-capturing element" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_eh, m2) = scrollCaptureAt "panelBg" pt (100, 100) hudH m1
                (_modalH, m3) = page "modal" LayerModal m2
            in routeScroll pt m3 `shouldBe` Nothing

    describe "visibility / detachment (#743)" $ do
        it "a hidden pointer-blocking element does not block pointer input" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
                m3 = setElementVisible eh False m2
            in routePointer PointerLeftClick pt m3 `shouldBe` RouteMiss

        it "an element beneath a hidden ancestor does not block pointer input or capture scroll" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (parentH, m2) = visualAt "parent" pt (100, 100) hudH m1
                (_childH, m3) = childOf parentH "child" (0, 0) (100, 100) hudH m2
                m4 = setElementBlocksPointer _childH True m3
                m5 = setElementCapturesScroll _childH True m4
                m6 = setElementVisible parentH False m5
            in do
                routePointer PointerLeftClick pt m6 `shouldBe` RouteMiss
                routeScroll pt m6 `shouldBe` Nothing

        it "a detached element (never added to any page) does not block pointer input or capture scroll" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = createElement "detached" 100 100 hudH m1
                m3 = setElementBlocksPointer eh True m2
                m4 = setElementCapturesScroll eh True m3
            in do
                routePointer PointerLeftClick pt m4 `shouldBe` RouteMiss
                routeScroll pt m4 `shouldBe` Nothing

        it "an element on a hidden (never-shown) page does not block pointer input or capture scroll" $
            let (hudH, m1) = createPage "hud" LayerHUD emptyUIPageManager  -- NOT shown
                (eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
                (eh2, m3) = scrollCaptureAt "scrollBg" pt (100, 100) hudH m2
            in do
                routePointer PointerLeftClick pt m3 `shouldBe` RouteMiss
                routeScroll pt m3 `shouldBe` Nothing
                eh `shouldNotBe` eh2 -- sanity: two distinct elements were created

    describe "no fake scroll-only click callbacks remain (#743 audit)" $ do
        let migratedFiles =
                [ "scripts/combat_log.lua"
                , "scripts/injury_log_panel.lua"
                , "scripts/unit_log.lua"
                , "scripts/event_log.lua"
                , "scripts/settings_menu.lua"
                , "scripts/create_world/log_panel.lua"
                ]

        it "every migrated log/settings panel uses the explicit scroll-capture + pointer-block contract" $ do
            sources ← mapM TIO.readFile migratedFiles
            mapM_ (\src → T.isInfixOf "UI.setScrollCapture" src `shouldBe` True) sources
            mapM_ (\src → T.isInfixOf "UI.setPointerBlocking" src `shouldBe` True) sources

        -- Review round 2: any element that relied on the OLD
        -- clickable+onClick-implies-wheel-eligible coupling for a REAL
        -- (not dead/no-op) feature — a list row, a scrollbar button/tab,
        -- a dropdown's option list, the unit-info stats panel body —
        -- needs the explicit scroll-capture opt-in restored at the
        -- shared-widget level, or wheel-over-that-widget silently
        -- regresses to world zoom.
        it "shared widgets that relied on the old click-implies-wheel coupling keep explicit scroll-capture" $ do
            sources ← mapM TIO.readFile
                [ "scripts/ui/list.lua"
                , "scripts/ui/scrollbar.lua"
                , "scripts/ui/dropdown.lua"
                , "scripts/unit_info_v2_panel_engine.lua"
                ]
            mapM_ (\src → T.isInfixOf "UI.setScrollCapture" src `shouldBe` True) sources

        it "the dead onTabFrameScroll/onLogPanelScroll no-op click callbacks are gone" $ do
            panelsSource ← TIO.readFile "scripts/ui_manager_panels.lua"
            settingsSource ← TIO.readFile "scripts/settings_menu.lua"
            logPanelSource ← TIO.readFile "scripts/create_world/log_panel.lua"
            -- The FUNCTION DEFINITIONS and their string-literal
            -- registrations (the actual dead-code shapes) are gone;
            -- historical mentions in migration comments are fine.
            T.isInfixOf "function uiManager.onTabFrameScroll" panelsSource `shouldBe` False
            T.isInfixOf "function uiManager.onLogPanelScroll" panelsSource `shouldBe` False
            T.isInfixOf "\"onTabFrameScroll\"" settingsSource `shouldBe` False
            T.isInfixOf "\"onLogPanelScroll\"" logPanelSource `shouldBe` False

    -- Wire-level integration: the same contract, but proven through the
    -- REAL 'Engine.Input.Thread.processInputs' dispatch (mirrors
    -- Test.Headless.UI.InputOwnership's own wire-integration block).
    around withHeadlessEngine $
        describe "wire integration (Engine.Input.Thread) — #743" $ do
            it "a pointer-blocking element with no callback: no Lua click event, focus clears, and outcome records a UI no-op" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env [InputMouseEvent GLFW.MouseButton'1 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Pressed]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem LuaUIFocusLost
                msgs `shouldSatisfy` all (not ∘ isMouseDownEvent)
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "noop"
                        aoHandler r `shouldBe` Just "ui_pointer_block"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

            it "press/release routing stays paired after a no-callback pointer-blocking consumption" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env
                    [ InputMouseEvent GLFW.MouseButton'1 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Pressed
                    , InputMouseEvent GLFW.MouseButton'1 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Released
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` any (≡ LuaMouseUpEvent GLFW.MouseButton'1 (realToFrac px) (realToFrac py) ClickSwallowed)

            it "a right-click on an ordinary left-clickable control never invokes its left-click callback" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_eh, m2) = clickableAt "btn" pt (100, 100) "btnClick" hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env [InputMouseEvent GLFW.MouseButton'2 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Pressed]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                msgs `shouldSatisfy` all (not ∘ isUIRightClickEvent)

            it "a middle-click over a pointer-blocking element is swallowed (no camera-drag routing)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_eh, m2) = blockingAt "logPanelBg" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env [InputMouseEvent GLFW.MouseButton'3 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Pressed]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isMouseDownEvent)

            it "a middle-click over a purely visual, pass-through element still reaches gameplay (#743 behavior change)" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (_eh, m2) = visualAt "deco" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env [InputMouseEvent GLFW.MouseButton'3 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Pressed]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` any isMouseDownEvent

            it "a scroll-capturing element with no click callback receives the real LuaUIScrollEvent broadcast" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    (eh, m2) = scrollCaptureAt "panelBg" pt (100, 100) hudH m1
                writeIORef (uiManagerRef env) m2
                let (px, py) = pt
                push env
                    [ InputCursorMove (realToFrac px) (realToFrac py)
                    , InputScrollEvent 0 1
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaUIScrollEvent eh 0 1)
                msgs `shouldSatisfy` all (not ∘ isScrollEvent)

            it "wheel over empty space still routes to game scroll (camera zoom)" $ \env → do
                resetAll env
                let (px, py) = pt
                push env
                    [ InputCursorMove (realToFrac px) (realToFrac py)
                    , InputScrollEvent 0 1
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaScrollEvent 0 1)

    -- #743 Lua-facing coverage: the new element policy can be
    -- configured and queried through the real UI API without any
    -- graphical engine (mirrors InputOwnership.hs's newBareLuaBackend
    -- pattern).
    around withHeadlessEngine $
        describe "Lua-facing UI API for the new element policy (#743)" $ do
            it "UI.setPointerBlocking / UI.isPointerBlocking round-trip through the real Lua UI API" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                before' ← evalDebug ls
                    "local pg = UI.newPage('t1', 'hud'); \
                    \local el = UI.newElement('e1', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \_G.__el1 = el; \
                    \return UI.isPointerBlocking(el)"
                before' `shouldBe` "false"
                after' ← evalDebug ls
                    "UI.setPointerBlocking(_G.__el1, true); return UI.isPointerBlocking(_G.__el1)"
                after' `shouldBe` "true"

            it "UI.setScrollCapture / UI.isScrollCapturing round-trip through the real Lua UI API" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                before' ← evalDebug ls
                    "local pg = UI.newPage('t2', 'hud'); \
                    \local el = UI.newElement('e2', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \_G.__el2 = el; \
                    \return UI.isScrollCapturing(el)"
                before' `shouldBe` "false"
                after' ← evalDebug ls
                    "UI.setScrollCapture(_G.__el2, true); return UI.isScrollCapturing(_G.__el2)"
                after' `shouldBe` "true"

            it "registering a click callback keeps blocking pointer input by default, with no explicit flag needed (compat)" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                out ← evalDebug ls
                    "local pg = UI.newPage('t3', 'hud'); \
                    \local el = UI.newElement('e3', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \UI.setClickable(el, true); \
                    \UI.setOnClick(el, 'someCallback'); \
                    \return UI.isPointerBlocking(el)"
                out `shouldBe` "true"

-- * Wire-integration helpers (mirrors Test.Headless.UI.InputOwnership)

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

isMouseDownEvent ∷ LuaMsg → Bool
isMouseDownEvent (LuaMouseDownEvent _ _ _) = True
isMouseDownEvent _ = False

isUIClickEvent ∷ LuaMsg → Bool
isUIClickEvent (LuaUIClickEvent _ _ _ _) = True
isUIClickEvent _ = False

isUIRightClickEvent ∷ LuaMsg → Bool
isUIRightClickEvent (LuaUIRightClickEvent _ _ _ _) = True
isUIRightClickEvent _ = False

isScrollEvent ∷ LuaMsg → Bool
isScrollEvent (LuaScrollEvent _ _) = True
isScrollEvent _ = False

-- | A real Lua backend with the FULL Lua API registered and nothing
--   else loaded — callers drive it with 'evalDebug' snippets calling
--   the real @UI.*@ functions, exactly as any script would.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses ('executeDebugLua').
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)
