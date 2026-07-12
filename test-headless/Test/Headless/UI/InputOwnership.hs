{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #742 gate: the modal-boundary contract (page-level pointer
--   ownership + the gameplay-blocked predicate). Most of this suite is
--   pure 'UI.InputOwnership'/'UI.Manager' coverage — the acceptance
--   criteria explicitly want the routing decision testable without
--   Vulkan, a window, or a running Lua engine. A small "wire
--   integration" block at the end drives the real
--   'Engine.Input.Thread.processInputs' to prove the same contract
--   holds through the actual dispatch path, not just the pure
--   function in isolation.
module Test.Headless.UI.InputOwnership (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Test.Hspec
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Input.Bindings (defaultKeyBindings)
import Engine.Input.Inject (noMods)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Focus (FocusId, createFocusManager, registerFocusTarget, setFocus, unFocusId)
import UI.InputOwnership
import UI.Manager
import UI.Types

-- * Pure fixtures

-- | Create a shown page on the given layer.
page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

-- | Add a clickable+right-clickable element (both callbacks the same
--   name, unless a test only cares about one) at a given rect on a page.
clickableAt ∷ Text → (Float, Float) → (Float, Float) → Text → PageHandle
           → UIPageManager → UIPageManager
clickableAt name (x, y) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
        m5 = setElementOnRightClick eh cb m4
    in m5

pt ∷ (Float, Float)
pt = (50, 50)

elsewhere ∷ (Float, Float)
elsewhere = (999, 999)

spec ∷ Spec
spec = do
    describe "routePointer — modal boundary" $ do
        let mgrModalWins =
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    m2 = clickableAt "hudBtn" pt (100, 100) "hudClick" hudH m1
                    (modalH, m3) = page "modal" LayerModal m2
                    m4 = clickableAt "modalBtn" pt (100, 100) "modalClick" modalH m3
                in m4

        it "a modal's own control wins over an overlapping lower HUD control" $
            case routePointer PointerLeftClick pt mgrModalWins of
                RouteElement _ cb → cb `shouldBe` "modalClick"
                other → expectationFailure ("expected the modal's control, got " ⧺ show other)

        let mgrModalBlocks =
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    m2 = clickableAt "hudBtn" pt (100, 100) "hudClick" hudH m1
                    (_modalH, m3) = page "modal" LayerModal m2
                    -- modal page is visible + exclusive by default, but
                    -- has no control of its own at `pt`
                in m3

        it "empty modal space blocks a lower HUD control (left-click)" $
            routePointer PointerLeftClick pt mgrModalBlocks `shouldBe` RouteMiss
        it "empty modal space blocks a lower HUD control (right-click)" $
            routePointer PointerRightClick pt mgrModalBlocks `shouldBe` RouteMiss
        it "empty modal space blocks a lower HUD control (wheel)" $
            routePointer PointerWheel pt mgrModalBlocks `shouldBe` RouteMiss

    describe "debug/shell pass-through above a modal" $ do
        it "a control on a debug-layer page still receives input above a modal, regardless of creation order" $
            let (debugH, m1) = page "shell" LayerDebug emptyUIPageManager
                m2 = clickableAt "debugBtn" pt (100, 100) "debugClick" debugH m1
                -- Created AFTER the debug page (higher PageHandle), so a
                -- boundary computed off creation order instead of layer
                -- would wrongly treat the modal as "on top".
                (_modalH, m3) = page "modal" LayerModal m2
            in case routePointer PointerLeftClick pt m3 of
                RouteElement _ cb → cb `shouldBe` "debugClick"
                other → expectationFailure ("expected the debug control, got " ⧺ show other)

        let debugAndHudFixture withModal =
                let (debugH, m1) = page "shell" LayerDebug emptyUIPageManager
                    m2 = clickableAt "debugBtn" pt (100, 100) "debugClick" debugH m1
                    m3 = if withModal then snd (page "modal" LayerModal m2) else m2
                    (hudH, m4) = page "hud" LayerHUD m3
                    m5 = clickableAt "hudBtn" elsewhere (100, 100) "hudClick" hudH m4
                in m5

        it "a miss on the debug page continues to the modal boundary but does not cross it" $
            routePointer PointerLeftClick elsewhere (debugAndHudFixture True) `shouldBe` RouteMiss

        it "a miss on the debug page reaches a lower HUD control when no modal is visible" $
            case routePointer PointerLeftClick elsewhere (debugAndHudFixture False) of
                RouteElement _ cb → cb `shouldBe` "hudClick"
                other → expectationFailure ("expected the HUD control, got " ⧺ show other)

    describe "notification-card pass-through (popup.lua parity)" $ do
        let passThroughModal =
                let (h, m1) = page "popups" LayerModal emptyUIPageManager
                    m2 = setPageInputExclusive h False m1
                in (h, m2)

        it "a pass-through modal-layer page's own control still receives input on itself" $
            let (h, m1) = passThroughModal
                m2 = clickableAt "closeCard" pt (30, 30) "dismissCard" h m1
            in case routePointer PointerLeftClick pt m2 of
                RouteElement _ cb → cb `shouldBe` "dismissCard"
                other → expectationFailure ("expected the card's control, got " ⧺ show other)

        it "a miss on a pass-through modal-layer page reaches a lower HUD control" $
            let (_h, m1) = passThroughModal  -- nothing at `elsewhere` on the card page
                (hudH, m3) = page "hud" LayerHUD m1
                m4 = clickableAt "hudBtn" elsewhere (100, 100) "hudClick" hudH m3
            in case routePointer PointerLeftClick elsewhere m4 of
                RouteElement _ cb → cb `shouldBe` "hudClick"
                other → expectationFailure ("expected the HUD control, got " ⧺ show other)

    -- Every LayerModal page-creation site in scripts/ (combat_log,
    -- pause_menu, event_log, injury_log_panel, unit_log, save_browser,
    -- create_world_menu, loading_screen, settings_menu, ui/context_menu)
    -- takes the unmodified default below; only scripts/popup.lua and
    -- scripts/input_check_fixture.lua opt out via setPageInputExclusive,
    -- covered separately above.
    describe "LayerModal page classification (audit — #742)" $ do
        it "a freshly created LayerModal page defaults input-exclusive" $
            let (h, mgr) = createPage "x" LayerModal emptyUIPageManager
            in (upInputExclusive ⊚ getPage h mgr) `shouldBe` Just True

        it "setPageInputExclusive can reclassify a LayerModal page pass-through" $
            let (h, m1) = createPage "x" LayerModal emptyUIPageManager
                m2 = setPageInputExclusive h False m1
            in (upInputExclusive ⊚ getPage h m2) `shouldBe` Just False

        it "every non-modal layer defaults pass-through" $
            mapM_
                (\layer → do
                    let (h, mgr) = createPage "x" layer emptyUIPageManager
                    (upInputExclusive ⊚ getPage h mgr) `shouldBe` Just False)
                [LayerHUD, LayerOverlay, LayerMenu, LayerTooltip, LayerDebug]

    describe "two visible modal pages — topmost owns the boundary" $ do
        let twoModals =
                let (m1H, m1) = page "modal1" LayerModal emptyUIPageManager
                    m2 = clickableAt "m1Btn" pt (100, 100) "m1Click" m1H m1
                    (m2H, m3) = page "modal2" LayerModal m2
                    -- modal2 is created (and thus painted) AFTER modal1,
                    -- so it's the topmost boundary; deliberately no
                    -- control of its own at `pt`.
                in (m1H, m2H, m3)

        it "the topmost of two visible modals owns the boundary, not the lower one's control" $
            let (_, _, mgr) = twoModals
            in routePointer PointerLeftClick pt mgr `shouldBe` RouteMiss

        it "hiding the topmost modal exposes the next modal boundary" $
            let (_, m2H, mgr) = twoModals
                mgr' = hidePage m2H mgr
            in case routePointer PointerLeftClick pt mgr' of
                RouteElement _ cb → cb `shouldBe` "m1Click"
                other → expectationFailure ("expected modal1's control, got " ⧺ show other)

    describe "a single modal's visibility gates the boundary" $ do
        let singleModalOverHud =
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    m2 = clickableAt "hudBtn" pt (100, 100) "hudClick" hudH m1
                    (modalH, m3) = page "modal" LayerModal m2
                in (modalH, m3)

        it "hiding the only visible modal restores lower-page input" $
            let (modalH, mgr) = singleModalOverHud
            in do
                routePointer PointerLeftClick pt mgr `shouldBe` RouteMiss
                case routePointer PointerLeftClick pt (hidePage modalH mgr) of
                    RouteElement _ cb → cb `shouldBe` "hudClick"
                    other → expectationFailure ("expected the HUD control, got " ⧺ show other)

        it "deleting the only visible modal restores lower-page input" $
            let (modalH, mgr) = singleModalOverHud
            in do
                routePointer PointerLeftClick pt mgr `shouldBe` RouteMiss
                case routePointer PointerLeftClick pt (deletePage modalH mgr) of
                    RouteElement _ cb → cb `shouldBe` "hudClick"
                    other → expectationFailure ("expected the HUD control, got " ⧺ show other)

    describe "isPointerSurfaceBlocked (middle-click — #742 review round 1)" $ do
        it "is blocked by a modal boundary even where the modal has no element of its own" $
            let (_, mgr) = page "modal" LayerModal emptyUIPageManager
            in isPointerSurfaceBlocked pt mgr `shouldBe` True

        it "is blocked by any visible sized element when no modal is present (pre-#742 parity)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = createElement "hudPanel" 100 100 hudH m1
                (px, py) = pt
                m3 = addElementToPage hudH eh px py m2
            in isPointerSurfaceBlocked pt m3 `shouldBe` True

        it "is not blocked over genuinely empty space with no modal present" $
            isPointerSurfaceBlocked pt emptyUIPageManager `shouldBe` False

    describe "isPageInScope (#742 review round 1)" $ do
        it "is true for every page when no modal boundary exists" $
            let (hudH, mgr) = page "hud" LayerHUD emptyUIPageManager
            in isPageInScope hudH mgr `shouldBe` True

        it "is false for a page below the modal boundary" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_, mgr) = page "modal" LayerModal m1
            in isPageInScope hudH mgr `shouldBe` False

        it "is true for the boundary page itself and for a page above it" $
            let (modalH, m1) = page "modal" LayerModal emptyUIPageManager
                (debugH, mgr) = page "shell" LayerDebug m1
            in do
                isPageInScope modalH mgr `shouldBe` True
                isPageInScope debugH mgr `shouldBe` True

    describe "isGameplayBlocked" $ do
        it "is true while a visible exclusive modal page exists" $
            let (_, mgr) = page "modal" LayerModal emptyUIPageManager
            in isGameplayBlocked mgr `shouldBe` True

        it "is false when nothing is visible" $
            isGameplayBlocked emptyUIPageManager `shouldBe` False

        it "is false while only a pass-through page (e.g. a notification card) is visible" $
            let (h, m1) = page "popups" LayerModal emptyUIPageManager
                m2 = setPageInputExclusive h False m1
            in isGameplayBlocked m2 `shouldBe` False

    -- Wire-level integration: the same contract, but proven through the
    -- REAL 'Engine.Input.Thread.processInputs' dispatch (same technique
    -- as 'Test.Headless.Input.LayerA' — the harness starts neither the
    -- input nor the Lua thread, so both queues are this test's to drive
    -- and drain by hand) rather than the pure function in isolation.
    around withHeadlessEngine $
        describe "wire integration (Engine.Input.Thread)" $ do
            it "a HUD control hidden behind a visible modal never fires, but the miss still reaches Lua" $ \env → do
                resetAll env
                let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                    m2 = clickableAt "hudBtn" pt (100, 100) "hudClick" hudH m1
                    (_modalH, m3) = page "modal" LayerModal m2
                writeIORef (uiManagerRef env) m3
                let (px, py) = pt
                push env [InputMouseEvent GLFW.MouseButton'1 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Pressed]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldNotSatisfy` any isHudClickEvent
                msgs `shouldSatisfy` any isMouseDownEvent

            it "Escape still reaches Lua even though a visible modal makes gameplay input inactive" $ \env → do
                resetAll env
                let (_, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                push env [InputKeyEvent GLFW.Key'Escape GLFW.KeyState'Pressed noMods]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem LuaUIEscape
                msgs `shouldSatisfy` any isKeyDownEscape

            it "shell text focus still receives characters while a visible modal is present" $ \env → do
                resetAll env
                let (_, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                fid ← shellFocus env
                push env [InputCharEvent 'a']
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` elem (LuaCharInput (unFocusId fid) 'a')

            it "a middle-click over empty modal space is swallowed instead of starting camera drag (#742 review round 1)" $ \env → do
                resetAll env
                let (_, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                let (px, py) = pt
                push env [InputMouseEvent GLFW.MouseButton'3 (realToFrac px, realToFrac py) GLFW.MouseButtonState'Pressed]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isMouseDownEvent)
                recs ← drainOutcomes env
                case recs of
                    [r] → do
                        aoOutcome r `shouldBe` "noop"
                        aoHandler r `shouldBe` Just "ui_surface_block"
                    _ → expectationFailure ("expected one outcome record, got " ⧺ show recs)

-- * Wire-integration helpers (mirrors Test.Headless.Input.LayerA)

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

shellFocus ∷ EngineEnv → IO FocusId
shellFocus env = do
    let (fid, fm1) = registerFocusTarget True 0 createFocusManager
        fm2 = setFocus fid fm1
    writeIORef (focusManagerRef env) fm2
    pure fid

isHudClickEvent ∷ LuaMsg → Bool
isHudClickEvent (LuaUIClickEvent _ "hudClick" _ _) = True
isHudClickEvent _ = False

isMouseDownEvent ∷ LuaMsg → Bool
isMouseDownEvent (LuaMouseDownEvent _ _ _) = True
isMouseDownEvent _ = False

isKeyDownEscape ∷ LuaMsg → Bool
isKeyDownEscape (LuaKeyDownEvent KeyEscape _) = True
isKeyDownEscape _ = False
