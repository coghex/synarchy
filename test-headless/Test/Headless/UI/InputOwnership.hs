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
import qualified HsLua as Lua
import Test.Hspec
import Data.IORef (readIORef, writeIORef, atomicModifyIORef', newIORef)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Foldable (toList)
import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Input.Bindings (defaultKeyBindings)
import Engine.Input.Inject (noMods)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Script (loadModuleRef)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaMsg(..), LuaBackendState(..))
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
        -- Wheel/scroll routing (#743) no longer shares the click callback
        -- machinery — it's tested separately via 'routeScroll' in
        -- Test.Headless.UI.ElementInputPolicy, including its own
        -- modal-boundary coverage.

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

        -- #742 review round 2: scripts/debug.lua's F8 overlay and
        -- scripts/debug_anim_panel.lua both now render on a real
        -- LayerDebug page (UI.newPage(_, "debug")) — matching this
        -- fixture exactly — but own NO clickable UI.Manager elements
        -- of their own; every actual click they claim goes through
        -- their parallel Lua-side tryClaimClick, entirely outside
        -- routePointer/topHitBy. This proves the structural property
        -- their pass-through relies on: an ELEMENTLESS debug-layer
        -- page still counts as in-scope, above-the-boundary real
        -- estate — 'isPageInScope' above already proves the same
        -- thing directly; this proves it doesn't perturb the pointer
        -- ROUTING decision for what's below it either.
        it "an elementless debug-layer page (matching F8/debug_anim_panel's real structure) neither captures nor leaks a miss past the modal boundary below it" $
            let (_debugH, m1) = page "shell" LayerDebug emptyUIPageManager
                (_modalH, m2) = page "modal" LayerModal m1
                (hudH, m3) = page "hud" LayerHUD m2
                m4 = clickableAt "hudBtn" pt (100, 100) "hudClick" hudH m3
            in routePointer PointerLeftClick pt m4 `shouldBe` RouteMiss

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

        -- #743 narrowed this from "any visible sized element" (the
        -- pre-#742/#743 behavior) to 'elementBlocksPointer' — a bare
        -- visual element with no pointer-blocking opt-in and no click
        -- callback is now pass-through, so a middle-click over it
        -- reaches gameplay. See Test.Headless.UI.ElementInputPolicy for
        -- the full independent click/pointer-block/scroll-capture
        -- matrix (including the element still blocking when it opts in
        -- via ueBlocksPointer or a registered callback).
        it "is NOT blocked by a bare visual element with no pointer-blocking opt-in, when no modal is present (#743 behavior change)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (eh, m2) = createElement "hudPanel" 100 100 hudH m1
                (px, py) = pt
                m3 = addElementToPage hudH eh px py m2
            in isPointerSurfaceBlocked pt m3 `shouldBe` False

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

        it "is true for the boundary page itself and for a page above it (e.g. F8/debug_anim_panel's real LayerDebug page)" $
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

    -- #742 review round 3: the pure/wire tests above prove the routing
    -- PRIMITIVE and the Lua-side decoupling's Haskell half
    -- (isGameplayBlocked); this block additionally boots a REAL Lua
    -- backend and loads the ACTUAL scripts/debug.lua, driving its own
    -- production tryClaimClick/inGameplayView functions directly
    -- (executeDebugLua — the same loadstring+pcall primitive the TCP
    -- debug console itself uses) against a REAL UIPageManager holding a
    -- visible exclusive modal, rather than a synthetic Haskell stand-in.
    -- It does not replay the full onMouseDown broadcast chain
    -- (scripts/init.lua/init_mouse.lua and their ~30-script transitive
    -- load, which needs a real generated+shown world to satisfy
    -- hud.currentView=="zoomed_in" through the ordinary game flow) —
    -- that's the scale of tools/combat_anim_probe.py's own manual-only
    -- probe, not a headless Hspec addition, and the issue itself
    -- doesn't require a graphical/offscreen probe from this PR.
    -- scripts/debug_anim_panel.lua reuses debugOverlay.canShow() with
    -- no logic of its own (see its header comment), so this exercises
    -- both production surfaces' shared decision point.
    around withHeadlessEngine $
        describe "real Lua parallel hit-test path (scripts/debug.lua — #742 review round 3)" $ do
            it "debugOverlay.tryClaimClick still fires above a visible exclusive modal" $ \env → do
                resetAll env
                let (_, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                ls ← newDebugLuaBackend env
                setup ← evalDebug ls
                    "local d = require('scripts.debug'); \
                    \d.visible = true; \
                    \d.clickableRects = {{x=10,y=10,w=50,h=50,action=function() _G.__f8_fired = true end}}; \
                    \require('scripts.ui_manager').currentMenu = 'world_view'; \
                    \require('scripts.hud').currentView = 'zoomed_in'"
                setup `shouldNotSatisfy` isLuaError
                claimed ← evalDebug ls "require('scripts.debug').tryClaimClick(1, 20, 20)"
                claimed `shouldBe` "true"
                fired ← evalDebug ls "_G.__f8_fired == true"
                fired `shouldBe` "true"

            it "debugOverlay.tryClaimClick still refuses outside the current view, modal or not (#147/#151 preserved)" $ \env → do
                resetAll env
                let (_, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                ls ← newDebugLuaBackend env
                setup ← evalDebug ls
                    "local d = require('scripts.debug'); \
                    \d.visible = true; \
                    \d.clickableRects = {{x=10,y=10,w=50,h=50,action=function() _G.__f8_fired = true end}}; \
                    \require('scripts.ui_manager').currentMenu = 'settings'"
                setup `shouldNotSatisfy` isLuaError
                claimed ← evalDebug ls "require('scripts.debug').tryClaimClick(1, 20, 20)"
                claimed `shouldBe` "false"

            it "debug_anim_panel.tryClaimClick (the third parallel path the issue amendment names) also still fires above a visible exclusive modal" $ \env → do
                resetAll env
                let (_, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                ls ← newDebugAnimPanelLuaBackend env
                setup ← evalDebug ls
                    "local p = require('scripts.debug_anim_panel'); \
                    \local d = require('scripts.debug'); \
                    \d.visible = true; \
                    \p.visible = true; \
                    \p.clickableRects = {{x=10,y=10,w=50,h=50,action=function() _G.__anim_fired = true end}}; \
                    \require('scripts.ui_manager').currentMenu = 'world_view'; \
                    \require('scripts.hud').currentView = 'zoomed_in'"
                setup `shouldNotSatisfy` isLuaError
                claimed ← evalDebug ls "require('scripts.debug_anim_panel').tryClaimClick(1, 20, 20)"
                claimed `shouldBe` "true"
                fired ← evalDebug ls "_G.__anim_fired == true"
                fired `shouldBe` "true"

    -- #742 review round 3: a right-click behind a visible exclusive
    -- modal must not reach the ordinary tool-handler fallback chain
    -- either — scripts/init_mouse.lua deliberately lets right-click
    -- through even when isGameplayInputActive() is false (so it can
    -- CANCEL an in-progress placement), but build_tool.lua's own
    -- right-click branch does more than cancel: with no pending
    -- anchor, it ERASES whatever construction designation sits under
    -- the cursor (scripts/build_tool.lua's MOUSE_RIGHT case) — a real
    -- world mutation the modal boundary must stop. mine_tool/chop_tool/
    -- till_tool/plant_tool share the identical left=mutate/right=cancel
    -- gate, so this covers all five uniformly. Drives the REAL
    -- scripts/init_mouse.lua (require, not loadModuleRef — the file has
    -- no package.loaded self-registration to make loadModuleRef's
    -- dofile-style load visible to a later `require`, unlike
    -- scripts/debug.lua) with the five tool modules + unit_drag_select
    -- STUBBED (via package.loaded) rather than genuinely loaded: this
    -- test's only question is whether init_mouse.lua's gate calls into
    -- them at all, not whether their own internal logic is correct
    -- (build_tool.lua's real construction/world dependencies are far
    -- outside this fix's scope).
    around withHeadlessEngine $
        describe "real Lua tool-handler gate (scripts/init_mouse.lua — #742 review round 3)" $
            it "a right-click behind a visible exclusive modal never reaches build_tool/mine_tool/chop_tool/till_tool/plant_tool" $ \env → do
                resetAll env
                let (_, mgr) = page "modal" LayerModal emptyUIPageManager
                writeIORef (uiManagerRef env) mgr
                ls ← newBareLuaBackend env
                setup ← evalDebug ls
                    "_G.__tool_reached = false; \
                    \local stub = { handleMouseDown = function() _G.__tool_reached = true; return false end }; \
                    \package.loaded['scripts.build_tool'] = stub; \
                    \package.loaded['scripts.mine_tool']  = stub; \
                    \package.loaded['scripts.chop_tool']  = stub; \
                    \package.loaded['scripts.till_tool']  = stub; \
                    \package.loaded['scripts.plant_tool'] = stub; \
                    \package.loaded['scripts.unit_drag_select'] = { \
                    \    handleMouseDown = function() end, deferClick = function() end, \
                    \    armBoxSelect = function() end, cancel = function() end }"
                setup `shouldNotSatisfy` isLuaError
                call ← evalDebug ls "require('scripts.init_mouse').onMouseDown(2, 20, 20)"
                call `shouldNotSatisfy` isLuaError
                reached ← evalDebug ls "_G.__tool_reached == true"
                reached `shouldBe` "false"

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

-- * Real-Lua-backend helpers (mirrors Test.Headless.Input.Followup's
--   newTestLuaBackend, minus the broadcast-script registration this
--   suite doesn't need — scripts/debug.lua's tryClaimClick isn't a
--   broadcast target itself, see the describe block above).

-- | A real Lua backend with the FULL Lua API registered and nothing
--   else loaded — callers pull in whatever they need via 'evalDebug'
--   snippets calling the real Lua @require@, exactly as any script
--   would.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Like 'newBareLuaBackend', but additionally 'loadModuleRef's the
--   given script path up front (for a module that self-registers into
--   @package.loaded@ at its own top level, e.g. scripts/debug.lua —
--   see its header comment — so a later 'evalDebug' @require@ resolves
--   to the SAME loaded instance regardless of dofile-vs-require
--   loading).
newLuaBackendLoading ∷ EngineEnv → FilePath → IO LuaBackendState
newLuaBackendLoading env modulePath = do
    ls ← newBareLuaBackend env
    eRef ← Lua.runWith (lbsLuaState ls) $ loadModuleRef modulePath
    case eRef of
        Right _  → pure ()
        Left err → error $ "failed to load " ⧺ modulePath ⧺ ": " ⧺ T.unpack err
    pure ls

-- | scripts/debug.lua's own transitive requires — scripts/ui/scale.lua,
--   scripts/ui/label.lua, scripts/debug/{mode,layout,modes}.lua — are
--   all load-time-cheap, no font/asset blocking.
newDebugLuaBackend ∷ EngineEnv → IO LuaBackendState
newDebugLuaBackend env = newLuaBackendLoading env "scripts/debug.lua"

-- | scripts/debug_anim_panel.lua 'require's scripts.debug itself at its
--   own top level, so this also makes @require('scripts.debug')@
--   resolve to the same loaded instance.
newDebugAnimPanelLuaBackend ∷ EngineEnv → IO LuaBackendState
newDebugAnimPanelLuaBackend env = newLuaBackendLoading env "scripts/debug_anim_panel.lua"

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses ('executeDebugLua') — so test
--   setup/assertions exercise production Lua exactly as a human poking
--   the console would, not a bespoke test-only evaluator.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t
