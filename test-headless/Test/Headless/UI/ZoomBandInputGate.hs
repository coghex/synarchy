-- | #1875: the zoom-band gate on fallback world-entity mouse input.
--
--   @scripts/init_mouse.lua@ routes a gameplay press through an ordered
--   claim chain and then, if nothing claimed it, into the world-entity
--   fallback — unit\/item\/building hit tests and selection on the left
--   button, context menus and move orders on the right. That fallback
--   moved to @scripts/init_mouse_entity.lua@ in this issue, behind a
--   gate that admits only a press made in the @zoomed_in@ HUD view
--   band; the three hit-test implementations themselves know nothing
--   about the band or the render fade, so before the gate a zoom-map or
--   fade-band click acted on entities the player was not looking at.
--
--   Everything here drives the REAL @scripts/init_mouse.lua@,
--   @scripts/init_mouse_entity.lua@, @scripts/unit_drag_select.lua@,
--   @scripts/hud.lua@, @scripts/ui_manager.lua@ and
--   @scripts/ui/view_teardown.lua@ through the same @loadstring@+@pcall@
--   primitive the TCP debug console uses, with only the modules that
--   need a generated world stubbed (the five designation tools, the
--   per-target context-menu builders, the AI's move-order verb) and only
--   the three entity hit tests replaced — the routing question is
--   whether they are REACHED, and the real ones need rendered geometry.
--   Unit selection itself is the production engine verb against a real
--   'UnitManager', so \"did the selection change\" is answered by the
--   engine, not by a stub. Precedent for the whole shape:
--   'Test.Headless.UI.InputOwnership'.
module Test.Headless.UI.ZoomBandInputGate (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Foldable (toList)
import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import UI.Manager
import UI.Types
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), emptyWorldManager, emptyWorldState )

spec ∷ Spec
spec = around withGateEngine $ do

    -- §1: a press classified outside zoomed_in reaches nothing in the
    -- world-entity fallback, and says which band it was in.
    describe "off-band presses reach no world-entity interaction (§1)" $ do

        it "a zoomed_out LEFT press runs no hit test, arms no box \
           \selection, and changes no selection" $ \env → do
            ls ← gateBackend env
            selectUnitA ls
            setBand ls "zoomed_out"
            clearOutcomes env

            leftPress ls 20 20

            counters ls `shouldReturn` noEntityWork
            armed ls `shouldReturn` "false"
            selectedCount ls `shouldReturn` "1"

            releaseLeft ls 20 20
            _ ← offBandRecord env "input.click" "noop" "zoomed_out"
            nothingPending ls `shouldReturn` "true"

        it "a zoomed_out RIGHT press opens no menu and issues no move \
           \order" $ \env → do
            ls ← gateBackend env
            selectUnitA ls
            setBand ls "zoomed_out"
            clearOutcomes env

            rightPress ls 20 20

            evalOk ls "return probe.menuTries"   `shouldReturn` "0"
            evalOk ls "return probe.moveOrders"  `shouldReturn` "0"
            counters ls `shouldReturn` noEntityWork
            selectedCount ls `shouldReturn` "1"

            releaseRight ls 20 20
            _ ← offBandRecord env "input.click" "noop" "zoomed_out"
            nothingPending ls `shouldReturn` "true"

        it "a `none` (fade band) LEFT press is suppressed the same way, \
           \and its diagnostic names `none` rather than a generic \
           \off-band reason" $ \env → do
            ls ← gateBackend env
            selectUnitA ls
            setBand ls "none"
            clearOutcomes env

            leftPress ls 20 20

            counters ls `shouldReturn` noEntityWork
            armed ls `shouldReturn` "false"
            selectedCount ls `shouldReturn` "1"

            releaseLeft ls 20 20
            rec ← offBandRecord env "input.click" "deadclick" "none"
            -- The two bands must be TELLABLE APART, not merely both
            -- reported: a reason that named the zoom map here would
            -- pass every other assertion in this example.
            aoReason rec `shouldNotSatisfy` maybe False (T.isInfixOf "zoomed_out")

        it "a `none` (fade band) RIGHT press is suppressed the same way" $
          \env → do
            ls ← gateBackend env
            selectUnitA ls
            setBand ls "none"
            clearOutcomes env

            rightPress ls 20 20

            evalOk ls "return probe.menuTries"  `shouldReturn` "0"
            evalOk ls "return probe.moveOrders" `shouldReturn` "0"
            selectedCount ls `shouldReturn` "1"

            releaseRight ls 20 20
            rec ← offBandRecord env "input.click" "deadclick" "none"
            aoReason rec `shouldNotSatisfy` maybe False (T.isInfixOf "zoomed_out")

    -- §3: the band is read once, at mouse-down, and neither re-checked
    -- nor rolled back afterwards.
    describe "press-time cross-band semantics (§3)" $ do

        it "an off-band press released back in zoomed_in stays \
           \suppressed, and its record still names the PRESS band" $
          \env → do
            ls ← gateBackend env
            selectUnitA ls
            setBand ls "zoomed_out"
            clearOutcomes env

            leftPress ls 20 20
            -- The camera returns to the zoomed-in band mid-gesture.
            setBand ls "zoomed_in"
            releaseLeft ls 20 20

            counters ls `shouldReturn` noEntityWork
            evalOk ls "return probe.rectTests" `shouldReturn` "0"
            selectedCount ls `shouldReturn` "1"
            _ ← offBandRecord env "input.click" "noop" "zoomed_out"
            nothingPending ls `shouldReturn` "true"

        it "an off-band LEFT press DRAGGED past threshold and released in \
           \zoomed_in commits no box selection, and the drag record it \
           \resolves to still names the press band" $ \env → do
            ls ← gateBackend env
            selectUnitA ls
            setBand ls "zoomed_out"
            clearOutcomes env

            leftPress ls 20 20
            setBand ls "zoomed_in"
            -- Well past unit_drag_select's 4px DRAG_THRESHOLD.
            releaseLeft ls 220 220

            -- boxSelectArmed was never set, so onMouseUp cannot run the
            -- rect commit whatever the release coordinates are.
            evalOk ls "return probe.rectTests" `shouldReturn` "0"
            selectedCount ls `shouldReturn` "1"
            nothingPending ls `shouldReturn` "true"
            -- A gesture past the threshold resolves as ONE "input.drag"
            -- record and DISCARDS the deferred click (#730), so the band
            -- has to survive into THAT record — otherwise a dragged
            -- off-band press carries no diagnostic naming its band at
            -- all (PR review round 1).
            _ ← offBandDragRecord env "zoomed_out"
            pure ()

        it "an off-band RIGHT press DRAGGED past threshold resolves to a \
           \drag record that still names the press band" $ \env → do
            ls ← gateBackend env
            selectUnitA ls
            setBand ls "none"
            clearOutcomes env

            rightPress ls 20 20
            releaseRight ls 220 220

            evalOk ls "return probe.moveOrders" `shouldReturn` "0"
            nothingPending ls `shouldReturn` "true"
            rec ← offBandDragRecord env "none"
            aoReason rec `shouldNotSatisfy` maybe False (T.isInfixOf "zoomed_out")

        it "an off-band RIGHT press already past threshold when the \
           \zoomBand teardown fires is cancelled with its band intact" $
          \env → do
            ls ← gateBackend env
            setBand ls "zoomed_out"
            clearOutcomes env

            rightPress ls 20 20
            -- The real periodic tick is what promotes rightState to
            -- "dragging"; driving it keeps this off a hand-set flag.
            -- (There is deliberately no LEFT twin: an off-band left
            -- press is never boxSelectArmed, so dragSelect.update never
            -- promotes it, and its teardown always takes the pending-
            -- click branch the example above already covers.)
            evalOk ls "probe.mouse = { 220, 220 }; \
                       \require('scripts.unit_drag_select').update(0.03); \
                       \return require('scripts.unit_drag_select').rightState \
                       \       == 'dragging'"
                `shouldReturn` "true"

            runZoomBandTeardown ls "zoomed_in"

            nothingPending ls `shouldReturn` "true"
            _ ← offBandDragRecord env "zoomed_out"
            pure ()

        it "a zoomed_in press keeps its immediate unit selection when the \
           \band changes afterwards, while the zoomBand teardown still \
           \cancels the pending drag state" $ \env → do
            ls ← gateBackend env
            setBand ls "zoomed_in"
            evalOk ls "probe.unitHitId = 1; return true" `shouldReturn` "true"
            clearOutcomes env

            leftPress ls 20 20
            -- The press was accepted: it selected through the real verb
            -- and armed box selection.
            selectedCount ls `shouldReturn` "1"
            armed ls `shouldReturn` "true"

            -- The camera crosses out of the band. This is the PRODUCTION
            -- teardown sweep, not a hand-picked hook.
            runZoomBandTeardown ls "zoomed_out"

            -- Not rolled back (§3): unit selection and move orders that
            -- already happened stand. (Item/building/cursor selections
            -- ARE cleared by that same sweep — pre-existing behavior
            -- this issue deliberately does not change, which is why this
            -- assertion is written against the unit domain.)
            selectedCount ls `shouldReturn` "1"
            -- Pending drag state IS canceled, and the deferred click is
            -- resolved by that path rather than left dangling.
            nothingPending ls `shouldReturn` "true"
            recs ← drainOutcomes env
            map aoKind recs `shouldBe` ["input.click"]
            map aoHandler recs `shouldBe` [Just "unit_select"]

        it "an off-band press resolved by the zoomBand teardown instead \
           \of a release still records its band diagnostic exactly once" $
          \env → do
            ls ← gateBackend env
            setBand ls "zoomed_out"
            clearOutcomes env

            leftPress ls 20 20
            -- No mouse-up ever arrives; the view transition resolves it.
            runZoomBandTeardown ls "zoomed_in"

            _ ← offBandRecord env "input.click" "noop" "zoomed_out"
            nothingPending ls `shouldReturn` "true"

    -- §2: everything that could claim the press first still gets to.
    describe "handler opportunity is preserved ahead of the gate (§2)" $ do

        it "an off-band RIGHT press still reaches the designation tools \
           \before the gate declines it" $ \env → do
            ls ← gateBackend env
            setBand ls "zoomed_out"

            rightPress ls 20 20

            evalOk ls "return probe.toolCalls > 0" `shouldReturn` "true"
            evalOk ls "return probe.menuTries"     `shouldReturn` "0"
            releaseRight ls 20 20

        it "an off-band LEFT press also still reaches them (the ordering \
           \holds for both buttons, not just right-click)" $ \env → do
            ls ← gateBackend env
            setBand ls "zoomed_out"

            leftPress ls 20 20

            evalOk ls "return probe.toolCalls > 0" `shouldReturn` "true"
            counters ls `shouldReturn` noEntityWork
            releaseLeft ls 20 20

        it "a tool that CLAIMS an off-band press keeps its own outcome; \
           \the gate never overwrites it" $ \env → do
            ls ← gateBackend env
            setBand ls "zoomed_out"
            evalOk ls "probe.toolClaims = true; return true"
                `shouldReturn` "true"
            clearOutcomes env

            rightPress ls 20 20
            releaseRight ls 20 20

            recs ← drainOutcomes env
            map aoHandler recs `shouldBe` [Just "build_tool"]

    -- §5: nothing about the zoomed-in path changed.
    describe "zoomed_in behavior is preserved (§5)" $ do

        it "a LEFT press runs the point hit tests and arms box selection" $
          \env → do
            ls ← gateBackend env
            setBand ls "zoomed_in"
            clearOutcomes env

            leftPress ls 20 20

            -- Missed every domain, so all three point tests ran in the
            -- documented units > items > buildings order.
            counters ls `shouldReturn` ("1", "1", "1")
            armed ls `shouldReturn` "true"
            releaseLeft ls 20 20
            recs ← drainOutcomes env
            map aoHandler recs `shouldBe` [Just "deselect"]

        it "a LEFT press on a unit still selects it through the real \
           \engine verb" $ \env → do
            ls ← gateBackend env
            setBand ls "zoomed_in"
            evalOk ls "probe.unitHitId = 1; return true" `shouldReturn` "true"

            leftPress ls 20 20

            selectedCount ls `shouldReturn` "1"
            evalOk ls "return unit.isSelected(1)" `shouldReturn` "true"
            releaseLeft ls 20 20

        it "a RIGHT press still reaches the context-menu routes" $
          \env → do
            ls ← gateBackend env
            setBand ls "zoomed_in"

            rightPress ls 20 20

            -- Three per-target menus plus the no-selection tile menu.
            evalOk ls "return probe.menuTries" `shouldReturn` "4"
            releaseRight ls 20 20

        it "a RIGHT press with a selection still issues move orders" $
          \env → do
            ls ← gateBackend env
            setBand ls "zoomed_in"
            selectUnitA ls
            clearOutcomes env

            rightPress ls 20 20

            evalOk ls "return probe.moveOrders" `shouldReturn` "1"
            releaseRight ls 20 20
            recs ← drainOutcomes env
            map aoHandler recs `shouldBe` [Just "move_order"]

        it "the independent gameplay-activity gate still blocks the \
           \fallback behind a visible exclusive modal, in-band" $
          \env → do
            ls ← gateBackend env
            setBand ls "zoomed_in"
            selectUnitA ls
            showModal env
            clearOutcomes env

            leftPress ls 20 20

            counters ls `shouldReturn` noEntityWork
            armed ls `shouldReturn` "false"
            selectedCount ls `shouldReturn` "1"
            releaseLeft ls 20 20
            -- The pre-existing gate's own reason, NOT the band gate's:
            -- the view-band check does not replace or weaken it.
            recs ← drainOutcomes env
            case recs of
                [r] → do
                    aoOutcome r `shouldBe` "deadclick"
                    aoReason r `shouldSatisfy`
                        maybe False (T.isInfixOf "gameplay input inactive")
                _ → expectationFailure ("expected one record, got " ⧺ show (map aoKind recs))

    -- §4: returning early from init_mouse does not consume the press.
    describe "zoom-map chunk interaction is unaffected (§4)" $ do

        it "a suppressed zoomed_out LEFT press still reaches the HUD's \
           \chunk selection" $ \env → do
            ls ← gateBackend env
            setBand ls "zoomed_out"

            -- The production broadcast (broadcastToModules "onMouseDown",
            -- Engine.Scripting.Lua.Thread.Dispatch) delivers ONE press to
            -- every loaded module; scripts/init.lua's game.onMouseDown
            -- and scripts/ui_manager_widgets.lua's forward to
            -- hud.onMouseDown are independent subscribers, so an early
            -- return from the first cannot stop the second. Driving both
            -- in that order is what proves it — asserting on init_mouse
            -- alone would prove nothing about the HUD half.
            leftPress ls 20 20
            hudPress ls 1 20 20

            evalOk ls "return probe.selectedChunk == '7,8'"
                `shouldReturn` "true"
            releaseLeft ls 20 20

        it "a suppressed zoomed_out RIGHT press still reaches the HUD's \
           \chunk clear" $ \env → do
            ls ← gateBackend env
            setBand ls "zoomed_out"

            rightPress ls 20 20
            hudPress ls 2 20 20

            evalOk ls "return probe.chunkClears" `shouldReturn` "1"
            releaseRight ls 20 20

-- * Fixture

-- | Engine init itself materializes @config/*.local.yaml@ (#1357 /
--   'Test.Headless.Harness.Isolation'), and this suite loads the real
--   @scripts/ui_manager.lua@ tree, so the scratch resource root goes
--   AROUND the engine rather than inside it. No generation parameters
--   are needed — the fixture's page is in-memory storage for a page id
--   — so the world worker never has to start.
withGateEngine ∷ (EngineEnv → IO α) → IO α
withGateEngine action =
    withIsolatedResourceRoot (withHeadlessEngineNoWorld action)

gatePage ∷ WorldPageId
gatePage = WorldPageId "zoom_band_gate_page"

gateUnit ∷ UnitId
gateUnit = UnitId 1

-- | Only 'uiPage' matters to selection; the rest are inert placeholders
--   (the same minimal-instance shape 'Test.Headless.UI.InputOwnership'
--   uses for its own selection fixture).
gateUnitInstance ∷ UnitInstance
gateUnitInstance = UnitInstance
    { uiDefName = "zoom_band_gate_test", uiName = "", uiPage = gatePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | One visible in-memory page holding one live unit, so @unit.select@ /
--   @unit.getSelected@ are the PRODUCTION page-filtered verbs rather
--   than stubs. No generation parameters, so no world worker is needed.
installGateWorld ∷ EngineEnv → IO ()
installGateWorld env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(gatePage, ws)], wmVisible = [gatePage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.fromList [(gateUnit, gateUnitInstance)]
        , umNextId = 2 }
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)

-- | A real Lua backend with the full API registered, the world fixture
--   installed, and the narrowest possible set of stubs.
--
--   Stubbed, because their real implementations need a generated world
--   or rendered geometry this suite has no business booting: the five
--   designation tools, @scripts/init_context_menu.lua@'s per-target menu
--   builders, the two move-order collaborators, the three entity point
--   hit tests, and the world picker/chunk verbs. NOT stubbed, because
--   they are what is under test or what answers the assertions:
--   @scripts/init_mouse.lua@, @scripts/init_mouse_entity.lua@,
--   @scripts/unit_drag_select.lua@, @scripts/hud.lua@,
--   @scripts/ui_manager.lua@, @scripts/ui/view_teardown.lua@, and every
--   @unit.*@ selection verb.
gateBackend ∷ EngineEnv → IO LuaBackendState
gateBackend env = do
    installGateWorld env
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    setup ← evalDebug ls
        "_G.probe = { unitHits = 0, itemHits = 0, bldHits = 0, \
        \             rectTests = 0, toolCalls = 0, menuTries = 0, \
        \             moveOrders = 0, chunkClears = 0, \
        \             selectedChunk = nil, unitHitId = nil, \
        \             toolClaims = false, tileMenuClaims = false, \
        \             mouse = { 0, 0 } }; \
        \local function makeTool() return { \
        \    handleMouseDown = function() \
        \        probe.toolCalls = probe.toolCalls + 1; \
        \        return probe.toolClaims end, \
        \    cancel = function() end } end; \
        \local buildTool = makeTool(); \
        \buildTool.hidePicker = function() end; \
        \buildTool.exitPlacement = function() end; \
        \package.loaded['scripts.build_tool'] = buildTool; \
        \package.loaded['scripts.mine_tool']  = makeTool(); \
        \package.loaded['scripts.chop_tool']  = makeTool(); \
        \package.loaded['scripts.till_tool']  = makeTool(); \
        \package.loaded['scripts.plant_tool'] = makeTool(); \
        \local function tryMenu() \
        \    probe.menuTries = probe.menuTries + 1; return false end; \
        \package.loaded['scripts.init_context_menu'] = { \
        \    tryBuildingMenu = tryMenu, tryUnitMenu = tryMenu, \
        \    tryItemMenu = tryMenu, \
        \    tryTileMenu = function() \
        \        probe.menuTries = probe.menuTries + 1; \
        \        return probe.tileMenuClaims end }; \
        \package.loaded['scripts.transfer_session'] = \
        \    { notePlayerOrder = function() end, clear = function() end }; \
        \package.loaded['scripts.unit_ai'] = { commandMove = function() \
        \    probe.moveOrders = probe.moveOrders + 1 end }; \
        \unit.hitTestAt = function() \
        \    probe.unitHits = probe.unitHits + 1; return probe.unitHitId end; \
        \unit.hitTestInRect = function() \
        \    probe.rectTests = probe.rectTests + 1; return {} end; \
        \item.hitTestAt = function() \
        \    probe.itemHits = probe.itemHits + 1; return nil end; \
        \building.hitTestAt = function() \
        \    probe.bldHits = probe.bldHits + 1; return nil end; \
        \world.pickTile = function() return 3, 4 end; \
        \world.pickChunk = function() return 7, 8 end; \
        \world.selectChunk = function(_, gx, gy) \
        \    probe.selectedChunk = gx .. ',' .. gy end; \
        \world.clearZoomCursorSelect = function() \
        \    probe.chunkClears = probe.chunkClears + 1 end; \
        \world.clearWorldCursorSelect = function() end; \
        \engine.getMousePosition = function() \
        \    return probe.mouse[1], probe.mouse[2] end; \
        \require('scripts.ui_manager').currentMenu = 'world_view'; \
        \local h = require('scripts.hud'); \
        \h.visible = true; h.worldId = 'zoom_band_gate_page'; \
        \h.currentView = 'zoomed_in'; \
        \return true"
    when (setup ≢ "true") $
        error ("zoom-band gate fixture setup failed: " ⧺ T.unpack setup)
    pure ls

-- * Driving the production entry points

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- | Evaluate and fail the example on a Lua error rather than letting a
--   stringified error silently compare unequal to an expected value.
evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    got ← evalDebug ls src
    when ("error:" `T.isPrefixOf` got ∨ "syntax error:" `T.isPrefixOf` got) $
        expectationFailure ("Lua error from " ⧺ show src ⧺ ": " ⧺ T.unpack got)
    pure got

setBand ∷ LuaBackendState → Text → IO ()
setBand ls band = do
    got ← evalOk ls ("require('scripts.hud').currentView = '" <> band
                     <> "'; return require('scripts.hud').currentView == '"
                     <> band <> "'")
    got `shouldBe` "true"

selectUnitA ∷ LuaBackendState → IO ()
selectUnitA ls = evalOk ls "return unit.select(1)" `shouldReturn` "true"

selectedCount ∷ LuaBackendState → IO Text
selectedCount ls = evalOk ls "return #unit.getSelected()"

armed ∷ LuaBackendState → IO Text
armed ls =
    evalOk ls "return require('scripts.unit_drag_select').boxSelectArmed == true"

-- | No deferred click and no live gesture left on either button — the
--   \"tests must not leave deferred click state pending\" requirement.
nothingPending ∷ LuaBackendState → IO Text
nothingPending ls = evalOk ls
    "local d = require('scripts.unit_drag_select'); \
    \return d.pendingClick == nil and d.rightPendingClick == nil \
    \       and d.state == 'idle' and d.rightState == 'idle'"

leftPress, rightPress ∷ LuaBackendState → Int → Int → IO ()
leftPress  = mousePress 1
rightPress = mousePress 2

mousePress ∷ Int → LuaBackendState → Int → Int → IO ()
mousePress button ls x y =
    evalOk ls ("require('scripts.init_mouse').onMouseDown("
               <> tshow button <> ", " <> tshow x <> ", " <> tshow y
               <> "); return true") `shouldReturn` "true"

releaseLeft, releaseRight ∷ LuaBackendState → Int → Int → IO ()
releaseLeft  = mouseRelease 1
releaseRight = mouseRelease 2

-- | The production release path: @scripts/init.lua@'s @game.onMouseUp@
--   forwards to @init_mouse.onMouseUp@, but the gesture itself is
--   resolved by @unit_drag_select.onMouseUp@ (which the engine's own
--   dispatch drives as a broadcast subscriber).
mouseRelease ∷ Int → LuaBackendState → Int → Int → IO ()
mouseRelease button ls x y =
    evalOk ls ("require('scripts.unit_drag_select').onMouseUp("
               <> tshow button <> ", " <> tshow x <> ", " <> tshow y
               <> ", 'game'); return true") `shouldReturn` "true"

-- | @scripts/hud.lua@'s own broadcast subscriber, the other half of the
--   press §4 is about. Takes FRAMEBUFFER coordinates, as
--   @uiManager.onMouseDown@ forwards them.
hudPress ∷ LuaBackendState → Int → Int → Int → IO ()
hudPress ls button x y =
    evalOk ls ("require('scripts.hud').onMouseDown("
               <> tshow button <> ", " <> tshow x <> ", " <> tshow y
               <> "); return true") `shouldReturn` "true"

-- | The PRODUCTION zoom-band teardown sweep, exactly as
--   @hud.reconcileView@ runs it.
runZoomBandTeardown ∷ LuaBackendState → Text → IO ()
runZoomBandTeardown ls newView = do
    setBand ls newView
    evalOk ls ("require('scripts.ui.view_teardown').run('zoomBand', \
               \{ worldId = require('scripts.hud').worldId, newView = '"
               <> newView <> "' }); return true") `shouldReturn` "true"

-- | A visible modal-exclusive page, so @UI.isInputBlocked()@ — and
--   therefore @uiManager.isGameplayInputActive()@ — goes false.
showModal ∷ EngineEnv → IO ()
showModal env = do
    let (h, m1) = createPage "zoom_band_gate_modal" LayerModal emptyUIPageManager
    writeIORef (uiManagerRef env) (showPage h m1)

-- * Assertions

-- | (unit, item, building) point-hit-test call counts.
counters ∷ LuaBackendState → IO (Text, Text, Text)
counters ls = do
    u ← evalOk ls "return probe.unitHits"
    i ← evalOk ls "return probe.itemHits"
    b ← evalOk ls "return probe.bldHits"
    pure (u, i, b)

noEntityWork ∷ (Text, Text, Text)
noEntityWork = ("0", "0", "0")

drainOutcomes ∷ EngineEnv → IO [ActionOutcome]
drainOutcomes env = toList ⊚ atomicModifyIORef' (actionOutcomeRef env)
    (\buf → (Seq.empty, buf))

-- | Drop whatever the fixture's own setup recorded, so an example's
--   assertions describe only the press it just made.
clearOutcomes ∷ EngineEnv → IO ()
clearOutcomes = void ∘ drainOutcomes

-- | Exactly one recorded outcome — the @input.drag@ one a gesture past
--   DRAG_THRESHOLD resolves to — whose reason still names the press band
--   the deferred click it discarded was carrying.
offBandDragRecord ∷ EngineEnv → Text → IO ActionOutcome
offBandDragRecord env band = do
    recs ← drainOutcomes env
    case recs of
        [r] → do
            aoKind r    `shouldBe` "input.drag"
            aoOutcome r `shouldBe` "noop"
            aoHandler r `shouldBe` Just "unit_drag_select"
            aoReason r  `shouldSatisfy` maybe False (T.isInfixOf band)
            pure r
        _ → do
            expectationFailure
                ("expected exactly one drag outcome record, got "
                 ⧺ show (map (\r → (aoKind r, aoOutcome r, aoReason r)) recs))
            error "unreachable"

-- | Exactly one recorded outcome, of the given kind and outcome, whose
--   reason names the given band.
offBandRecord ∷ EngineEnv → Text → Text → Text → IO ActionOutcome
offBandRecord env kind outcome band = do
    recs ← drainOutcomes env
    case recs of
        [r] → do
            aoKind r    `shouldBe` kind
            aoOutcome r `shouldBe` outcome
            aoHandler r `shouldBe` Nothing
            aoReason r  `shouldSatisfy` maybe False (T.isInfixOf band)
            pure r
        _ → do
            expectationFailure
                ("expected exactly one outcome record, got "
                 ⧺ show (map (\r → (aoKind r, aoOutcome r, aoReason r)) recs))
            error "unreachable"
