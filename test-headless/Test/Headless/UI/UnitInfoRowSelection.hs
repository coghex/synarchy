-- | #1929: the unit right-click menu's \"Info\" row clears the building
--   and ground-item selection domains only when its own
--   @unit.select@ reports success.
--
--   @scripts/init_context_menu.lua@'s @tryUnitMenu@ captures
--   @targetUid@ in a closure when the menu opens, and
--   @scripts.ui.context_menu.handleItemClick@ hides the menu and fires
--   that closure later with no revalidation. The modal does not pause
--   simulation, so a unit killed, despawned or moved off the active
--   page in between makes @unit.select@ refuse
--   ('Unit.Selection.selectUnit' leaves @umSelected@ exactly as it
--   was) while the two cleanup verbs it was paired with —
--   @building.deselect@ and @item.deselect@ — are unconditional
--   clears. The Info action then erased two domains and selected
--   nothing. Same stale-captured-target shape
--   'Test.Headless.UI.ConsumableGesture' gates for the consumable rows
--   (#1580 requirement 5).
--
--   Everything below is the production path: a real Lua backend with
--   the full API registered, the REAL @scripts/init_context_menu.lua@
--   and @scripts/ui/context_menu.lua@ (rendered rows, not a
--   @contextMenu.show@ spy), and the REAL engine selection verbs
--   against real manager refs — so \"did @unit.select@ refuse\" and
--   \"what is selected now\" are both answered by the engine. Only
--   @unit.hitTestAt@ is stubbed (the real one needs rendered world
--   geometry) and @scripts.hud@ is the same @menuFont@ stand-in
--   'Test.Headless.UI.PopupPlacement' uses to get past the context
--   menu's font-readiness check.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match \"Unit Info row selection gate\"'@.
module Test.Headless.UI.UnitInfoRowSelection (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, writeIORef, modifyIORef')
import Building.Types
    ( BuildingId(..), BuildingInstance(..), BuildingManager(..)
    , emptyBuildingManager )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import UI.Types (emptyUIPageManager)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), emptyWorldManager, emptyWorldState )

-- * Fixture identities

infoPage ∷ WorldPageId
infoPage = WorldPageId "info_row_selection_page"

-- | The unit already selected when the menu is activated. Deliberately
--   NOT the menu's target, so \"the unit domain was left alone\" is
--   distinguishable from \"the target got selected anyway\".
priorUid ∷ UnitId
priorUid = UnitId 1

-- | The unit the menu was opened on, i.e. the uid its Info row
--   captured.
targetUid ∷ UnitId
targetUid = UnitId 2

-- | The already-selected building. Any live instance on the active
--   page satisfies @building.select@; no def registration is involved.
priorBid ∷ BuildingId
priorBid = BuildingId 5

spec ∷ Spec
spec = around (withIsolatedResourceRoot . withHeadlessEngineNoWorld) $ do

    it "a live target selects it and clears the building and \
       \ground-item domains" $ \env → do
        ls ← infoRowBackend env
        seedAllThreeDomains ls
        snapshot ls `shouldReturn` seededSnapshot

        openUnitMenu ls
        clickInfo ls

        -- Requirements 1 and 3: the success branch is unchanged --
        -- the target becomes the sole unit selection and the other
        -- two domains are cleared, keeping one-domain-at-a-time HUD
        -- ownership.
        snapshot ls `shouldReturn` "\"units=2 building=nil item=nil\""

    it "a target removed after the menu opened leaves all three \
       \domains exactly as they were" $ \env → do
        ls ← infoRowBackend env
        seedAllThreeDomains ls
        openUnitMenu ls

        -- The scheduling window the defect lives in: UnitDestroy
        -- removes the instance while the menu waits for input
        -- (Unit.Thread.Command.Lifecycle). The captured closure keeps
        -- the uid regardless.
        removeUnit env targetUid

        -- The failure comes from the REAL selection verb, not a
        -- stubbed refusal: Unit.Selection.selectUnit is the boundary
        -- whose no-op result the row now has to honour. The call is
        -- itself the no-op it reports, so it disturbs nothing the
        -- snapshot below then captures.
        evalOk ls "return unit.select(2)" `shouldReturn` "false"

        -- Requirement 2 / review correction: compare against a
        -- snapshot taken immediately BEFORE activation and AFTER the
        -- removal, so an unrelated concurrent change could not be
        -- mistaken for a mutation this callback caused.
        before ← snapshot ls
        before `shouldBe` seededSnapshot
        clickInfo ls
        after ← snapshot ls
        after `shouldBe` before

-- * Driving the production path

-- | Seed distinguishable, non-empty state in all three domains. A
--   default-empty selection could not detect the reported clears,
--   because @building.deselect@ and @item.deselect@ are unconditional.
seedAllThreeDomains ∷ LuaBackendState → IO ()
seedAllThreeDomains ls = do
    evalOk ls "return unit.select(1)" `shouldReturn` "true"
    runOk ls "building.select(5)"
    -- Ground item 77: @item.select@ records the id on the active
    -- world's cursor state without consulting the item manager, so no
    -- instance is needed to make this domain non-empty.
    runOk ls "item.select(77)"

-- | What 'seedAllThreeDomains' establishes, as 'snapshot' reports it.
seededSnapshot ∷ Text
seededSnapshot = "\"units=1 building=5 item=77\""

-- | Open the real unit context menu on 'targetUid'. @tryUnitMenu@
--   builds the item list (Info first) and hands it to the real
--   @contextMenu.show@, which renders clickable rows.
openUnitMenu ∷ LuaBackendState → IO ()
openUnitMenu ls = do
    claimed ← evalOk ls
        "return require('scripts.init_context_menu').tryUnitMenu(10, 20)"
    claimed `shouldBe` "true"

-- | Activate the Info row the way a click does: resolve its REAL
--   rendered handle through the menu's own dump (label -> handle) and
--   pass it to @cm.handleItemClick@, which is what hides the menu and
--   invokes the captured callback. Invoking a captured closure
--   directly would bypass that production dispatch.
clickInfo ∷ LuaBackendState → IO ()
clickInfo ls = do
    handled ← evalOk ls
        "local cm = require('scripts.ui.context_menu'); \
        \local h; \
        \for _, w in ipairs(cm.dump()) do \
        \  if w.label == 'Info' then h = w.handle end \
        \end; \
        \if not h then return 'no Info row' end; \
        \return cm.handleItemClick(h)"
    handled `shouldBe` "true"

-- | All three selection domains as the engine reports them, read back
--   through the production query verbs.
snapshot ∷ LuaBackendState → IO Text
snapshot ls = evalOk ls
    "local sel = unit.getSelected() or {}; \
    \table.sort(sel); \
    \local parts = {}; \
    \for i, u in ipairs(sel) do parts[i] = tostring(u) end; \
    \return 'units=' .. table.concat(parts, ',') \
    \    .. ' building=' .. tostring(building.getSelected()) \
    \    .. ' item=' .. tostring(item.getSelected())"

-- | Drop a unit instance the way @UnitDestroy@ does, leaving the
--   selection sets untouched so the snapshot below sees only what the
--   Info callback itself changes.
removeUnit ∷ EngineEnv → UnitId → IO ()
removeUnit env uid = modifyIORef' (unitManagerRef env) $ \um →
    um { umInstances = HM.delete uid (umInstances um) }

-- * Fixture

-- | One in-memory page holding the two units and the building, so the
--   page-scoped selection verbs (#76/#78) are the production ones
--   rather than stubs. No generation parameters, so no world worker.
installInfoRowWorld ∷ EngineEnv → IO ()
installInfoRowWorld env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(infoPage, ws)], wmVisible = [infoPage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.fromList
            [(priorUid, liveUnit), (targetUid, liveUnit)]
        , umNextId = 3 }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmInstances = HM.singleton priorBid liveBuilding
        , bmNextId = 6 }
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)

-- | A real Lua backend with the full API registered and the world
--   fixture installed. @unit.hitTestAt@ is the only replaced verb --
--   the real one needs rendered geometry, and which unit was
--   right-clicked is fixture, not behavior. @scripts.hud@ stands in
--   for the HUD's font handle so @contextMenu.show@ gets past
--   @ensureReady@ (the same stand-in
--   'Test.Headless.UI.PopupPlacement' uses).
infoRowBackend ∷ EngineEnv → IO LuaBackendState
infoRowBackend env = do
    installInfoRowWorld env
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    runOk ls "package.loaded['scripts.hud'] = { menuFont = 1 }; \
             \unit.hitTestAt = function() return 2 end"
    pure ls

-- | Only 'uiPage' matters to the selection verbs; the rest are inert
--   placeholders (the minimal-instance shape
--   'Test.Headless.UI.ZoomBandInputGate' uses for its own fixture).
liveUnit ∷ UnitInstance
liveUnit = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = infoPage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | Likewise, only 'biPage' matters to @building.select@ /
--   @building.getSelected@.
liveBuilding ∷ BuildingInstance
liveBuilding = BuildingInstance
    { biDefName = "cargo_hold", biPage = infoPage
    , biTexture = TextureHandle 0
    , biAnchorX = 0, biAnchorY = 0, biGridZ = 0, biSpawnedAt = 0
    , biTileW = 1, biTileH = 1, biSpawnRemaining = 0, biBuildProgress = 0
    , biMaterialsDelivered = HM.empty, biStorage = []
    }

-- * Lua plumbing

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- | Evaluate, failing the example loudly on a Lua error rather than
--   letting a stringified error compare unequal to an expected value.
evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    got ← evalDebug ls src
    when ("error:" `T.isPrefixOf` got ∨ "syntax error:" `T.isPrefixOf` got) $
        expectationFailure ("Lua error from " ⧺ show src ⧺ ": " ⧺ T.unpack got)
    pure got

runOk ∷ LuaBackendState → Text → IO ()
runOk ls src = void (evalOk ls src)
