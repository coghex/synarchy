-- | #2300: the ground-item right-click menu's \"Info\" row clears the
--   unit and building selection domains only when its own
--   @item.select@ reports success.
--
--   The item-domain counterpart of 'Test.Headless.UI.UnitInfoRowSelection'
--   (#1929), and the same defect shape. @tryItemMenu@ (now
--   @scripts/init_context_menu_item.lua@, reached through
--   @scripts/init_context_menu.lua@'s entry point) captures @gid@ in a
--   closure when the menu OPENS and
--   @scripts.ui.context_menu.handleItemClick@ fires that closure later
--   with no revalidation. The modal does not pause simulation, so the
--   item can be picked up or removed in between — and @item.select@
--   used to install the captured number regardless, returning nothing,
--   next to two unconditional clears. The player right-clicked an item,
--   chose Info, and landed with their units and building deselected and
--   an item selection pointing at nothing.
--
--   Everything below is the production path: a real Lua backend with
--   the full API registered, the REAL context-menu scripts (rendered
--   rows, not a @contextMenu.show@ spy), and the REAL engine selection
--   verbs against real manager refs. Only @item.hitTestAt@ is stubbed
--   (the real one needs rendered world geometry) and @scripts.hud@ is
--   the same @menuFont@ stand-in 'Test.Headless.UI.PopupPlacement' uses
--   to get past the context menu's font-readiness check.
--
--   The two ground items are DISTINCT and both live at seeding time:
--   the prior selection is a different item from the menu's target, so
--   \"the item domain was left alone\" is distinguishable from \"the
--   target got selected anyway\", and only the target is removed — a
--   removal that also took the prior selection's item away could not
--   tell a preserved selection from a lucky one.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match \"Item Info row selection gate\"'@.
module Test.Headless.UI.ItemInfoRowSelection (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
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
import Item.Ground (spawnGroundItem)
import Item.Types (ItemInstance(..), emptyItemManager)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import UI.Types (emptyUIPageManager)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager
    , emptyWorldState )

-- * Fixture identities

infoPage ∷ WorldPageId
infoPage = WorldPageId "item_info_row_selection_page"

-- | The unit already selected when the menu is activated.
priorUid ∷ UnitId
priorUid = UnitId 1

-- | The already-selected building.
priorBid ∷ BuildingId
priorBid = BuildingId 5

-- | The already-selected GROUND ITEM. Deliberately not the menu's
--   target, and never removed, so the refusal case can assert that a
--   live prior selection survived rather than that the domain merely
--   ended up empty.
priorGid ∷ Int
priorGid = 0

-- | The ground item the menu was opened on, i.e. the gid its Info row
--   captured. Spawned second into a fresh page, so the page's own
--   allocator hands out this id.
targetGid ∷ Int
targetGid = 1

spec ∷ Spec
spec = around (withIsolatedResourceRoot . withHeadlessEngineNoWorld) $ do

    it "a live target selects it and clears the unit and building \
       \domains" $ \env → do
        ls ← infoRowBackend env
        seedAllThreeDomains ls
        snapshot ls `shouldReturn` seededSnapshot

        openItemMenu ls
        clickInfo ls

        -- The success branch is unchanged: the target becomes the item
        -- selection and the other two domains are cleared, keeping
        -- one-domain-at-a-time HUD ownership.
        snapshot ls `shouldReturn`
            ("\"units= building=nil item=" <> tshow targetGid <> "\"")

    it "a target removed after the menu opened leaves all three \
       \domains exactly as they were" $ \env → do
        ls ← infoRowBackend env
        seedAllThreeDomains ls
        openItemMenu ls

        -- The scheduling window the defect lives in: the item leaves
        -- the page while the menu waits for input. ONLY the target
        -- goes; the prior item selection's own item stays live.
        evalOk ls ("return item.removeGround(" <> tshow targetGid <> ")")
            `shouldReturn` "true"

        -- The refusal comes from the REAL selection verb, not a stubbed
        -- one. The call is itself the no-op it reports, so it disturbs
        -- nothing the snapshot below then captures.
        evalOk ls ("return item.select(" <> tshow targetGid <> ")")
            `shouldReturn` "false"

        -- Compare against a snapshot taken immediately BEFORE
        -- activation and AFTER the removal, so an unrelated change
        -- could not be mistaken for a mutation this callback caused.
        before ← snapshot ls
        before `shouldBe` seededSnapshot
        clickInfo ls
        after ← snapshot ls
        after `shouldBe` before

-- * Driving the production path

-- | Seed distinguishable, non-empty state in all three domains. A
--   default-empty selection could not detect the reported clears,
--   because @unit.deselectAll@ and @building.deselect@ are
--   unconditional.
seedAllThreeDomains ∷ LuaBackendState → IO ()
seedAllThreeDomains ls = do
    evalOk ls "return unit.select(1)" `shouldReturn` "true"
    runOk ls "building.select(5)"
    evalOk ls ("return item.select(" <> tshow priorGid <> ")")
        `shouldReturn` "true"

-- | What 'seedAllThreeDomains' establishes, as 'snapshot' reports it.
seededSnapshot ∷ Text
seededSnapshot = "\"units=1 building=5 item=" <> tshow priorGid <> "\""

-- | Open the real ground-item context menu on 'targetGid'.
--   @tryItemMenu@ builds the item list (Info first) and hands it to the
--   real @contextMenu.show@, which renders clickable rows. Reached
--   through @scripts.init_context_menu@, the entry point production
--   uses, rather than the split-out module directly.
openItemMenu ∷ LuaBackendState → IO ()
openItemMenu ls = do
    claimed ← evalOk ls
        "return require('scripts.init_context_menu').tryItemMenu(10, 20)"
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

-- * Fixture

-- | One in-memory page holding the two ground items, the unit and the
--   building, so the page-scoped selection verbs (#76/#78/#2300) are
--   the production ones rather than stubs. No generation parameters, so
--   no world worker.
installInfoRowWorld ∷ EngineEnv → IO ()
installInfoRowWorld env = do
    ws ← emptyWorldState
    prior ← atomicModifyIORef' (wsGroundItemsRef ws) $
                spawnGroundItem (mkItem 700) 3 4
    target ← atomicModifyIORef' (wsGroundItemsRef ws) $
                 spawnGroundItem (mkItem 701) 5 6
    -- Pins what 'priorGid' / 'targetGid' assume about a fresh page's
    -- allocator, so a change there fails here rather than silently
    -- seeding ids the assertions do not name.
    (prior, target) `shouldBe` (priorGid, targetGid)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(infoPage, ws)], wmVisible = [infoPage] }
    writeIORef (itemManagerRef env) emptyItemManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.singleton priorUid liveUnit, umNextId = 2 }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmInstances = HM.singleton priorBid liveBuilding
        , bmNextId = 6 }
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)

-- | A real Lua backend with the full API registered and the world
--   fixture installed. @item.hitTestAt@ is the only replaced verb --
--   the real one needs rendered geometry, and which item was
--   right-clicked is fixture, not behavior.
infoRowBackend ∷ EngineEnv → IO LuaBackendState
infoRowBackend env = do
    installInfoRowWorld env
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    runOk ls ("package.loaded['scripts.hud'] = { menuFont = 1 }; \
              \item.hitTestAt = function() return "
              <> tshow targetGid <> " end")
    pure ls

-- | Nothing about the instance is read by @item.select@ or
--   @item.getSelected@ -- only its presence in @gisItems@ is.
mkItem ∷ Word64 → ItemInstance
mkItem iid = ItemInstance
    { iiDefName = "info_row_marker", iiCurrentFill = 0, iiQuality = 100
    , iiCondition = 100, iiWeight = 1.0, iiSharpness = 100
    , iiContents = [], iiInstanceId = iid, iiTemp = Nothing
    , iiBulk = Just 1.0, iiStorage = Nothing
    }

-- | Only 'uiPage' matters to the selection verbs; the rest are inert
--   placeholders (the minimal-instance shape
--   'Test.Headless.UI.UnitInfoRowSelection' uses for its own fixture).
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
