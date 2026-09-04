{-# LANGUAGE Strict #-}
-- | "power.placeNode page ownership" (#1205): the supplying unit must
--   live on the page the placement resolves to.
--
--   Driven through the REAL registered production @power.placeNode@,
--   because only that surface exercises BOTH page-resolution forms —
--   the explicit @pageId@ argument and the implicit active world — and
--   both have to be refused when the unit belongs somewhere else.
--   'Test.Headless.Power.Types' covers the pure registry transitions
--   and says outright that this integrated path is not covered there.
--
--   The engine is this spec's own (@aroundAll withHeadlessEngine@ in
--   @Spec.hs@): it WRITES the unit/building manager refs and both
--   pages' node registries, the same reason
--   'Test.Headless.Unit.TransferApi' does not share the worldgen
--   engine. Both pages are in-memory 'emptyWorldState' pages with a
--   synthetic flat chunk, so nothing here costs worldgen.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "power placement page ownership"'@.
module Test.Headless.Power.Placement (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (newIORef, readIORef, writeIORef)
import Building.Schema
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingManager(..)
    , emptyBuildingManager )
import Building.Command.Types (BuildingCommand(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..), emptyPowerNodes)
import Power.Base (PowerNodeSpec(..))
import Structure.Types (emptyChunkStructures)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )
import World.Tile.Types (WorldTileData(..))

-- * Fixture identities

-- | The active page — where a build tool would normally be placing.
activePage ∷ WorldPageId
activePage = WorldPageId "pwr_active"

-- | A loaded but NOT visible page. The supplier lives here, so it is
--   off the active world exactly as issue #1205's live reproduction
--   had it.
hiddenPage ∷ WorldPageId
hiddenPage = WorldPageId "pwr_hidden"

-- | The supplier: on 'hiddenPage', carrying the panel.
supplierUid ∷ UnitId
supplierUid = UnitId 1

-- | The item instance ids the supplier carries. The panel is
--   deliberately NOT first in the list, so an assertion on exact order
--   can tell "nothing happened" apart from "popped and spliced back".
rationIid, panelIid, spareIid ∷ Word64
rationIid = 101
panelIid  = 102
spareIid  = 103

-- | Anywhere inside the synthetic flat chunk. Placement here succeeds
--   on either page, so every refusal below is about ownership and
--   nothing else.
placeX, placeY ∷ Int
placeX = 4
placeY = 6

-- * Fixtures

mkItem ∷ Text → Word64 → ItemInstance
mkItem name iid = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 1.0
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

-- | Only the fields this path reads carry any weight; mirrors
--   'Test.Headless.Unit.TransferApi.minimalDef'.
minimalDef ∷ UnitDef
minimalDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

mkUnit ∷ WorldPageId → [ItemInstance] → UnitInstance
mkUnit page inv = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 4, uiGridY = 6, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | The building side of a placed solar panel. @bdBuildWork = 0@ is
--   the instant-build path power nodes ride on, and @bdPowerNode@ is
--   what makes it placeable at all since #1148 — the def carries the
--   role + rating its shipped YAML declares.
panelBuildingDef ∷ BuildingDef
panelBuildingDef = BuildingDef
    { bdName = "solar_panel", bdDisplayName = "Solar Panel"
    , bdCategory = "Power", bdDescription = ""
    , bdTextures = legacyAssets (TextureHandle 0), bdIconTexture = TextureHandle 0
    , bdTileW = 1, bdTileH = 1, bdPlacement = "flat_ground"
    , bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0
    , bdOperations = [], bdAnimations = HM.empty
    , bdRoleAnims = Map.empty
    , bdVisualClass     = FreestandingInstallation, bdPowerDrain = 0
    , bdPowerNode = Just (PowerNodeSource 400)
    }

-- | An ordinary building that declares no power node — the #1148
--   control. It is a real, spawnable def, so a "false" from
--   power.isPlaceable can only come from the missing declaration.
ordinaryBuildingDef ∷ BuildingDef
ordinaryBuildingDef = panelBuildingDef
    { bdName = "shed", bdDisplayName = "Shed", bdCategory = "Storage"
    , bdPowerNode = Nothing }

-- | A flat, fluid-free chunk at (0,0) — the loaded terrain
--   'Building.Placement.canPlaceAt' needs.
--
--   Unlike 'Test.Headless.Building.Placement.flatChunkAt' this one
--   carries a REAL per-tile column vector, because the page it lands on
--   is visible and the live world thread renders it every tick:
--   'World.Render.Quads' indexes @lcTiles@ and then reads the column at
--   the surface z, so an empty column vector (or a surface z the column
--   doesn't reach) crashes the world thread. One material at z 0, with
--   both surface maps agreeing on 0, is the smallest chunk that both
--   validates and draws.
flatChunk ∷ LoadedChunk
flatChunk =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.singleton 1
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area 0
        , lcTerrainSurfaceMap = VU.replicate area 0
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

flatTiles ∷ WorldTileData
flatTiles = WorldTileData
    { wtdChunks    = HM.singleton (lcCoord flatChunk) flatChunk
    , wtdMaxChunks = 1
    }

-- * Scene

-- | Two loaded pages, only 'activePage' visible, both carrying the same
--   flat terrain; the supplier lives on 'hiddenPage' with the panel
--   sandwiched between two other items. Returns each page's
--   'WorldState' so a scenario can read its node registry back.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsActive ← emptyWorldState
    wsHidden ← emptyWorldState
    writeIORef (wsTilesRef wsActive) flatTiles
    writeIORef (wsTilesRef wsHidden) flatTiles
    writeIORef (wsPowerNodesRef wsActive) emptyPowerNodes
    writeIORef (wsPowerNodesRef wsHidden) emptyPowerNodes
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(activePage, wsActive), (hiddenPage, wsHidden)]
        , wmVisible = [activePage]
        }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" minimalDef
        , umInstances = HM.singleton supplierUid $
            mkUnit hiddenPage [ mkItem "ration" rationIid
                              , mkItem "solar_panel" panelIid
                              , mkItem "wiring" spareIid ]
        }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList [ ("solar_panel", panelBuildingDef)
                               , ("shed", ordinaryBuildingDef) ] }
    _ ← drainBuildingQueue env
    pure (wsActive, wsHidden)

-- | The headless harness starts no building thread, so 'BuildingSpawn'
--   commands accumulate. Empty the queue before each scenario, and
--   read it back after to prove a refusal queued nothing.
drainBuildingQueue ∷ EngineEnv → IO [BuildingCommand]
drainBuildingQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (buildingQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- * Live-state readers

-- | The supplier's inventory as (instance id, def name) IN ORDER —
--   instance identity and position both, since a pop-then-rollback
--   that lost the index would still carry the right ids.
inventoryOf ∷ EngineEnv → IO [(Word64, Text)]
inventoryOf env = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup supplierUid (umInstances um) of
        Nothing → []
        Just u  → [(iiInstanceId i, iiDefName i) | i ← uiInventory u]

-- | Every allocated node on a page as (node id, building id), plus the
--   registry's next id — an allocation that was rolled back would still
--   move the counter.
nodesOn ∷ WorldState → IO ([(Word32, Word32)], Word32)
nodesOn ws = do
    nodes ← readIORef (wsPowerNodesRef ws)
    let rows = [ (unPowerNodeId (pnId n), unBuildingId (pnBuilding n))
               | n ← HM.elems (pnsNodes nodes) ]
    pure (rows, pnsNextId nodes)

-- | Placed building instances plus the id counter.
buildingsIn ∷ EngineEnv → IO ([BuildingId], Word32)
buildingsIn env = do
    bm ← readIORef (buildingManagerRef env)
    pure (HM.keys (bmInstances bm), bmNextId bm)

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    loaded ← executeDebugLua (lbsLuaState ls) formatterLua
    loaded `shouldNotSatisfy` isLuaError
    pure ls

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | @__pn(nodeId, buildingId)@ flattens power.placeNode's two return
--   values into one string: @"ok|<node>|<building>"@ on success, or
--   @"nil|<reason>"@ on the nil-plus-reason failure shape. The debug
--   console reports only the first return value, so the pair has to be
--   folded together in Lua.
formatterLua ∷ Text
formatterLua = T.concat
    [ "_G.__pn = function(a, b) "
    , "  if a == nil then return 'nil|' .. tostring(b) end; "
    , "  return 'ok|' .. tostring(a) .. '|' .. tostring(b); "
    , "end; "
    , "return 'ok'"
    ]

-- | Debug-console return values come back JSON-encoded, so a Lua string
--   arrives quoted.
q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | @power.placeNode(supplier, 'solar_panel', x, y)@ with an optional
--   explicit page, through the registered production API.
placeNode ∷ LuaBackendState → Maybe WorldPageId → IO Text
placeNode ls mPage = executeDebugLua (lbsLuaState ls) $ T.concat
    [ "return _G.__pn(power.placeNode("
    , T.pack (show (1 ∷ Int)), ", 'solar_panel', "
    , T.pack (show placeX), ", ", T.pack (show placeY)
    , case mPage of
        Nothing               → ""
        Just (WorldPageId pg) → ", '" <> pg <> "'"
    , "))"
    ]

-- | The inventory the scene starts with, in order.
startingInventory ∷ [(Word64, Text)]
startingInventory =
    [ (rationIid, "ration")
    , (panelIid, "solar_panel")
    , (spareIid, "wiring") ]

-- | 'BuildingCommand' has no 'Eq' instance, so an empty queue is
--   asserted by draining and reporting whatever was found.
expectNoBuildingQueued ∷ EngineEnv → IO ()
expectNoBuildingQueued env = do
    queued ← drainBuildingQueue env
    case queued of
        [] → pure ()
        cmds → expectationFailure $
            "expected no queued building command, got: " <> show cmds

-- | Assert that a refusal touched nothing: the supplier still holds
--   every instance at its original index, neither page allocated a
--   node, the building manager gained no instance and did not move its
--   counter, and no 'BuildingSpawn' was queued.
expectNothingHappened ∷ EngineEnv → WorldState → WorldState → IO ()
expectNothingHappened env wsActive wsHidden = do
    inv ← inventoryOf env
    inv `shouldBe` startingInventory
    activeNodes ← nodesOn wsActive
    hiddenNodes ← nodesOn wsHidden
    activeNodes `shouldBe` ([], 1)
    hiddenNodes `shouldBe` ([], 1)
    buildings ← buildingsIn env
    buildings `shouldBe` ([], 1)
    expectNoBuildingQueued env

spec ∷ SpecWith EngineEnv
spec = do
    ownershipSpec
    placeabilitySpec
    footprintClaimSpec

ownershipSpec ∷ SpecWith EngineEnv
ownershipSpec = describe "power placement page ownership (#1205)" $ do

    it "refuses an EXPLICIT-page placement whose supplier is elsewhere" $ \env → do
        (wsActive, wsHidden) ← resetScene env
        ls ← newBareLuaBackend env
        -- The supplier is on hiddenPage; the placement names activePage.
        r ← placeNode ls (Just activePage)
        r `shouldBe` q "nil|unit is not on page pwr_active"
        expectNothingHappened env wsActive wsHidden

    it "refuses the IMPLICIT active-page form the same way" $ \env → do
        (wsActive, wsHidden) ← resetScene env
        ls ← newBareLuaBackend env
        -- No page argument: resolution falls to the active world, which
        -- is still not the supplier's. This is the form the shipped
        -- build tool uses, so an active-page change between selection
        -- and commit lands exactly here.
        r ← placeNode ls Nothing
        r `shouldBe` q "nil|unit is not on page pwr_active"
        expectNothingHappened env wsActive wsHidden

    it "reports the page mismatch, not a missing item" $ \env → do
        _ ← resetScene env
        ls ← newBareLuaBackend env
        r ← placeNode ls (Just activePage)
        -- The supplier IS carrying a solar_panel; blaming the inventory
        -- would send a caller hunting for the wrong bug.
        r `shouldNotSatisfy` T.isInfixOf "has no"
        r `shouldSatisfy` T.isInfixOf "not on page"

    it "still places onto the supplier's OWN (hidden) page" $ \env → do
        (wsActive, wsHidden) ← resetScene env
        ls ← newBareLuaBackend env
        r ← placeNode ls (Just hiddenPage)
        r `shouldBe` q "ok|1|1"
        -- Exactly the panel instance left, order otherwise intact.
        inv ← inventoryOf env
        inv `shouldBe` [(rationIid, "ration"), (spareIid, "wiring")]
        -- The node landed on the supplier's page, not the active one.
        hiddenNodes ← nodesOn wsHidden
        hiddenNodes `shouldBe` ([(1, 1)], 2)
        activeNodes ← nodesOn wsActive
        activeNodes `shouldBe` ([], 1)
        (_, nextBid) ← buildingsIn env
        nextBid `shouldBe` 2
        queued ← drainBuildingQueue env
        case queued of
            [BuildingSpawn bid defName sx sy _ pid] → do
                bid `shouldBe` BuildingId 1
                defName `shouldBe` "solar_panel"
                (sx, sy) `shouldBe` (placeX, placeY)
                pid `shouldBe` hiddenPage
            other → expectationFailure $
                "expected one BuildingSpawn, got: " <> show other

    it "rolls a same-page rejection back to the item's ORIGINAL index" $ \env → do
        (_, wsHidden) ← resetScene env
        ls ← newBareLuaBackend env
        -- Same page, but no loaded chunk out here: canPlaceAt refuses
        -- AFTER the pop, so this is the rollback path — the behaviour
        -- the ownership check must leave untouched.
        r ← executeDebugLua (lbsLuaState ls)
            "return _G.__pn(power.placeNode(1, 'solar_panel', 9000, 9000, 'pwr_hidden'))"
        r `shouldBe` q "nil|chunk not loaded"
        inv ← inventoryOf env
        inv `shouldBe` startingInventory
        hiddenNodes ← nodesOn wsHidden
        hiddenNodes `shouldBe` ([], 1)
        expectNoBuildingQueued env

-- | The other half of the same registry: since #1148 both
--   @power.isPlaceable@ and @power.placeNode@ answer from the building
--   def's own 'bdPowerNode', so this group pins all three answers the
--   deleted hardcoded catalogue used to give — and the refusal path
--   that must NOT touch the supplier's inventory.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "power placeability"'@.
placeabilitySpec ∷ SpecWith EngineEnv
placeabilitySpec =
  describe "power placeability comes off the building def (#1148)" $ do

    it "says yes for a def that declares a node" $ \env → do
        _ ← resetScene env
        ls ← newBareLuaBackend env
        isPlaceable ls "solar_panel" `shouldReturn` q "true"

    it "says no for a def that declares none" $ \env → do
        _ ← resetScene env
        ls ← newBareLuaBackend env
        isPlaceable ls "shed" `shouldReturn` q "false"

    it "says no for a name with no building def at all" $ \env → do
        _ ← resetScene env
        ls ← newBareLuaBackend env
        -- `wiring` is the shipped case: a real item the supplier is
        -- carrying, but never a building.
        isPlaceable ls "wiring" `shouldReturn` q "false"

    it "refuses a non-power item WITHOUT touching the inventory" $ \env → do
        (_, wsHidden) ← resetScene env
        ls ← newBareLuaBackend env
        -- The supplier's own page and a placeable tile, so the only
        -- thing wrong is the item. The catalogue check has to happen
        -- BEFORE the pop: a pop-then-rollback would pass an
        -- order-blind assertion but reorder a duplicate.
        r ← placeNamed ls "wiring" hiddenPage
        r `shouldBe` q "nil|not a placeable power item"
        inv ← inventoryOf env
        inv `shouldBe` startingInventory
        nodesOn wsHidden `shouldReturn` ([], 1)
        (bids, nextBid) ← buildingsIn env
        bids `shouldBe` []
        nextBid `shouldBe` 1
        expectNoBuildingQueued env

    it "refuses an ordinary building def the same way" $ \env → do
        (_, wsHidden) ← resetScene env
        ls ← newBareLuaBackend env
        r ← placeNamed ls "shed" hiddenPage
        r `shouldBe` q "nil|not a placeable power item"
        inv ← inventoryOf env
        inv `shouldBe` startingInventory
        nodesOn wsHidden `shouldReturn` ([], 1)
        expectNoBuildingQueued env

-- | #2326: @power.placeNode@ is the OTHER producer of a queued
--   @BuildingSpawn@, and the only one with irreversible side effects of
--   its own — it pops an exact item instance out of a unit's inventory
--   and allocates a 'Power.Types.PowerNode' before the building
--   commits. Footprint authority therefore has to answer BEFORE either
--   of those becomes final, which is what these two examples pin from
--   both directions.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "power placement footprint claim"'@.
footprintClaimSpec ∷ SpecWith EngineEnv
footprintClaimSpec =
  describe "power placement footprint claim (#2326)" $ do

    it "refuses a node whose tile a queued building already claimed, \
       \leaving the inventory and the node registry untouched" $ \env → do
        (wsActive, wsHidden) ← resetScene env
        ls ← newBareLuaBackend env
        -- An ordinary spawn takes the tile and is left QUEUED, exactly
        -- as it would be between the Lua thread and the unit thread's
        -- drain. `shed` is 1x1 on the same tile the node wants.
        claimed ← executeDebugLua (lbsLuaState ls) $ T.concat
            [ "return _G.__pn(building.spawn('shed', "
            , T.pack (show placeX), ", ", T.pack (show placeY)
            , ", 'pwr_hidden'))" ]
        -- `__pn` folds two returns: an accepted spawn answers with its
        -- id and no reason.
        claimed `shouldBe` q "ok|1|nil"
        r ← placeNode ls (Just hiddenPage)
        r `shouldBe` q "nil|tile already occupied"
        -- The refusal is the ORDINARY rejection path: the panel is back
        -- at its original index, no node exists on either page, and the
        -- only id spent is the shed's.
        inv ← inventoryOf env
        inv `shouldBe` startingInventory
        nodesOn wsHidden `shouldReturn` ([], 1)
        nodesOn wsActive `shouldReturn` ([], 1)
        (_, nextBid) ← buildingsIn env
        nextBid `shouldBe` 2
        -- Exactly the shed's spawn was ever queued.
        queued ← drainBuildingQueue env
        case queued of
            [BuildingSpawn bid defName _ _ _ _] → do
                bid `shouldBe` BuildingId 1
                defName `shouldBe` "shed"
            other → expectationFailure $
                "expected only the shed's BuildingSpawn, got: " <> show other

    it "claims the tile itself, so a later building spawn is refused" $
        \env → do
            (_, wsHidden) ← resetScene env
            ls ← newBareLuaBackend env
            r ← placeNode ls (Just hiddenPage)
            r `shouldBe` q "ok|1|1"
            -- The node's building has not committed, but its footprint
            -- is taken: nothing else may be admitted onto it.
            blocked ← executeDebugLua (lbsLuaState ls) $ T.concat
                [ "return _G.__pn(building.spawn('shed', "
                , T.pack (show placeX), ", ", T.pack (show placeY)
                , ", 'pwr_hidden'))" ]
            blocked `shouldBe` q "nil|tile already occupied"
            -- The node placement itself is untouched by the refusal.
            nodesOn wsHidden `shouldReturn` ([(1, 1)], 2)
            (_, nextBid) ← buildingsIn env
            nextBid `shouldBe` 2

-- | @power.isPlaceable(name)@ through the registered production API.
isPlaceable ∷ LuaBackendState → Text → IO Text
isPlaceable ls name = executeDebugLua (lbsLuaState ls) $ T.concat
    [ "return tostring(power.isPlaceable('", name, "'))" ]

-- | 'placeNode' with the item name spelled out, for the refusal cases.
placeNamed ∷ LuaBackendState → Text → WorldPageId → IO Text
placeNamed ls name (WorldPageId pg) =
    executeDebugLua (lbsLuaState ls) $ T.concat
        [ "return _G.__pn(power.placeNode(1, '", name, "', "
        , T.pack (show placeX), ", ", T.pack (show placeY)
        , ", '", pg, "'))" ]
