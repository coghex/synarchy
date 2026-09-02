{-# LANGUAGE Strict #-}
-- | "power node demolition" (#1206): a power node is retired by the
--   same 'BuildingDestroy' transaction that removes its building.
--
--   Driven end to end through the PRODUCTION path, because that is
--   where the defect lived: nodes are registered by the real
--   @power.placeNode@, buildings are spawned and destroyed by the real
--   @building.spawn@ / @building.destroy@ (which only ENQUEUE), and the
--   queue is drained by the real
--   'Building.Thread.Command.processAllBuildingCommands' — the same
--   FIFO drain "Unit.Thread" runs every tick. The headless harness
--   starts no unit thread, so this spec owns the drain and can pin
--   spawn-before-destroy ordering exactly.
--
--   'Test.Headless.Power.Types' covers the pure registry transitions,
--   including 'Power.Types.removePowerNode' preserving the id counter;
--   what it cannot show is that anything in production ever CALLS it,
--   which is the whole of this gate.
--
--   The engine is this spec's own (@aroundAll withHeadlessEngine@ in
--   @Spec.hs@) for the same reason 'Test.Headless.Power.Placement' has
--   one: it WRITES the unit/building manager refs and installs its own
--   two-page world manager. Both pages are in-memory 'emptyWorldState'
--   pages with a synthetic flat chunk, so nothing here costs worldgen.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "power node demolition"'@.
module Test.Headless.Power.Demolition (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.List (sort)
import Building.Schema
import Building.Thread.Command (processAllBuildingCommands)
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingManager(..)
    , emptyBuildingManager )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..))
import Power.Base (PowerNodeSpec(..))
import Power.Types
    (PowerNode(..), PowerNodeId(..), PowerNodes(..), PowerRole(..)
    , addPowerNode, emptyPowerNodes)
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

-- | The page the demolitions happen on.
homePage ∷ WorldPageId
homePage = WorldPageId "dem_home"

-- | A second loaded page carrying its OWN node. Its page-local
--   'PowerNodeId's deliberately COLLIDE with 'homePage'\'s (both
--   registries allocate from 1), which is what makes "only the node
--   belonging to that global 'BuildingId' disappears" a real assertion
--   rather than one an id comparison could satisfy by accident.
farPage ∷ WorldPageId
farPage = WorldPageId "dem_far"

-- | One supplier per page: 'power.placeNode' requires the supplying
--   unit to live on the page the placement resolves to (#1205).
homeUid, farUid ∷ UnitId
homeUid = UnitId 1
farUid  = UnitId 2

-- | Tiles inside the synthetic flat chunk. Distinct so no placement
--   is refused for occupancy.
panelTile, battTile, shedTile, laterTile, farTile ∷ (Int, Int)
panelTile = (4, 6)
battTile  = (5, 6)
shedTile  = (6, 6)
laterTile = (7, 6)
farTile   = (4, 6)

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
--   'Test.Headless.Power.Placement.minimalDef'.
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

-- | @bdBuildWork = 0@ is the instant-build path power nodes ride on.
bareDef ∷ Text → BuildingDef
bareDef name = BuildingDef
    { bdName = name, bdDisplayName = name
    , bdCategory = "Power", bdDescription = ""
    , bdTextures = legacyAssets (TextureHandle 0), bdIconTexture = TextureHandle 0
    , bdTileW = 1, bdTileH = 1, bdPlacement = "flat_ground"
    , bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0
    , bdOperations = [], bdAnimations = HM.empty
    , bdRoleAnims = Map.empty
    , bdVisualClass     = FreestandingInstallation, bdPowerDrain = 0, bdPowerNode = Nothing
    }

-- | The two power hosts plus one ORDINARY building — the control that
--   proves a non-power demolition leaves every registry alone.
--
--   The node role/rating rides the def itself since #1148, so the two
--   hosts declare theirs here exactly as their shipped YAML does; the
--   shed declares none, which is what makes it non-placeable.
buildingDefs ∷ HM.HashMap Text BuildingDef
buildingDefs = HM.fromList
    [ ("solar_panel"
      , (bareDef "solar_panel")
            { bdPowerNode = Just (PowerNodeSource 400) })
    , ("high_voltage_battery"
      , (bareDef "high_voltage_battery")
            { bdPowerNode = Just (PowerNodeStorage 5000) })
    , ("shed", (bareDef "shed") { bdCategory = "Storage" })
    ]

-- | A flat, fluid-free chunk at (0,0). Carries a REAL per-tile column
--   vector for the same reason 'Test.Headless.Power.Placement.flatChunk'
--   does: the visible page is rendered by the live world thread every
--   tick, and an empty column crashes it.
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

-- | The live scene: two loaded pages (only 'homePage' visible), a
--   supplier on each carrying both power items, and the three building
--   defs registered. Returns each page's 'WorldState'.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsHome ← emptyWorldState
    wsFar  ← emptyWorldState
    writeIORef (wsTilesRef wsHome) flatTiles
    writeIORef (wsTilesRef wsFar) flatTiles
    writeIORef (wsPowerNodesRef wsHome) emptyPowerNodes
    writeIORef (wsPowerNodesRef wsFar) emptyPowerNodes
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(homePage, wsHome), (farPage, wsFar)]
        , wmVisible = [homePage]
        }
    let kit = [ mkItem "solar_panel" 101
              , mkItem "high_voltage_battery" 102
              , mkItem "solar_panel" 103 ]
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" minimalDef
        , umInstances = HM.fromList
            [ (homeUid, mkUnit homePage kit)
            , (farUid,  mkUnit farPage kit) ]
        }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = buildingDefs }
    pure (wsHome, wsFar)

-- | Run the REAL building-command drain once — the same call
--   "Unit.Thread" makes every tick, so queue ordering here is
--   production ordering.
drainBuildings ∷ EngineEnv → IO ()
drainBuildings env = processAllBuildingCommands
    (loggerRef env)
    (toWorldSimCapability env)
    (toContentRegistriesViewCapability env)
    (toBuildingCapability env)

-- * Live-state readers

-- | Every node on a page as (node id, building id), sorted — the
--   registry-level view, used for the page the Lua queries cannot see
--   (they read the ACTIVE page only).
nodesOn ∷ WorldState → IO [(Word32, Word32)]
nodesOn ws = do
    nodes ← readIORef (wsPowerNodesRef ws)
    pure $ sort [ (unPowerNodeId (pnId n), unBuildingId (pnBuilding n))
                | n ← HM.elems (pnsNodes nodes) ]

nextNodeIdOn ∷ WorldState → IO Word32
nextNodeIdOn ws = pnsNextId ⊚ readIORef (wsPowerNodesRef ws)

buildingIdsIn ∷ EngineEnv → IO [Word32]
buildingIdsIn env = do
    bm ← readIORef (buildingManagerRef env)
    pure $ sort (map unBuildingId (HM.keys (bmInstances bm)))

-- | Make @page@ the visible one, so the active-page Lua queries
--   (@power.getNodeForBuilding@ / @power.listNodes@) report ITS
--   registry. Visibility is the only thing changed.
showPage ∷ EngineEnv → WorldPageId → IO ()
showPage env page = do
    wm ← readIORef (worldManagerRef env)
    writeIORef (worldManagerRef env) wm { wmVisible = [page] }

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | @power.placeNode(uid, item, gx, gy, page)@ through the registered
--   production verb, reporting the BuildingId it allocated (the node id
--   is read back from the registry). Returns 'Nothing' on refusal.
placeNode ∷ LuaBackendState → UnitId → Text → (Int, Int) → WorldPageId
          → IO (Maybe Word32)
placeNode ls (UnitId uid) item (gx, gy) (WorldPageId page) = do
    r ← executeDebugLua (lbsLuaState ls) $ T.concat
        [ "local n, b = power.placeNode(", T.pack (show uid), ", '", item
        , "', ", T.pack (show gx), ", ", T.pack (show gy)
        , ", '", page, "'); if n == nil then return -1 end; return b" ]
    pure (readWord32 r)

-- | @building.spawn(def, gx, gy, page)@ — the ordinary (non-power)
--   control building.
spawnBuilding ∷ LuaBackendState → Text → (Int, Int) → WorldPageId
              → IO (Maybe Word32)
spawnBuilding ls def (gx, gy) (WorldPageId page) = do
    r ← executeDebugLua (lbsLuaState ls) $ T.concat
        [ "local b = building.spawn('", def, "', ", T.pack (show gx), ", "
        , T.pack (show gy), ", '", page, "'); if b == nil then return -1 end; "
        , "return b" ]
    pure (readWord32 r)

-- | @building.destroy(bid)@ — ENQUEUES only; nothing happens until the
--   drain runs.
destroyBuilding ∷ LuaBackendState → Word32 → IO Text
destroyBuilding ls bid = executeDebugLua (lbsLuaState ls) $
    "return tostring(building.destroy(" <> T.pack (show bid) <> "))"

-- | @power.getNodeForBuilding(bid).id@ on the ACTIVE page, or 'Nothing'
--   when the verb returns nil — the exact surface requirement 1 names.
luaNodeIdForBuilding ∷ LuaBackendState → Word32 → IO (Maybe Word32)
luaNodeIdForBuilding ls bid = do
    r ← executeDebugLua (lbsLuaState ls) $ T.concat
        [ "local n = power.getNodeForBuilding(", T.pack (show bid)
        , "); if n == nil then return -1 end; return n.id" ]
    pure (readWord32 r)

-- | The @(node id, building id)@ rows @power.listNodes()@ reports on the
--   ACTIVE page, sorted. Folded to a string because the debug console
--   returns only the first value.
luaListNodes ∷ LuaBackendState → IO [(Word32, Word32)]
luaListNodes ls = do
    r ← executeDebugLua (lbsLuaState ls) $ T.concat
        [ "local out = {}; for _, n in ipairs(power.listNodes()) do "
        , "out[#out+1] = tostring(n.id) .. ':' .. tostring(n.building) end; "
        , "return table.concat(out, ',')" ]
    pure $ sort
        [ (a, b)
        | field ← T.splitOn "," (T.filter (≢ '"') r)
        , not (T.null field)
        , Just (a, b) ← [pairOf (T.splitOn ":" field)] ]
  where
    pairOf [a, b] = (,) <$> readWord32 a <*> readWord32 b
    pairOf _      = Nothing

-- | Debug-console values arrive JSON-encoded; @-1@ is this spec's own
--   "the verb returned nil" sentinel, and 'TR.decimal' rejects its sign,
--   so a nil answer and a malformed one both read as 'Nothing'. A Lua
--   error string ("error: ...") lands there too, which is why every
--   caller below asserts on the result rather than defaulting it.
readWord32 ∷ Text → Maybe Word32
readWord32 raw = case TR.decimal (T.filter (≢ '"') (T.strip raw)) of
    Right (n, rest) | T.null rest → Just (fromIntegral (n ∷ Integer))
    _                             → Nothing

-- | Bind a placement/spawn that MUST have succeeded, failing the
--   example with the console's own answer instead of a bare pattern
--   mismatch.
mustId ∷ Text → IO (Maybe Word32) → IO Word32
mustId label act = act ⌦ \mBid → case mBid of
    Just bid → pure bid
    Nothing  → do
        expectationFailure (T.unpack label <> " did not return a building id")
        error "unreachable"

spec ∷ SpecWith EngineEnv
spec = describe "power node demolition (#1206)" $ do

    it "retires the destroyed host's node and leaves the other node, \
       \on the same page and on another" $ \env → do
        (wsHome, wsFar) ← resetScene env
        ls ← newBareLuaBackend env

        -- Two hosts on the home page, one on the far page. Every
        -- registry allocates from 1, so far's node collides with
        -- home's first node id.
        panelBid ← mustId "home solar_panel" $
            placeNode ls homeUid "solar_panel" panelTile homePage
        battBid ← mustId "home high_voltage_battery" $
            placeNode ls homeUid "high_voltage_battery" battTile homePage
        farBid ← mustId "far solar_panel" $
            placeNode ls farUid "solar_panel" farTile farPage
        drainBuildings env

        homeBefore ← nodesOn wsHome
        farBefore  ← nodesOn wsFar
        homeBefore `shouldBe` [(1, panelBid), (2, battBid)]
        farBefore  `shouldBe` [(1, farBid)]
        -- The colliding page-local id is the point of the far page.
        map fst farBefore `shouldSatisfy` any (`elem` map fst homeBefore)

        -- Destroy the SOURCE host. Its node goes; the storage node on
        -- the same page and the far page's node both stay.
        _ ← destroyBuilding ls panelBid
        drainBuildings env
        nodesOn wsHome ⌦ (`shouldBe` [(2, battBid)])
        nodesOn wsFar  ⌦ (`shouldBe` [(1, farBid)])
        luaNodeIdForBuilding ls panelBid ⌦ (`shouldBe` Nothing)
        luaNodeIdForBuilding ls battBid ⌦ (`shouldBe` Just 2)
        luaListNodes ls ⌦ (`shouldBe` [(2, battBid)])

        -- Now the STORAGE host. The far page's node is still untouched.
        _ ← destroyBuilding ls battBid
        drainBuildings env
        nodesOn wsHome ⌦ (`shouldBe` [])
        nodesOn wsFar  ⌦ (`shouldBe` [(1, farBid)])
        luaNodeIdForBuilding ls battBid ⌦ (`shouldBe` Nothing)
        luaListNodes ls ⌦ (`shouldBe` [])

        -- The far page's node is reachable, unchanged, from the Lua
        -- surface too — the queries read the ACTIVE page, so this is
        -- the only way to see it there.
        showPage env farPage
        luaNodeIdForBuilding ls farBid ⌦ (`shouldBe` Just 1)
        luaListNodes ls ⌦ (`shouldBe` [(1, farBid)])

    it "destroying a host on one page never touches another page's \
       \node, even while THAT page is the active one" $ \env → do
        (wsHome, wsFar) ← resetScene env
        ls ← newBareLuaBackend env
        panelBid ← mustId "home solar_panel" $
            placeNode ls homeUid "solar_panel" panelTile homePage
        farBid ← mustId "far solar_panel" $
            placeNode ls farUid "solar_panel" farTile farPage
        drainBuildings env

        -- Demolish the HOME host while FAR is the visible page: the
        -- cleanup must resolve by BuildingId across every live page,
        -- not by whichever page happens to be active.
        showPage env farPage
        _ ← destroyBuilding ls panelBid
        drainBuildings env
        nodesOn wsHome ⌦ (`shouldBe` [])
        nodesOn wsFar  ⌦ (`shouldBe` [(1, farBid)])
        luaNodeIdForBuilding ls farBid ⌦ (`shouldBe` Just 1)

    it "never renumbers surviving nodes, and a later placement gets an \
       \id ABOVE the retired one" $ \env → do
        (wsHome, _) ← resetScene env
        ls ← newBareLuaBackend env
        panelBid ← mustId "home solar_panel" $
            placeNode ls homeUid "solar_panel" panelTile homePage
        battBid ← mustId "home high_voltage_battery" $
            placeNode ls homeUid "high_voltage_battery" battTile homePage
        drainBuildings env
        nextBefore ← nextNodeIdOn wsHome
        nextBefore `shouldBe` 3

        _ ← destroyBuilding ls panelBid
        drainBuildings env
        -- The survivor keeps ITS id: retirement is a delete, never a
        -- compaction.
        nodesOn wsHome ⌦ (`shouldBe` [(2, battBid)])
        nextAfter ← nextNodeIdOn wsHome
        nextAfter `shouldBe` nextBefore

        -- The retired id (1) must never be handed out again.
        laterBid ← mustId "replacement solar_panel" $
            placeNode ls homeUid "solar_panel" laterTile homePage
        drainBuildings env
        later ← nodesOn wsHome
        later `shouldBe` [(2, battBid), (3, laterBid)]
        map fst later `shouldSatisfy` all (> 1)

    it "leaves every registry alone when the destroyed building has no \
       \node, and drains spawn-before-destroy" $ \env → do
        (wsHome, wsFar) ← resetScene env
        ls ← newBareLuaBackend env
        panelBid ← mustId "home solar_panel" $
            placeNode ls homeUid "solar_panel" panelTile homePage
        farBid ← mustId "far solar_panel" $
            placeNode ls farUid "solar_panel" farTile farPage
        shedBid ← mustId "shed" $ spawnBuilding ls "shed" shedTile homePage
        -- Spawn and destroy the ordinary building are enqueued BEFORE
        -- either is drained, so the destroy can only work if the FIFO
        -- order held (a destroy processed first would find nothing and
        -- leave the shed standing).
        _ ← destroyBuilding ls shedBid
        drainBuildings env

        buildingIdsIn env ⌦ (`shouldBe` sort [panelBid, farBid])
        nodesOn wsHome ⌦ (`shouldBe` [(1, panelBid)])
        nodesOn wsFar  ⌦ (`shouldBe` [(1, farBid)])
        nextNodeIdOn wsHome ⌦ (`shouldBe` 2)
        nextNodeIdOn wsFar ⌦ (`shouldBe` 2)
        luaNodeIdForBuilding ls panelBid ⌦ (`shouldBe` Just 1)

    it "tolerates a demolition whose node was already gone, and a \
       \dangling node whose building never existed" $ \env → do
        (wsHome, _) ← resetScene env
        ls ← newBareLuaBackend env
        panelBid ← mustId "home solar_panel" $
            placeNode ls homeUid "solar_panel" panelTile homePage
        drainBuildings env

        -- A node riding a BuildingId this session never allocated —
        -- the #758/#763 shape a pre-fix save restores verbatim. A
        -- demolition elsewhere must not sweep it up.
        atomicModifyIORef' (wsPowerNodesRef wsHome) $ \nodes →
            (fst (addPowerNode (BuildingId 999999) PowerStorage 5000 nodes), ())
        _ ← destroyBuilding ls panelBid
        drainBuildings env
        nodesOn wsHome ⌦ (`shouldBe` [(2, 999999)])

        -- Re-destroying an already-demolished building is a no-op, not
        -- a crash and not a second sweep.
        _ ← destroyBuilding ls panelBid
        drainBuildings env
        nodesOn wsHome ⌦ (`shouldBe` [(2, 999999)])
        bm ← readIORef (buildingManagerRef env)
        HM.member (BuildingId panelBid) (bmInstances bm) `shouldBe` False
