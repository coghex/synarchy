{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Shared fixture and driving infrastructure for the "Build placement
--   page binding" (#1602) gate, whose façade is
--   "Test.Headless.Building.PageBinding".
--
--   This module owns what more than one of the four spec owners needs:
--   the synthetic two-page world, its deliberate discriminators, the
--   real Lua backend and its debug seam, the controlled command
--   dispatchers, and the readers every owner asserts against. A fixture
--   consumed by exactly one owner lives in that owner's module instead.
--
--   It DEFINES the engine-free half of the fixture lifecycle
--   ('newBareLuaBackend', 'installPageSwitch', 'rememberRealVerbs') but
--   never runs it: the façade calls each exactly once, inside its single
--   'Test.Headless.Harness.Isolation.withIsolatedResourceRoot'.
module Test.Headless.Building.PageBinding.Support
    ( -- * Fixture identity
      pageA, pageB
    , sizeA, sizeB
    , terrainZA, terrainZB
    , occupiedA, occupiedB
    , placeTile
    , insideLocA, insideLocB
    , onlyLoadedOnB
    , aliasOfA
      -- * Terrain fixtures
    , tilesA, tilesB
      -- * Building definitions
    , portalName, shedName
      -- * Scene
    , resetScene, resetSceneBothVisible, resetNoWorlds, resetHiddenOnly
      -- * Admission
    , admitPlacement
      -- * Queue readers and controlled dispatch
    , drainBuildingQueue
    , committedPlacements
    , runWorldQueue
    , runOneWorldCommand
    , applyQueuedBuildings
    , designationKeys
    , placedBuildings
    , ghostOf
    , selectionGen
      -- * Lua plumbing
    , newBareLuaBackend
    , evalDebug
    , installPageSwitch
      -- * The real engine pick
    , aimAt
      -- * Build-tool driving
    , armBuildTool
    , rememberRealVerbs
    , clearStubs
    , clickAt
    , commitOutcomes
    , canPlaceAt
      -- * Shared assertions
    , expectStale
    ) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua

import qualified Data.Map.Strict as Map
import Building.Schema
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Building.Command.Types (BuildingCommand(..))
import Building.Reservation (reserveFootprint)
import Building.Thread.Command (processAllBuildingCommands)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Language.Semantic.Types (ConceptId(..))
import Location.Bounds (RelBounds(..))
import Location.Instance (LocationInstances, buildLocationInstances)
import Test.Headless.Location.Fixture (expectGeometry)
import Location.Overlay.Types (LocationOverlay)
import Location.Types
    ( LocationDef(..), LocationNaming(..), LocationRegistry
    , emptyLocationRegistry, registerLocation )
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Grid (gridToWorld, tileHeight)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState
    , settleSelectionProjection )
import World.Thread.Command (handleWorldCommand)
import World.Thread.Command.UI
    (handleWorldHideCommand, handleWorldShowCommand)
import World.Tile.Types (WorldTileData(..))
import Building.Types (BuildingGhost(..))

-- * Fixture identity

-- | The page the click hit-tests in every scenario below.
pageA ∷ WorldPageId
pageA = WorldPageId "bind_page_a"

-- | The page a mid-click selection change switches to. Deliberately
--   different in EVERY dimension 'building.canPlaceAt' consults, so no
--   assertion below can pass by accident on a shared value.
pageB ∷ WorldPageId
pageB = WorldPageId "bind_page_b"

-- | World sizes in chunks. Both are large enough that every fixture
--   chunk below is already canonical under BOTH (their u ranges are
--   [-4,4) and [-8,8), and no fixture chunk has |u| > 1), so the ONLY
--   coordinate the two sizes disagree about is the deliberate u-alias
--   'aliasOfA' builds.
sizeA, sizeB ∷ Int
sizeA = 8
sizeB = 16

-- | Surface elevations. A ghost elevated to 'terrainZB' while page A is
--   visible is terrain read from the wrong page.
terrainZA, terrainZB ∷ Int
terrainZA = 5
terrainZB = 9

-- | Page A's own occupied tile, and page B's — different tiles, so an
--   occupancy answer names which page's building manager was filtered.
occupiedA, occupiedB ∷ (Int, Int)
occupiedA = (3, 3)
occupiedB = (4, 4)

-- | The tile every placement scenario commits at: on page A's loaded
--   terrain, unoccupied, and close enough to page A's placed location
--   that 'building.remoteCheck' does NOT route the portal through the
--   remote-settlement modal — the direct single-click commit branch.
placeTile ∷ (Int, Int)
placeTile = (12, 12)

-- | A tile inside page A's placed location bounds, and one inside page
--   B's. Only the first may be refused while page A is visible.
insideLocA, insideLocB ∷ (Int, Int)
insideLocA = (8, 8)
insideLocB = (chunkSize + 8, 8)

-- | A tile whose chunk is loaded on page B but NOT on page A.
onlyLoadedOnB ∷ (Int, Int)
onlyLoadedOnB = (3, chunkSize + 3)

-- | The u-alias of a tile in PAGE A's frame. Canonicalising it needs
--   page A's world size; page B's leaves it somewhere else entirely
--   (asserted as a fixture precondition below, so the discriminator is
--   proven to discriminate rather than assumed to).
aliasOfA ∷ (Int, Int) → (Int, Int)
aliasOfA (gx, gy) = (gx + tileAliasStep sizeA, gy - tileAliasStep sizeA)

-- * Terrain fixtures

-- | A flat chunk with a real per-tile column vector — 'pickWorldTile'
--   indexes @lcTiles@, so an empty column vector cannot be picked.
flatChunkAt ∷ ChunkCoord → Int → LoadedChunk
flatChunkAt coord z =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
            { ctStartZ = z
            , ctMats   = VU.singleton 1
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area z
        , lcTerrainSurfaceMap = VU.replicate area z
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

tilesFrom ∷ [LoadedChunk] → WorldTileData
tilesFrom chunks = WorldTileData
    { wtdChunks    = HM.fromList [ (lcCoord c, c) | c ← chunks ]
    , wtdMaxChunks = length chunks
    }

-- | Page A: two chunks in a row. Page B: those two PLUS the row below,
--   which is what makes 'onlyLoadedOnB' discriminate.
tilesA, tilesB ∷ WorldTileData
tilesA = tilesFrom
    [ flatChunkAt (ChunkCoord 0 0) terrainZA
    , flatChunkAt (ChunkCoord 1 0) terrainZA ]
tilesB = tilesFrom
    [ flatChunkAt (ChunkCoord 0 0) terrainZB
    , flatChunkAt (ChunkCoord 1 0) terrainZB
    , flatChunkAt (ChunkCoord 0 1) terrainZB
    , flatChunkAt (ChunkCoord 1 1) terrainZB ]

-- * Location fixtures

testNaming ∷ LocationNaming
testNaming = LocationNaming
    { lnHeads = [ConceptId "KEEP"], lnModifiers = [ConceptId "ASH"] }

locDef ∷ Text → LocationDef
locDef lid = LocationDef
    { ldId       = lid
    , ldLabel    = "Binding Ruin"
    , ldType     = "ruin"
    , ldBuilder  = "room_small"
    , ldAnchor   = []
    , ldMaxCount = 0
    , ldMinSpacing = 0
    , ldContents = []
    , ldBounds   = RelBounds (-2) (-2) 2 2
    , ldMapIcon  = Nothing
    , ldNaming   = testNaming
    }

-- | One placed location per page, in DIFFERENT chunks: page A's sits in
--   chunk (0,0) (bounds around 'insideLocA'), page B's in chunk (1,0)
--   (bounds around 'insideLocB').
instancesFor ∷ ChunkCoord → LocationInstances
instancesFor cc = expectGeometry (buildLocationInstances Nothing registry overlay)
  where
    registry ∷ LocationRegistry
    registry = registerLocation (locDef "bind_loc") emptyLocationRegistry
    overlay ∷ LocationOverlay
    overlay = HM.singleton cc "bind_loc"

genParamsFor ∷ Int → ChunkCoord → WorldGenParams
genParamsFor size locChunk = defaultWorldGenParams
    { wgpWorldSize         = size
    , wgpLocationInstances = instancesFor locChunk
    }

-- * Building definitions

mkDef ∷ Text → Bool → BuildingDef
mkDef name starting = BuildingDef
    { bdName            = name
    , bdDisplayName     = name
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTextures         = legacyAssets (TextureHandle 0), bdIconTexture         = TextureHandle 0
    , bdTileW           = 1
    , bdTileH           = 1
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = starting
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdRoleAnims      = Map.empty
    , bdVisualClass     = FreestandingInstallation
    , bdPowerDrain      = 0
    , bdPowerNode       = Nothing
    }

-- | The two pre-placed fixture buildings, filtered out of every
--   commit assertion.
occupantDefName ∷ Text
occupantDefName = "bind_occupant"

portalName, shedName ∷ Text
portalName = "bind_portal"
shedName   = "bind_shed"

occupantAt ∷ BuildingId → WorldPageId → (Int, Int) → BuildingInstance
occupantAt _ page (gx, gy) = BuildingInstance
    { biDefName            = occupantDefName
    , biPage               = page
    , biTexture            = TextureHandle 0
    , biAnchorX            = gx
    , biAnchorY            = gy
    , biGridZ              = 0
    , biSpawnedAt          = 0
    , biTileW              = 1
    , biTileH              = 1
    , biSpawnRemaining     = 0
    , biBuildProgress      = 0
    , biMaterialsDelivered = HM.empty
    , biStorage            = []
    }

-- * Scene

-- | Both pages installed, page A visible. Returns each page's state so
--   a scenario can read its designation map back.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    writeIORef (wsTilesRef wsA) tilesA
    writeIORef (wsTilesRef wsB) tilesB
    writeIORef (wsGenParamsRef wsA) (Just (genParamsFor sizeA (ChunkCoord 0 0)))
    writeIORef (wsGenParamsRef wsB) (Just (genParamsFor sizeB (ChunkCoord 1 0)))
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageA, wsA), (pageB, wsB)]
        , wmVisible = [pageA] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList [ (portalName, mkDef portalName True)
                               , (shedName,   mkDef shedName False) ]
        , bmInstances = HM.fromList
            [ (BuildingId 1, occupantAt (BuildingId 1) pageA occupiedA)
            , (BuildingId 2, occupantAt (BuildingId 2) pageB occupiedB) ]
        , bmNextId = 3 }
    _ ← drainBuildingQueue env
    _ ← drainWorldQueue env
    pure (wsA, wsB)

-- | 'resetScene' with BOTH pages visible and page A still the head.
--   @wmVisible@ is a list, so this is an ordinary state — @world.show@
--   prepends — and it is the one that separates "the visible SET
--   changed" from "the page a binding names changed".
resetSceneBothVisible ∷ EngineEnv → IO (WorldState, WorldState)
resetSceneBothVisible env = do
    scene ← resetScene env
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmVisible = [pageA, pageB] }, ())
    pure scene

-- | Install only page A (nothing else registered) — the "no worlds at
--   all" empty state.
resetNoWorlds ∷ EngineEnv → IO ()
resetNoWorlds env = writeIORef (worldManagerRef env) emptyWorldManager

-- | Page A registered but NOTHING visible — the other empty state, whose
--   rejection reason must stay distinct from the one above.
resetHiddenOnly ∷ EngineEnv → IO ()
resetHiddenOnly env = do
    wsA ← emptyWorldState
    writeIORef (wsTilesRef wsA) tilesA
    writeIORef (wsGenParamsRef wsA) (Just (genParamsFor sizeA (ChunkCoord 0 0)))
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageA, wsA)], wmVisible = [] }

-- * Admission

-- | Admit a placement exactly the way @building.spawn@ does (#2326),
--   and hand back the 'BuildingId' that transaction allocated.
--
--   A spawn command carries the footprint CLAIM its admission took, and
--   'Building.Thread.Command.applyBuildingSpawn' inserts nothing
--   without one. So a scenario that writes a 'BuildingSpawn' or
--   'World.Command.Types.WorldSpawnBoundBuilding' by hand — which is
--   how the examples below land a selection change at an exact point
--   AROUND one, rather than only before or after a whole click — has to
--   take that claim through the production transaction first. An id
--   invented here would describe a spawn no admission ever accepted,
--   and every "the building landed" assertion under it would be
--   vacuous.
--
--   The def and the page's u-wrap world size are read back out of the
--   live fixture, so this cannot drift from what @resetScene@ installed.
admitPlacement ∷ EngineEnv → WorldPageId → Text → (Int, Int) → IO BuildingId
admitPlacement env pid defName (gx, gy) = do
    bm ← readIORef (buildingManagerRef env)
    wm ← readIORef (worldManagerRef env)
    def ← case HM.lookup defName (bmDefs bm) of
        Just d  → pure d
        Nothing → fail ("admitPlacement: no def " <> T.unpack defName)
    size ← case lookup pid (wmWorlds wm) of
        Nothing → fail ("admitPlacement: no page " <> show pid)
        Just ws → maybe 0 wgpWorldSize <$> readIORef (wsGenParamsRef ws)
    eBid ← atomicModifyIORef' (buildingManagerRef env)
                              (reserveFootprint size pid def gx gy)
    case eBid of
        Right bid  → pure bid
        Left reason → fail ("admitPlacement refused: " <> T.unpack reason)

-- * Queue readers

drainBuildingQueue ∷ EngineEnv → IO [BuildingCommand]
drainBuildingQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (buildingQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

drainWorldQueue ∷ EngineEnv → IO [WorldCommand]
drainWorldQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (worldQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- | The placements that actually COMMITTED, as (defName, gx, gy, page),
--   with the two fixture occupants removed.
--
--   Both real dispatchers are run first — the world thread's, which is
--   where a page-BOUND placement is checked AND inserted, and then the
--   building drain for anything unbound. So this asks "did the placement
--   land", never "was something enqueued": a click that enqueued a
--   command the binding check then refused reports nothing here.
committedPlacements ∷ EngineEnv → IO [(Text, Int, Int, WorldPageId)]
committedPlacements env = do
    runWorldQueue env
    applyQueuedBuildings env
    bm ← readIORef (buildingManagerRef env)
    pure [ (biDefName b, biAnchorX b, biAnchorY b, biPage b)
         | b ← HM.elems (bmInstances bm)
         , biDefName b ≢ occupantDefName ]

-- | Drain the world queue through the REAL world-thread dispatcher, the
--   way "World.Thread" does — including its per-command projection
--   settle, without which a partially drained queue would never
--   re-synchronise.
runWorldQueue ∷ EngineEnv → IO ()
runWorldQueue env = do
    cmds ← drainWorldQueue env
    mapM_ (applyWorldCommand env) cmds

-- | One command through the same path, so a scenario can stop the drain
--   HALFWAY — which is the only way to observe a queue that still holds
--   a dependent selection change after an earlier one has landed.
applyWorldCommand ∷ EngineEnv → WorldCommand → IO ()
applyWorldCommand env cmd = do
    logger ← readIORef (loggerRef env)
    handleWorldCommand env logger cmd
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (settleSelectionProjection mgr, ())

-- | Take exactly one command off the world queue and apply it.
runOneWorldCommand ∷ EngineEnv → IO ()
runOneWorldCommand env = do
    mCmd ← Q.tryReadQueue (worldQueue env)
    forM_ mCmd (applyWorldCommand env)

-- | Both pages' live designation maps — a stale attempt must leave both
--   empty, not merely fail to enqueue.
designationKeys ∷ WorldState → WorldState → IO ([(Int, Int)], [(Int, Int)])
designationKeys wsA wsB = do
    a ← HM.keys <$> readIORef (wsConstructDesignationsRef wsA)
    b ← HM.keys <$> readIORef (wsConstructDesignationsRef wsB)
    pure (a, b)

-- | Placed building instances, so "no building on either page" is
--   asserted against live state as well as the queue.
placedBuildings ∷ EngineEnv → IO [(WorldPageId, Int, Int)]
placedBuildings env = do
    bm ← readIORef (buildingManagerRef env)
    pure [ (biPage b, biAnchorX b, biAnchorY b) | b ← HM.elems (bmInstances bm) ]

ghostOf ∷ EngineEnv → IO (Maybe BuildingGhost)
ghostOf env = readIORef (buildingGhostRef env)

selectionGen ∷ EngineEnv → IO Word64
selectionGen env = wmSelectionGen <$> readIORef (worldManagerRef env)

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

-- | @__pageSwitch(mode)@ — a SYNCHRONOUS page-selection change driven by
--   the REAL production handlers ('handleWorldShowCommand' /
--   'handleWorldHideCommand'), callable from inside a Lua stub. That is
--   what lets a scenario land a selection change at an exact point
--   INSIDE one @handleMouseDown@ call — between the pick and the
--   validation, or between the validation and the commit — rather than
--   only before or after the whole handler.
--
--   @"toB"@ hides A and shows B. @"aba"@ additionally returns to A, so
--   the final visible page id is the one the pick reported and only the
--   selection generation can tell the binding is stale.
installPageSwitch ∷ EngineEnv → LuaBackendState → IO ()
installPageSwitch env ls = Lua.runWith (lbsLuaState ls) $ do
    Lua.pushHaskellFunction switchFn
    Lua.setglobal (Lua.Name "__pageSwitch")
  where
    switchFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
    switchFn = do
        modeArg ← Lua.tostring 1
        Lua.liftIO $ do
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
                hide = handleWorldHideCommand wsc logger
                show' = handleWorldShowCommand wsc logger
            case modeArg of
                Just "toB" → hide pageA >> show' pageB
                Just "aba" → do
                    hide pageA
                    show' pageB
                    hide pageB
                    show' pageA
                _ → pure ()
        pure 0
-- * The real engine pick

-- | Viewport wide enough that the pick resolves rather than being
--   culled — the same generous configuration
--   'Test.Headless.World.Render.PickSeam' uses.
pickZoom ∷ Float
pickZoom = 40.0

pickFbW, pickFbH, pickWinW, pickWinH ∷ Int
pickFbW = 800
pickFbH = 600
pickWinW = 8000
pickWinH = 6000

-- | Park the camera on a tile and return the window pixel that
--   unprojects back to it, so @world.pickTile@ genuinely resolves that
--   tile through the production hit test.
aimAt ∷ EngineEnv → (Int, Int) → Int → IO (Int, Int)
aimAt env (gx, gy) z = do
    let (camX, camY) = gridToWorld FaceSouth gx gy
        (wx, wy0)    = gridToWorld FaceSouth gx gy
        wy     = wy0 + tileHeight * 0.5
        aspect = fromIntegral pickFbW / fromIntegral pickFbH ∷ Float
        px = fromIntegral pickWinW * (((wx - camX) / (pickZoom * aspect)) + 1.0) / 2.0
        py = fromIntegral pickWinH * (((wy - camY) / pickZoom) + 1.0) / 2.0
    writeIORef (cameraRef env) defaultCamera
        { camPosition = (camX, camY), camZoom = pickZoom
        , camFacing = FaceSouth, camZSlice = z }
    writeIORef (windowSizeRef env) (pickWinW, pickWinH)
    writeIORef (framebufferSizeRef env) (pickFbW, pickFbH)
    pure (round px, round py)
-- * Build-tool driving

-- | Arm the real build tool in placement mode on one target. @hud@ is
--   stubbed to the one field the handler reads plus the callback
--   'commitStartingPlacement' invokes — the HUD itself never boots
--   headless.
armBuildTool ∷ LuaBackendState → Text → Bool → IO Text
armBuildTool ls defName starting = evalDebug ls $ T.concat
    [ "local bt = require('scripts.build_tool'); "
    , "bt.hud = { worldId = '", unWorldPageId pageA
    , "', selectDefaultTool = function() end }; "
    , "bt.state.mode = 'placement'; "
    , "bt.state.target = { kind = 'building', def = '", defName
    , "', isStarting = ", if starting then "true" else "false", " }; "
    , "return 'armed'" ]

-- | Remember the REAL engine verbs, once, before any example wraps one.
--   Every wrapper below delegates to these rather than to whatever is
--   currently installed, and 'clearStubs' restores them — otherwise one
--   example's wrapper would still be layered under the next one's and
--   fire a second, unasked-for page switch.
rememberRealVerbs ∷ LuaBackendState → IO Text
rememberRealVerbs ls = evalDebug ls
    "_G.__realPickTile = world.pickTile; \
    \_G.__realCanPlaceAt = building.canPlaceAt; return 'remembered'"
-- | Put every example back on the real verbs and a freshly-required
--   build tool, so no scenario inherits the previous one's wrapper or
--   its leftover placement state.
clearStubs ∷ LuaBackendState → IO Text
clearStubs ls = evalDebug ls
    "world.pickTile = _G.__realPickTile; \
    \building.canPlaceAt = _G.__realCanPlaceAt; \
    \package.loaded['scripts.build_tool'] = nil; \
    \package.loaded['scripts.build_tool_remote_warning'] = nil; \
    \debug.drainActionOutcomes(); return 'cleared'"

-- | One left click at a window pixel, through the real handler.
clickAt ∷ LuaBackendState → (Int, Int) → IO Text
clickAt ls (px, py) = evalDebug ls $ T.concat
    [ "return tostring(require('scripts.build_tool')"
    , ".handleMouseDown(1, ", tshow px, ", ", tshow py, "))" ]

-- | Drained @buildTool.commitPlacement@ outcomes as
--   @outcome|reason@ rows — flattened in Lua because the debug console
--   reports one value.
commitOutcomes ∷ LuaBackendState → IO [Text]
commitOutcomes ls = do
    raw ← evalDebug ls $ T.concat
        [ "local rows = {}; "
        , "for _, o in ipairs(debug.drainActionOutcomes() or {}) do "
        , "  if o.kind == 'buildTool.commitPlacement' then "
        , "    rows[#rows+1] = tostring(o.outcome) .. '|' "
        , "      .. tostring(o.reason) end end; "
        , "return table.concat(rows, ';')" ]
    pure $ filter (not . T.null) (T.splitOn ";" raw)
-- | @building.canPlaceAt@ folded to @ok|reason|stale@.
canPlaceAt ∷ LuaBackendState → Text → (Int, Int) → Maybe (WorldPageId, Word64)
           → IO Text
canPlaceAt ls defName (gx, gy) mBind = evalDebug ls $ T.concat
    [ "local ok, why, stale = building.canPlaceAt('", defName, "', "
    , tshow gx, ", ", tshow gy
    , case mBind of
        Nothing → ""
        Just (WorldPageId pg, gen) →
            T.concat [ ", '", pg, "', ", tshow gen ]
    , "); return tostring(ok) .. '|' .. tostring(why) .. '|' .. tostring(stale)" ]
-- * Shared assertions

-- | Every stale case asserts the same three things (#1602 r6/r8):
--   exactly one distinguishing rejected outcome, no building queued or
--   placed on EITHER page, and no designation on either page.
expectStale ∷ EngineEnv → WorldState → WorldState → LuaBackendState → IO ()
expectStale env wsA wsB ls = do
    outs ← commitOutcomes ls
    outs `shouldBe` ["rejected|page binding changed"]
    -- Drains and APPLIES both dispatchers first, so this is "nothing
    -- committed" rather than the weaker "nothing enqueued".
    committedPlacements env `shouldReturn` []
    designationKeys wsA wsB `shouldReturn` ([], [])
    -- Placement stays armed, exactly as an ordinary invalid-tile
    -- refusal leaves it.
    evalDebug ls "return require('scripts.build_tool').state.mode"
        `shouldReturn` "placement"
    -- Placement stays armed, exactly as an ordinary invalid-tile
    -- refusal leaves it.
    evalDebug ls "return require('scripts.build_tool').state.mode"
        `shouldReturn` "placement"

-- | Run the REAL building-command drain — the same
--   'processAllBuildingCommands' the unit thread runs (there is no
--   separate building thread; see 'Building.Thread.Command'). Nothing is
--   reimplemented here, so what lands is what the engine would land.
applyQueuedBuildings ∷ EngineEnv → IO ()
applyQueuedBuildings env =
    processAllBuildingCommands (loggerRef env)
        (toWorldSimCapability env)
        (toContentRegistriesViewCapability env)
        (toBuildingCapability env)
