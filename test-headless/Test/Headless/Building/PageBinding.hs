{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Build placement page binding" (#1602): ONE build placement is bound
--   to the page its click hit-tested, from the synchronous pick through
--   validation to commit.
--
--   The contract has two halves, and both are exercised here against the
--   REAL registered Lua API and the REAL @scripts/build_tool.lua@ /
--   @scripts/build_tool_remote_warning.lua@ paths:
--
--     * __Page coherence within one call.__ @building.canPlaceAt@ and
--       @building.setGhost@ each resolve the world manager exactly once
--       and derive everything from that single resolution — page id,
--       page-scoped occupancy, placed locations, u-wrap world size,
--       canonical coordinates and terrain. Both fixture pages differ in
--       every one of those, so an answer assembled from two reads is
--       distinguishable from one assembled from a single read.
--     * __Freshness across calls.__ @world.pickTile@ reports the page it
--       hit-tested together with the page-SELECTION generation it
--       resolved under, and a placement that carries that pair is
--       refused once selection has moved — including an A→B→A sequence
--       that ends on the same page id, which no page-id comparison can
--       see.
--
--   The engine here is this module's own ('initializeEngineHeadless',
--   like 'Test.Headless.World.DesignationSeam'\'s engine-backed half):
--   it runs NO worker threads, so a queued 'BuildingSpawn' or
--   'WorldDesignateConstruct' stays in its queue and "nothing was
--   committed" is asserted on the queue itself rather than raced
--   against a drainer.
--
--   Neither page costs worldgen: both are in-memory 'emptyWorldState'
--   pages carrying synthetic flat chunks, the same stand-in
--   @tools/remote_warning_page_guard_probe.py@ makes with two arenas.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Build placement page binding"'@.
module Test.Headless.Building.PageBinding (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua

import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Building.Command.Types (BuildingCommand(..))
import Building.Thread.Command (processAllBuildingCommands)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
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
import World.Construct.Types (ConstructTarget(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Grid (gridToWorld, tileHeight)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState
    , settleSelectionProjection, selectionChangeInFlight, projectedVisible )
import World.Thread.Command (handleWorldCommand)
import World.Thread.Command.Init (handleWorldInitArenaCommand)
import World.Thread.Command.Cursor.Construct
    (handleWorldDesignateConstructCommand)
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
    , bdTexture         = TextureHandle 0, bdIconTexture         = TextureHandle 0
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
    , bdStateAnims      = HM.empty
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

-- | Wrap @world.pickTile@ so a page-selection change lands between the
--   REAL pick and the validation that follows it. The binding handed to
--   the validation is the one the engine's own pick produced — the stub
--   only chooses WHEN the switch happens, never what the token is.
stubPickThenSwitch ∷ LuaBackendState → Text → IO Text
stubPickThenSwitch ls mode = evalDebug ls $ T.concat
    [ "world.pickTile = function(px, py) "
    , "  local gx, gy, gz, page, gen = _G.__realPickTile(px, py); "
    , "  if gx then __pageSwitch('", mode, "') end; "
    , "  return gx, gy, gz, page, gen "
    , "end; return 'stubbed'" ]

-- | Wrap @building.canPlaceAt@ so the change lands between validation
--   and commit instead. The validation itself runs for real, against
--   the still-current page, and answers before the switch.
stubValidateThenSwitch ∷ LuaBackendState → Text → IO Text
stubValidateThenSwitch ls mode = evalDebug ls $ T.concat
    [ "building.canPlaceAt = function(...) "
    , "  local ok, why, stale = _G.__realCanPlaceAt(...); "
    , "  __pageSwitch('", mode, "'); "
    , "  return ok, why, stale "
    , "end; return 'stubbed'" ]

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

-- | Every drained outcome as @kind|outcome|reason@ — the remote-warning
--   scenarios need the kind too.
allOutcomes ∷ LuaBackendState → IO [Text]
allOutcomes ls = do
    raw ← evalDebug ls $ T.concat
        [ "local rows = {}; "
        , "for _, o in ipairs(debug.drainActionOutcomes() or {}) do "
        , "  rows[#rows+1] = tostring(o.kind) .. '|' .. tostring(o.outcome) "
        , "    .. '|' .. tostring(o.reason) end; "
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

-- * Spec

spec ∷ Spec
spec = describe "Build placement page binding (#1602)" $ aroundAll setup $ do
    fixtureSpec
    pickBindingSpec
    apiCoherenceSpec
    emptyVisibleSpec
    staleSpec
    applyTimeSpec
    pendingSpec
  where
    -- Isolation wraps the boot, not the other way round (#1357): engine
    -- init is itself a config writer, so a scratch resource root
    -- established afterwards would already be too late. It stays open
    -- for the whole group because the engine booted inside it —
    -- @scripts/@ is symlinked there, so the real build-tool Lua still
    -- loads.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadless
        ls ← newBareLuaBackend env
        installPageSwitch env ls
        _ ← rememberRealVerbs ls
        act (env, ls)

-- | The discriminators the rest of the module leans on really do
--   discriminate. Without this, a coherence assertion could pass
--   against a fixture where both pages happen to agree.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "the two fixture pages really differ" $ do

    it "canonicalises page A's alias differently under each world size" $
        \_ → do
            let aliased = aliasOfA occupiedA
            uncurry (canonicalTile sizeA) aliased `shouldBe` occupiedA
            uncurry (canonicalTile sizeB) aliased `shouldNotBe` occupiedA

    it "loads a chunk on page B that page A does not have" $ \_ → do
        let cc = ChunkCoord 0 1
        HM.member cc (wtdChunks tilesA) `shouldBe` False
        HM.member cc (wtdChunks tilesB) `shouldBe` True

    it "gives the two pages different terrain elevations" $ \_ →
        terrainZA `shouldNotBe` terrainZB

pickBindingSpec ∷ SpecWith (EngineEnv, LuaBackendState)
pickBindingSpec =
  describe "world.pickTile reports the page it hit-tested (#1602 r1)" $ do

    it "returns the visible page id and its selection generation" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            (px, py) ← aimAt env placeTile terrainZA
            got ← evalDebug ls $ T.concat
                [ "local gx, gy, gz, page, g = world.pickTile("
                , tshow px, ", ", tshow py, "); "
                , "return tostring(gx) .. '|' .. tostring(gy) .. '|' "
                , "  .. tostring(page) .. '|' .. tostring(g)" ]
            got `shouldBe` T.concat
                [ tshow (fst placeTile), "|", tshow (snd placeTile)
                , "|", unWorldPageId pageA, "|", tshow gen ]

    it "reports a DIFFERENT generation after page selection moves, even \
       \back to the same page (A→B→A)" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        before' ← selectionGen env
        _ ← evalDebug ls "__pageSwitch('aba'); return 'switched'"
        mgr ← readIORef (worldManagerRef env)
        -- Same page id, different generation: a page-id comparison
        -- would see nothing at all here.
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldNotBe` before'

    it "does not move the generation when a show/hide changes nothing" $
        \(env, _) → do
            _ ← resetScene env
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
            before' ← selectionGen env
            -- Page A is already visible; page B is already hidden.
            handleWorldShowCommand wsc logger pageA
            handleWorldHideCommand wsc logger pageB
            selectionGen env `shouldReturn` before'

apiCoherenceSpec ∷ SpecWith (EngineEnv, LuaBackendState)
apiCoherenceSpec = describe "one resolution answers the whole call" $ do

  describe "building.canPlaceAt (#1602 r3)" $ do

    it "filters occupancy to the VISIBLE page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        -- Page A's own occupant blocks; page B's, at a different tile,
        -- must not.
        canPlaceAt ls shedName occupiedA Nothing
            `shouldReturn` "false|tile already occupied|false"
        canPlaceAt ls shedName occupiedB Nothing
            `shouldReturn` "true|nil|false"

    it "reads world size, occupancy and terrain from the SAME page" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            -- The alias canonicalises onto page A's occupied tile under
            -- page A's world size, and onto an unloaded chunk under page
            -- B's. One answer therefore names which page supplied the
            -- size, the occupancy filter AND the terrain.
            canPlaceAt ls shedName (aliasOfA occupiedA) Nothing
                `shouldReturn` "false|tile already occupied|false"

    it "reads terrain from the VISIBLE page, not a registered one" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            canPlaceAt ls shedName onlyLoadedOnB Nothing
                `shouldReturn` "false|chunk not loaded|false"

    it "reads placed locations from the VISIBLE page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        canPlaceAt ls portalName insideLocA Nothing
            `shouldReturn` "false|inside a location's bounds|false"
        canPlaceAt ls portalName insideLocB Nothing
            `shouldReturn` "true|nil|false"

    it "accepts a binding that still holds" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "refuses a binding naming a page that is no longer visible, even \
       \at the current generation" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        -- The generation half alone would accept this (it has not
        -- moved); the page half is what refuses it, so a supplied id is
        -- never taken and then quietly ignored.
        canPlaceAt ls shedName placeTile (Just (pageB, gen))
            `shouldReturn` "false|page binding stale|true"

  describe "building.setGhost (#1602 r9)" $ do

    it "canonicalises by the visible page's world size AND elevates from \
       \that same page's terrain" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        let aliased = aliasOfA placeTile
        _ ← evalDebug ls $ T.concat
            [ "building.setGhost('", shedName, "', "
            , tshow (fst aliased), ", ", tshow (snd aliased)
            , ", true); return 'set'" ]
        ghost ← ghostOf env
        fmap (\g → (bgGridX g, bgGridY g, bgGridZ g)) ghost
            `shouldBe` Just (fst placeTile, snd placeTile, terrainZA)

emptyVisibleSpec ∷ SpecWith (EngineEnv, LuaBackendState)
emptyVisibleSpec =
  describe "empty-visible behaviour is unchanged (#1602 r10)" $ do

    it "canPlaceAt says 'no active world' with NO page registered" $
        \(env, ls) → do
            resetNoWorlds env
            _ ← clearStubs ls
            canPlaceAt ls shedName placeTile Nothing
                `shouldReturn` "false|no active world|false"

    it "canPlaceAt says 'no world loaded' with a page registered but \
       \none visible" $ \(env, ls) → do
        resetHiddenOnly env
        _ ← clearStubs ls
        -- The registered-but-hidden page is NOT silently used: its
        -- terrain would have made this placeable.
        canPlaceAt ls shedName placeTile Nothing
            `shouldReturn` "false|no world loaded|false"

    it "setGhost falls back to unwrapped coordinates and elevation 0" $
        \(env, ls) → do
            resetHiddenOnly env
            _ ← clearStubs ls
            let aliased = aliasOfA placeTile
            _ ← evalDebug ls $ T.concat
                [ "building.setGhost('", shedName, "', "
                , tshow (fst aliased), ", ", tshow (snd aliased)
                , ", true); return 'set'" ]
            ghost ← ghostOf env
            fmap (\g → (bgGridX g, bgGridY g, bgGridZ g)) ghost
                `shouldBe` Just (fst aliased, snd aliased, 0)

staleSpec ∷ SpecWith (EngineEnv, LuaBackendState)
staleSpec = describe "a moved page selection rejects the placement" $ do

  describe "the starting-building branch (#1602 r5)" $ do

    it "commits exactly once on the captured page when nothing moves" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            outs ← commitOutcomes ls
            outs `shouldBe` ["accepted|nil"]
            committedPlacements env `shouldReturn`
                [(portalName, fst placeTile, snd placeTile, pageA)]
            designationKeys wsA wsB `shouldReturn` ([], [])

    it "rejects a switch between the pick and the validation" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            _ ← stubPickThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects a switch between the validation and the commit" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            _ ← stubValidateThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects an A→B→A switch despite the page id matching" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls portalName True
            _ ← stubPickThenSwitch ls "aba"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            mgr ← readIORef (worldManagerRef env)
            wmVisible mgr `shouldBe` [pageA]
            expectStale env wsA wsB ls

  describe "the construction.designate branch (#1602 r6)" $ do

    it "designates on the captured page when nothing moves" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            outs ← commitOutcomes ls
            outs `shouldBe` ["accepted|routed to construction.designate"]
            -- Applying the world queue turns the enqueued designation
            -- into a real one on the captured page, and only there.
            committedPlacements env `shouldReturn` []
            designationKeys wsA wsB `shouldReturn` ([placeTile], [])

    it "rejects a switch between the pick and the validation" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← stubPickThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects a switch between the validation and the designation" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← stubValidateThenSwitch ls "toB"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "rejects an A→B→A switch despite the page id matching" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← stubValidateThenSwitch ls "aba"
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

  describe "the remote-warning confirmation branch (#1602 r5)" $ do

    it "carries the ORIGINAL click binding and refuses a stale \
       \confirmation without any commitPlacement record" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "local rw = require('scripts.build_tool_remote_warning'); "
            , "rw.init(0, 0, 0, 1280, 720); "
            , "rw.open('", portalName, "', ", tshow (fst placeTile), ", "
            , tshow (snd placeTile), ", nil, 128, '", unWorldPageId pageA
            , "', ", tshow gen, "); return tostring(rw.isOpen())" ]
        _ ← allOutcomes ls   -- discard the 'presented' record
        _ ← evalDebug ls "__pageSwitch('aba'); return 'switched'"
        _ ← evalDebug ls
            "local rw = require('scripts.build_tool_remote_warning'); \
            \rw.establishHere(); return tostring(rw.isOpen())"
        outs ← allOutcomes ls
        outs `shouldBe`
            [ "buildTool.remoteWarning|confirmed|nil"
            , "buildTool.remoteWarning|revalidationRejected|page binding changed" ]
        committedPlacements env `shouldReturn` []
        designationKeys wsA wsB `shouldReturn` ([], [])

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

-- | Enqueuing is not committing. The Lua-side check answers the caller,
--   but page selection belongs to the WORLD thread, so that is where a
--   bound placement's binding is actually discharged — for the spawn via
--   'World.Command.Types.WorldSpawnBoundBuilding', for the designation
--   in its own handler. These examples move the selection AFTER the
--   command was enqueued, through the real handlers, and drive the real
--   dispatchers over it.
applyTimeSpec ∷ SpecWith (EngineEnv, LuaBackendState)
applyTimeSpec =
  describe "the binding is discharged on the thread that owns selection" $ do

    it "the world thread forwards NOTHING for a bound spawn whose \
       \binding went stale after it was enqueued" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        let wsc = toWorldSimCapability env
        Q.writeQueue (worldQueue env) $
            WorldSpawnBoundBuilding (BuildingId 9) portalName
                (fst placeTile) (snd placeTile) terrainZA pageA gen
        -- Selection moves AFTER the command was enqueued — exactly the
        -- window a Lua-thread check cannot cover — and it moves through
        -- the REAL handlers, on the same thread that then drains the
        -- command, so the two cannot interleave.
        handleWorldHideCommand wsc logger pageA
        handleWorldShowCommand wsc logger pageB
        runWorldQueue env
        -- The insert happens on THIS thread, so there is nothing left
        -- for a later drain to apply — which is the point: a check that
        -- only authorised a queued write would leave that window open.
        forwarded ← drainBuildingQueue env
        map show forwarded `shouldBe` []
        applyQueuedBuildings env
        placed ← placedBuildings env
        placed `shouldMatchList`
            [ (pageA, fst occupiedA, snd occupiedA)
            , (pageB, fst occupiedB, snd occupiedB) ]

    it "the world thread INSERTS the building itself when the binding \
       \held, with nothing left to drain" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        Q.writeQueue (worldQueue env) $
            WorldSpawnBoundBuilding (BuildingId 9) portalName
                (fst placeTile) (snd placeTile) terrainZA pageA gen
        runWorldQueue env
        -- Placed already, before any building-queue drain runs.
        placed ← placedBuildings env
        placed `shouldMatchList`
            [ (pageA, fst occupiedA, snd occupiedA)
            , (pageB, fst occupiedB, snd occupiedB)
            , (pageA, fst placeTile, snd placeTile) ]
        forwarded ← drainBuildingQueue env
        map show forwarded `shouldBe` []

    it "a hide landing between the check and a LATER drain cannot \
       \resurrect the placement, because there is no later drain" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
            Q.writeQueue (worldQueue env) $
                WorldSpawnBoundBuilding (BuildingId 9) portalName
                    (fst placeTile) (snd placeTile) terrainZA pageA gen
            runWorldQueue env
            -- pageA stays REGISTERED, so the drain's own world-gone
            -- guard would not have caught this: only doing the insert
            -- on the selection-owning thread does.
            handleWorldHideCommand wsc logger pageA
            handleWorldShowCommand wsc logger pageB
            applyQueuedBuildings env
            mgr ← readIORef (worldManagerRef env)
            map fst (wmWorlds mgr) `shouldMatchList` [pageA, pageB]
            placed ← placedBuildings env
            placed `shouldMatchList`
                [ (pageA, fst occupiedA, snd occupiedA)
                , (pageB, fst occupiedB, snd occupiedB)
                , (pageA, fst placeTile, snd placeTile) ]

    it "an UNBOUND spawn never reaches that gate at all" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        logger ← readIORef (loggerRef env)
        let wsc = toWorldSimCapability env
        -- Location content-spawning, blueprint staking and power
        -- placement carry no click binding: they go straight to the
        -- building queue and keep landing on their explicit page
        -- however selection moves.
        Q.writeQueue (buildingQueue env) $
            BuildingSpawn (BuildingId 9) portalName
                (fst placeTile) (snd placeTile) terrainZA pageA
        handleWorldHideCommand wsc logger pageA
        handleWorldShowCommand wsc logger pageB
        applyQueuedBuildings env
        placed ← placedBuildings env
        placed `shouldMatchList`
            [ (pageA, fst occupiedA, snd occupiedA)
            , (pageB, fst occupiedB, snd occupiedB)
            , (pageA, fst placeTile, snd placeTile) ]

    it "the world thread writes NO designation for a binding that went \
       \stale after the command was enqueued" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        let wsc = toWorldSimCapability env
        handleWorldHideCommand wsc logger pageA
        handleWorldShowCommand wsc logger pageB
        handleWorldDesignateConstructCommand env logger pageA
            (fst placeTile) (snd placeTile) (fst placeTile) (snd placeTile)
            (CtBuilding shedName) (Just gen)
        designationKeys wsA wsB `shouldReturn` ([], [])

    it "the world thread writes the SAME designation when the binding \
       \held" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateConstructCommand env logger pageA
            (fst placeTile) (snd placeTile) (fst placeTile) (snd placeTile)
            (CtBuilding shedName) (Just gen)
        designationKeys wsA wsB `shouldReturn` ([placeTile], [])

    it "an UNBOUND designation is unaffected by a selection change" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
            handleWorldHideCommand wsc logger pageA
            handleWorldShowCommand wsc logger pageB
            handleWorldDesignateConstructCommand env logger pageA
                (fst placeTile) (snd placeTile)
                (fst placeTile) (snd placeTile) (CtBuilding shedName) Nothing
            designationKeys wsA wsB `shouldReturn` ([placeTile], [])

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

-- | The window round 3 named: a selection change ENQUEUED before the
--   click but applied after it. Comparing generations alone reports
--   "fresh" there — the world thread has not applied anything yet — so
--   the placement would be accepted synchronously and then correctly
--   dropped at the commit, leaving the build tool having recorded an
--   acceptance for a building that never landed. The pending count
--   closes it: the rejection is SYNCHRONOUS, which is what lets the tool
--   record the required outcome and stay armed.
pendingSpec ∷ SpecWith (EngineEnv, LuaBackendState)
pendingSpec =
  describe "a selection change ENQUEUED but not yet applied" $ do

    it "makes canPlaceAt report the binding stale before the world \
       \thread has touched anything" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.hide('", unWorldPageId pageA, "'); return 'queued'" ]
        -- Nothing has been applied: the page is still visible and the
        -- generation has not moved.
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldBe` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

    it "rejects the starting-building click, records the outcome and \
       \leaves placement armed" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.hide('", unWorldPageId pageA, "'); return 'queued'" ]
        (px, py) ← aimAt env placeTile terrainZA
        _ ← clickAt ls (px, py)
        expectStale env wsA wsB ls

    it "rejects the construction.designate click the same way" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            _ ← armBuildTool ls shedName False
            _ ← evalDebug ls $ T.concat
                [ "world.show('", unWorldPageId pageB, "'); return 'queued'" ]
            (px, py) ← aimAt env placeTile terrainZA
            _ ← clickAt ls (px, py)
            expectStale env wsA wsB ls

    it "counts world.initArenaDone too, whose handler also prepends to \
       \wmVisible" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        -- The one selection-changing verb that used to enqueue its
        -- command directly: an already-registered hidden arena's
        -- initArenaDone queued ahead of a click would make the visible
        -- page change under a placement the synchronous check had just
        -- called fresh.
        _ ← evalDebug ls $ T.concat
            [ "world.initArenaDone('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        (wmSelectionPending <$> readIORef (worldManagerRef env))
            `shouldReturn` 1
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

    it "rejects a click queued behind world.initArenaDone, recording the \
       \outcome and staying armed" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.initArenaDone('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        (px, py) ← aimAt env placeTile terrainZA
        _ ← clickAt ls (px, py)
        expectStale env wsA wsB ls

    it "discharges every selection-changing verb it counts" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            -- One of each, then one drain: a verb that incremented
            -- without a matching handler discharge would leave the
            -- count stuck above zero and wedge every later binding.
            _ ← evalDebug ls $ T.concat
                [ "world.hide('", unWorldPageId pageA, "'); "
                , "world.show('", unWorldPageId pageA, "'); "
                , "world.initArenaDone('", unWorldPageId pageB, "'); "
                , "world.destroy('", unWorldPageId pageB, "'); "
                , "return 'queued'" ]
            (wmSelectionPending <$> readIORef (worldManagerRef env))
                `shouldReturn` 4
            runWorldQueue env
            (wmSelectionPending <$> readIORef (worldManagerRef env))
                `shouldReturn` 0

    it "does NOT invalidate a binding for an INEFFECTIVE request" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            -- Showing the already-visible page, and hiding the already
            -- hidden one: ordinary traffic that moves no selection. A
            -- click must still be accepted (requirement 12's
            -- no-page-switch path), even though both are still queued.
            _ ← evalDebug ls $ T.concat
                [ "world.show('", unWorldPageId pageA, "'); "
                , "world.hide('", unWorldPageId pageB, "'); "
                , "return 'queued'" ]
            (wmSelectionPending <$> readIORef (worldManagerRef env))
                `shouldReturn` 2
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "true|nil|false"

    it "commits a click made while only INEFFECTIVE requests are in \
       \flight" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageA, "'); return 'queued'" ]
        (px, py) ← aimAt env placeTile terrainZA
        _ ← clickAt ls (px, py)
        outs ← commitOutcomes ls
        outs `shouldBe` ["accepted|nil"]
        committedPlacements env `shouldReturn`
            [(portalName, fst placeTile, snd placeTile, pageA)]

    it "judges a DEPENDENT sequence in queue order, not against the \
       \applied list" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        -- show(B) then hide(B). Against the APPLIED list the hide looks
        -- like a no-op — B is not visible yet — so judging there would
        -- call the pair harmless. Against the projection the show makes
        -- B visible first, so the hide is a real change.
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageB, "'); "
            , "world.hide('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        mgr0 ← readIORef (worldManagerRef env)
        -- The projection walked show-then-hide in order and came back
        -- to [A]; both were counted as real changes.
        snd (projectedVisible mgr0) `shouldBe` [pageA]
        selectionChangeInFlight mgr0 `shouldBe` True
        -- Drain HALFWAY: the show has landed, the hide has not. The
        -- projection must still report a change in flight — this is the
        -- exact window a placement would be accepted in and then
        -- dropped at the commit.
        runOneWorldCommand env
        mgr1 ← readIORef (worldManagerRef env)
        wmVisible mgr1 `shouldBe` [pageB, pageA]
        wmSelectionPending mgr1 `shouldBe` 1
        gen ← selectionGen env
        canPlaceAt ls shedName placeTile (Just (pageB, gen))
            `shouldReturn` "false|page binding stale|true"
        -- And it settles honestly once the hide lands.
        runWorldQueue env
        mgr2 ← readIORef (worldManagerRef env)
        wmVisible mgr2 `shouldBe` [pageA]
        wmProjectedGen mgr2 `shouldBe` wmSelectionGen mgr2

    it "rejects a click made in that half-drained window" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        _ ← armBuildTool ls portalName True
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageB, "'); "
            , "world.hide('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        runOneWorldCommand env
        -- The show has landed, so page B is what a click now hit-tests;
        -- aim at ITS terrain so the pick resolves and the rejection is
        -- the binding's, not an off-world miss.
        (px, py) ← aimAt env placeTile terrainZB
        _ ← clickAt ls (px, py)
        expectStale env wsA wsB ls

    it "does NOT invalidate a binding for a destroy that touches no \
       \visible page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        -- A page that does not exist, and one that is registered but
        -- HIDDEN. Neither is what any binding names — a pick only ever
        -- resolves the visible head — so neither may cost a click.
        _ ← evalDebug ls $ T.concat
            [ "world.destroy('bind_page_missing'); "
            , "world.destroy('", unWorldPageId pageB, "'); "
            , "return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"
        runWorldQueue env
        -- Still fresh once they have actually been applied.
        selectionGen env `shouldReturn` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "DOES invalidate for a destroy of the visible page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.destroy('", unWorldPageId pageA, "'); return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

    it "does NOT invalidate a binding when a HIDDEN page is \
       \re-initialised" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        -- The visible-page counterpart of this is asserted below; a
        -- hidden page's replacement leaves the binding alone.
        handleWorldInitArenaCommand env logger pageB
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldBe` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "ignores a hide/destroy/re-init of a visible page that is NOT \
       \the head" $ \(env, ls) → do
        _ ← resetSceneBothVisible env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        -- Page B is visible but sits BEHIND A, so nothing done to it can
        -- move the page a binding names. None of these may cost a click,
        -- queued or applied.
        _ ← evalDebug ls $ T.concat
            [ "world.hide('", unWorldPageId pageB, "'); return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"
        runWorldQueue env
        selectionGen env `shouldReturn` gen
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        -- Re-initialising a non-head visible page, likewise.
        _ ← resetSceneBothVisible env
        gen2 ← selectionGen env
        handleWorldInitArenaCommand env logger pageB
        selectionGen env `shouldReturn` gen2
        canPlaceAt ls shedName placeTile (Just (pageA, gen2))
            `shouldReturn` "true|nil|false"
        -- And destroying one.
        _ ← resetSceneBothVisible env
        gen3 ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.destroy('", unWorldPageId pageB, "'); return 'queued'" ]
        canPlaceAt ls shedName placeTile (Just (pageA, gen3))
            `shouldReturn` "true|nil|false"
        runWorldQueue env
        selectionGen env `shouldReturn` gen3

    it "DOES invalidate when the HEAD of a multi-visible list is hidden" $
        \(env, ls) → do
            _ ← resetSceneBothVisible env
            _ ← clearStubs ls
            gen ← selectionGen env
            -- Same list, but now the page removed IS the head, so the
            -- page a binding names really does change — to B.
            _ ← evalDebug ls $ T.concat
                [ "world.hide('", unWorldPageId pageA, "'); return 'queued'" ]
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "false|page binding stale|true"
            runWorldQueue env
            mgr ← readIORef (worldManagerRef env)
            wmVisible mgr `shouldBe` [pageB]
            selectionGen env `shouldNotReturn` gen

    -- One table rather than an example per verb: every selection verb,
    -- in a configuration where its OWN handler will change no selection,
    -- must leave a live binding alone — queued AND once applied. Adding
    -- a verb, or narrowing one's precondition, without extending the
    -- prediction fails here instead of in review.
    describe "no selection verb invalidates a binding when its handler \
             \will change nothing" $
      forM_ noOpSelectionRequests $ \(label, luaCall) →
        it label $ \(env, ls) → do
            _ ← resetSceneBothVisible env
            _ ← clearStubs ls
            gen ← selectionGen env
            _ ← evalDebug ls (luaCall <> " return 'queued'")
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "true|nil|false"
            runWorldQueue env
            selectionGen env `shouldReturn` gen
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "true|nil|false"

    it "still invalidates for a show that a queued init makes REAL" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            -- The show alone would be refused (the page is not
            -- registered), but the init ahead of it registers the page,
            -- so the show WILL prepend and move the head. Predicting
            -- from the applied registration set would miss this.
            _ ← evalDebug ls
                "world.initArena('bind_page_new'); \
                \world.show('bind_page_new'); return 'queued'"
            canPlaceAt ls shedName placeTile (Just (pageA, gen))
                `shouldReturn` "false|page binding stale|true"

    it "heals after a request the handler REFUSES, so a later \
       \ineffective one still costs nothing" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        -- A show for a page that does not exist: predicted effective,
        -- refused by the handler. Without the settle the projection
        -- would stay ahead of the applied generation for good, and the
        -- redundant show below would then read as a change in flight.
        _ ← evalDebug ls "world.show('bind_page_missing'); return 'queued'"
        runWorldQueue env
        gen ← selectionGen env
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageA, "'); return 'queued'" ]
        mgr ← readIORef (worldManagerRef env)
        selectionChangeInFlight mgr `shouldBe` False
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "settles once the world thread applies the change" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        _ ← evalDebug ls $ T.concat
            [ "world.show('", unWorldPageId pageB, "'); return 'queued'" ]
        runWorldQueue env
        (wmSelectionPending <$> readIORef (worldManagerRef env))
            `shouldReturn` 0
        -- A binding taken AFTER the change settled is good again.
        gen ← selectionGen env
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageB, pageA]
        canPlaceAt ls shedName placeTile (Just (pageB, gen))
            `shouldReturn` "true|nil|false"

    it "invalidates a binding when a VISIBLE page is re-initialised \
       \under the same id" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        -- world.init/initArena REPLACE the page's WorldState while
        -- leaving wmVisible alone, so the page id still matches: only
        -- the generation can tell the binding apart from a live one.
        handleWorldInitArenaCommand env logger pageA
        mgr ← readIORef (worldManagerRef env)
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldNotBe` gen
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "false|page binding stale|true"

-- | Every selection-changing verb, paired with a call its own handler
--   will turn into a no-op given 'resetSceneBothVisible' (pages A then
--   B visible, A the head, both registered). None may cost a click.
noOpSelectionRequests ∷ [(String, Text)]
noOpSelectionRequests =
    [ ( "world.show of the page that is already the head"
      , call "world.show" pageA )
    , ( "world.show of a page that is visible but NOT the head"
      , call "world.show" pageB )
    , ( "world.show of a page that is not registered at all"
      , "world.show('bind_page_missing');" )
    , ( "world.hide of a visible page that is not the head"
      , call "world.hide" pageB )
    , ( "world.hide of a page that is not visible"
      , "world.hide('bind_page_missing');" )
    , ( "world.destroy of a visible page that is not the head"
      , call "world.destroy" pageB )
    , ( "world.destroy of a page that does not exist"
      , "world.destroy('bind_page_missing');" )
    , ( "world.initArena replacing a visible page that is not the head"
      , call "world.initArena" pageB )
    , ( "world.initArena registering a brand new page"
      , "world.initArena('bind_page_new');" )
    , ( "world.initArenaDone for the page that is already the head"
      , call "world.initArenaDone" pageA )
    ]
  where
    call verb (WorldPageId pid) =
        T.concat [verb, "('", pid, "');"]
