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
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua

import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Building.Command.Types (BuildingCommand(..))
import Engine.Asset.Handle (TextureHandle(..))
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
import World.Generate.Coordinates (canonicalTile, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Grid (gridToWorld, tileHeight)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
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
instancesFor cc = buildLocationInstances Nothing registry overlay
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
    , bdTexture         = TextureHandle 0
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

portalName, shedName ∷ Text
portalName = "bind_portal"
shedName   = "bind_shed"

occupantAt ∷ BuildingId → WorldPageId → (Int, Int) → BuildingInstance
occupantAt _ page (gx, gy) = BuildingInstance
    { biDefName            = "bind_occupant"
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

-- | Every queued building SPAWN as (defName, gx, gy, page).
spawnsQueued ∷ EngineEnv → IO [(Text, Int, Int, WorldPageId)]
spawnsQueued env = do
    cmds ← drainBuildingQueue env
    pure [ (n, gx, gy, p) | BuildingSpawn _ n gx gy _ p ← cmds ]

-- | Every queued construction designation as (page, x1, y1).
designationsQueued ∷ EngineEnv → IO [(WorldPageId, Int, Int)]
designationsQueued env = do
    cmds ← drainWorldQueue env
    pure [ (p, x1, y1) | WorldDesignateConstruct p x1 y1 _ _ _ ← cmds ]

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
            spawnsQueued env `shouldReturn`
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
            designationsQueued env `shouldReturn`
                [(pageA, fst placeTile, snd placeTile)]
            designationKeys wsA wsB `shouldReturn` ([], [])
            spawnsQueued env `shouldReturn` []

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
        spawnsQueued env `shouldReturn` []
        placedBuildings env `shouldReturn`
            [(pageA, fst occupiedA, snd occupiedA)
            ,(pageB, fst occupiedB, snd occupiedB)]
        designationKeys wsA wsB `shouldReturn` ([], [])

-- | Every stale case asserts the same three things (#1602 r6/r8):
--   exactly one distinguishing rejected outcome, no building queued or
--   placed on EITHER page, and no designation on either page.
expectStale ∷ EngineEnv → WorldState → WorldState → LuaBackendState → IO ()
expectStale env wsA wsB ls = do
    outs ← commitOutcomes ls
    outs `shouldBe` ["rejected|page binding changed"]
    spawnsQueued env `shouldReturn` []
    designationsQueued env `shouldReturn` []
    designationKeys wsA wsB `shouldReturn` ([], [])
    -- Only the two fixture occupants remain: nothing was placed.
    placed ← placedBuildings env
    placed `shouldMatchList`
        [ (pageA, fst occupiedA, snd occupiedA)
        , (pageB, fst occupiedB, snd occupiedB) ]
    -- Placement stays armed, exactly as an ordinary invalid-tile
    -- refusal leaves it.
    evalDebug ls "return require('scripts.build_tool').state.mode"
        `shouldReturn` "placement"
