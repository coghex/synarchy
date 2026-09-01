{-# LANGUAGE OverloadedStrings #-}
-- | "Every completed 'World.Render.updateWorldTiles' pass publishes one
--   self-consistent scene-statistics snapshot" (#1921).
--
--   The fixture is deliberately synthetic and worldgen-free, following
--   "Test.Headless.World.Render.QuadSnapshot": one in-memory
--   'emptyWorldState' page carrying a single chunk with three solid
--   columns, driven through the REAL 'updateWorldTiles'. That makes the
--   required cache-hit case cheap — a second pass over an unchanged
--   fixture and camera takes the reuse branch — and it makes every
--   scanned count exactly predictable, which a generated world could
--   not.
--
--   What this gate can and cannot see headlessly is itself part of the
--   contract. 'Unit.Render', 'Building.Render' and 'Structure.Render'
--   all return no quads while @rvTextureSystemRef@ is 'Nothing', which
--   is the normal GPU-free state — so units and buildings legitimately
--   report a non-zero SCANNED with zero EMITTED here (their global-map
--   walk happens before that check), while structures report zero on
--   BOTH counts (their check happens before any piece is enumerated).
--   Neither is a failure, and asserting a non-zero emission for them
--   would be asserting behaviour this engine does not have without a
--   GPU. 'tools/scene_stats_probe.py' is where non-zero emissions are
--   proved, in @--offscreen@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "scene assembly telemetry"'@.
module Test.Headless.World.Render.SceneStats (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Data.Text as T
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex
    (Vec2(..), Vec4(..), mkVertexWorld, packWorldUV)
import Engine.Scene.Types (SortableQuad)
import Engine.Scene.Stats
    ( SceneCategory(..), SceneCategoryStat(..), SceneStats(..)
    , sceneCategoryId, sceneCategoryOrder )

import Blood.Types
    ( BloodDecalSpec(..), BloodStore(..), BloodTextureId(..)
    , SeverityBucket(..), addDecal, bdlDecals, bstDecals )
import Building.Types
    ( BuildingDef(..), BuildingGhost(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Item.Ground (GroundItem(..), GroundItems(..), emptyGroundItems)
import Item.Types (ItemInstance(..))
import Location.Instance
    (LocationInstances, allocateLocationInstance, emptyLocationInstances)
import Location.Bounds (RelBounds(..))
import Location.Types (LocationDef(..), LocationNaming(..))
import Language.Semantic.Types (ConceptId(..))
import Structure.Palette (TexPalette, emptyTexPalette, internPath)
import Structure.Types
    ( ChunkStructures, StructurePieceData(..), StructureSlot(..)
    , emptyChunkStructures )
import Structure.Render (structureChunkQuads, structureChunkQuadsScanned)
import Structure.WallCatalog (emptyStructureWallCatalog)
import Unit.Types
    ( UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager )
import Unit.Faction (Faction(..))
import Unit.Direction (Direction(..))
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Cursor.Types (CursorState(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Construct.Types
    (ConstructTarget(..), newConstructDesignation)
import World.Mine.Types (MineDesignation(..))
import World.Page.Types (WorldPageId(..))
import World.Render (updateWorldTiles)
import World.Render.ViewBounds (ViewBounds, computeViewBounds)
import World.Render.Zoom.Cursor (pixelToChunkOrigin)
import World.Render.Zoom.Types (BakedZoomEntry(..))
import World.Tool.Types (ToolMode(..))
import World.Material.Id (MaterialId(..))
import World.Spoil.Types (SpoilPile(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Thread.Command.Basic
    (handleWorldDestroyAllCommand, handleWorldDestroyCommand)
import World.Tile.Types (WorldTileData(..))

-- * Fixture

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "scene_stats_page"

-- | Large enough that the u-wrap alias search never displaces the
--   fixture chunk, so nothing here depends on seam behaviour.
worldSizeChunks ∷ Int
worldSizeChunks = 128

testFb ∷ (Int, Int)
testFb = (1920, 1080)

-- | The solid tiles, as chunk-local coords — the same three
--   'QuadSnapshot' uses, at three distinct screen offsets.
solidTiles ∷ [(Int, Int)]
solidTiles = [(0, 0), (15, 0), (15, 15)]

-- | Two stacked solid cells per solid column, so the terrain pass emits
--   exactly @2 * length solidTiles@ quads. Every other column is a
--   single air cell the quad loop skips on @mat ≡ 0@, and the terrain
--   surface map stays at 0 so the blank-tile fill never fires.
fixtureChunk ∷ LoadedChunk
fixtureChunk =
    let area = chunkSize * chunkSize
        solid = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.fromList [1, 1]
            , ctSlopes = VU.fromList [0, 0]
            , ctVeg    = VU.fromList [0, 0]
            }
        air = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.singleton 0
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
        columnAt idx =
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in if (lx, ly) `elem` solidTiles then solid else air
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.generate area columnAt
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

fixtureTiles ∷ WorldTileData
fixtureTiles = WorldTileData
    { wtdChunks    = HM.singleton (lcCoord fixtureChunk) fixtureChunk
    , wtdMaxChunks = 1
    }

genParams ∷ WorldGenParams
genParams = defaultWorldGenParams { wgpWorldSize = worldSizeChunks }

-- | Terrain cells one rebuild of this fixture visits: its single
--   visible chunk's whole column grid.
expectedTerrainCells ∷ Int
expectedTerrainCells = chunkSize * chunkSize

-- | Quads the terrain pass emits for this fixture: two stacked cells
--   per solid column.
expectedTerrainQuads ∷ Int
expectedTerrainQuads = 2 * length solidTiles

-- | Zoom 1.2 is exactly 'World.Grid.zoomFadeStart', so tiles are fully
--   opaque (@tileAlpha == 1@) and the zoom map is fully faded out
--   (@zoomAlpha == 0@) — the gameplay view, with the zoom pass sitting
--   behind its activation guard.
--
--   @camZTracking = False@ matters: with it on, 'updateWorldTiles'
--   rewrites @camZSlice@ from the fixture's surface map, and the
--   changed camera would then defeat the very quad-cache reuse the
--   cache-hit example exists to observe.
gameplayCamera ∷ Camera2D
gameplayCamera = defaultCamera
    { camPosition  = (0, 0)
    , camZoom      = 1.2
    , camZSlice    = 1
    , camFacing    = FaceSouth
    , camZTracking = False
    }

-- | Zoom 2.0 is past 'World.Grid.zoomFadeEnd': the zoom map is fully
--   opaque and every tile-space category is behind its
--   @tileAlpha ≤ 0.001@ guard. That inversion is what makes the
--   guard rule ("a bypassed category reports zero scanned and zero
--   emitted") observable on nine categories at once.
zoomedOutCamera ∷ Camera2D
zoomedOutCamera = gameplayCamera { camZoom = 2.0, camZTracking = False }

-- * Scene assembly

-- | Install the fixture page, visible and alone, with @camera@ live and
--   a cold quad cache. Returns the page's 'WorldState' so an example
--   can seed the per-page sources it is about to count.
resetScene ∷ EngineEnv → Camera2D → IO WorldState
resetScene env camera = do
    -- Tear the previous example's session down FIRST, through the
    -- production handler, so this one starts from the documented
    -- unavailable state rather than a leftover snapshot.
    clearTelemetry env
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) fixtureTiles
    writeIORef (wsGenParamsRef ws) (Just genParams)
    writeIORef (wsQuadCacheRef ws) Nothing
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(fixturePage, ws)]
        , wmVisible = [fixturePage] }
    writeIORef (unitManagerRef env) emptyUnitManager
    writeIORef (buildingManagerRef env) emptyBuildingManager
    writeIORef (buildingGhostRef env) Nothing
    writeIORef (rvCameraRef (toRenderViewCapability env)) camera
    writeIORef (rvFramebufferSizeRef (toRenderViewCapability env)) testFb
    writeIORef (rvWindowSizeRef (toRenderViewCapability env)) testFb
    pure ws

-- | Drop any previously published snapshot so an example's sequence
--   assertions start from the documented unavailable state. This is the
--   PRODUCTION teardown path, not a test-only setter.
clearTelemetry ∷ EngineEnv → IO ()
clearTelemetry env = do
    logger ← readIORef (loggerRef env)
    handleWorldDestroyAllCommand env logger

statsRef ∷ EngineEnv → IORef (Maybe SceneStats)
statsRef = rhSceneStatsRef . toRenderHandoffCapability

readStats ∷ EngineEnv → IO (Maybe SceneStats)
readStats = readIORef . statsRef

-- | One completed pass, answering with the snapshot it published.
--   Every example goes through the real entry point.
runPass ∷ EngineEnv → IO SceneStats
runPass env = do
    _ ← updateWorldTiles env
    mStats ← readStats env
    case mStats of
        Just stats → pure stats
        Nothing    → fail "updateWorldTiles published no scene stats"

rowFor ∷ SceneCategory → SceneStats → SceneCategoryStat
rowFor cat stats =
    case filter ((≡ cat) . scsCategory) (ssCategories stats) of
        (row : _) → row
        []        → error ("no telemetry row for " <> show cat)

scannedOf, emittedOf ∷ SceneCategory → SceneStats → Int
scannedOf cat = scsScanned . rowFor cat
emittedOf cat = scsEmitted . rowFor cat

-- * Source populations

mineDesignationsAt ∷ [(Int, Int)] → HM.HashMap (Int, Int) MineDesignation
mineDesignationsAt tiles = HM.fromList
    [ (t, MineDesignation { mdZ = 0
                          , mdCorners = (1, 1, 1, 1)
                          , mdChunkProgress = 0 })
    | t ← tiles ]

spoilPilesAt ∷ [(Int, Int)] → HM.HashMap (Int, Int) SpoilPile
spoilPilesAt verts = HM.fromList
    [ (v, SpoilPile { spMat = MaterialId 1, spFill = (1, 1, 1, 1) }) | v ← verts ]

groundItemsAt ∷ [(Float, Float)] → GroundItems
groundItemsAt positions = emptyGroundItems
    { gisNextId = length positions
    , gisItems = HM.fromList
        [ (i, GroundItem (fixtureItem (fromIntegral i)) x y)
        | (i, (x, y)) ← zip [0 ..] positions ]
    }

fixtureItem ∷ Word64 → ItemInstance
fixtureItem iid = ItemInstance
    { iiDefName     = "scene_stats_item"
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 1
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

-- | @n@ decals on the fixture page, added through the real store API so
--   the count this gate predicts is the count the store really holds.
storeWithDecals ∷ Int → BloodStore → BloodStore
storeWithDecals n store0 =
    store0 { bstDecals = foldl' step (bstDecals store0) [1 .. n] }
  where
    step decals i = fst (addDecal (decalSpec (fromIntegral i)) decals)

decalSpec ∷ Float → BloodDecalSpec
decalSpec offset = BloodDecalSpec
    { bspTexture        = BloodTextureId 1
    , bspPage           = fixturePage
    , bspX              = offset
    , bspY              = offset
    , bspSurfaceZ       = 0
    , bspOffsetX        = 0
    , bspOffsetY        = 0
    , bspRotation       = 0
    , bspScale          = 1
    , bspCreatedAt      = 0
    , bspInitialWetness = 1
    , bspWoundKind      = "stab"
    , bspSeverity       = SeverityModerate
    , bspSourceUnit     = Nothing
    , bspOpacity        = 1
    }

fixtureUnit ∷ WorldPageId → UnitInstance
fixtureUnit page = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 2, uiGridY = 2, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A 2x3 building def, so a single anchor-only construction
--   designation expands to a footprint of SIX marker candidates — a
--   number no other quantity in this fixture could be mistaken for.
footprintDef ∷ BuildingDef
footprintDef = BuildingDef
    { bdName = "scene_stats_building", bdDisplayName = "Scene Stats Hall"
    , bdCategory = "Storage", bdDescription = ""
    , bdTexture = TextureHandle 0, bdIconTexture = TextureHandle 0
    , bdTileW = 2, bdTileH = 3, bdPlacement = "flat_ground"
    , bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0
    , bdOperations = [], bdAnimations = HM.empty
    , bdStateAnims = HM.empty, bdPowerDrain = 0
    , bdPowerNode = Nothing
    }

fixtureBuilding ∷ WorldPageId → (Int, Int) → BuildingInstance
fixtureBuilding page (ax, ay) = BuildingInstance
    { biDefName = "scene_stats_building", biPage = page
    , biTexture = TextureHandle 0
    , biAnchorX = ax, biAnchorY = ay, biGridZ = 0, biSpawnedAt = 0
    , biTileW = 1, biTileH = 1, biSpawnRemaining = 0, biBuildProgress = 0
    , biMaterialsDelivered = HM.empty, biStorage = []
    }

-- | A one-unit baked zoom entry at @x@ on the world x axis.
--
--   'seedBakedZoom' clusters these tightly around the origin the
--   zoomed-out camera sits on, so 'World.Render.Zoom.Quads.makeMapQuads'
--   culls none of them and the emitted count is the seeded count. That
--   is deliberate: the example is about the telemetry, so its view
--   culling must not be what decides the number.
bakedEntryAt ∷ Float → BakedZoomEntry
bakedEntryAt x = BakedZoomEntry
    { bzeChunkX = 0, bzeChunkY = 0
    , bzeDrawX = x, bzeDrawY = 0
    , bzeWidth = 1, bzeHeight = 1
    , bzeSortKey = 0
    , bzeV0 = corner (Vec2 x 0)
    , bzeV1 = corner (Vec2 (x + 1) 0)
    , bzeV2 = corner (Vec2 (x + 1) 1)
    , bzeV3 = corner (Vec2 x 1)
    , bzeTexture = TextureHandle 0
    , bzeIsOcean = False, bzeHasLava = False, bzeElev = 0
    }
  where
    corner p = mkVertexWorld (packWorldUV 0 0) p (Vec2 0 0)
                             (Vec4 1 1 1 1) 0 0

-- | Seed the page's baked zoom cache directly. An EMPTY raw cache is
--   what makes this stick: 'World.Render.Zoom.Bake.ensureBakedAtlas'
--   only rebakes when the raw cache has entries, so it hands these
--   straight back.
seedBakedZoom ∷ WorldState → Int → IO ()
seedBakedZoom ws n = do
    (_, textures, facing) ← readIORef (wsBakedZoomRef ws)
    writeIORef (wsZoomCacheRef ws) V.empty
    writeIORef (wsBakedZoomRef ws)
        ( V.fromList (map (bakedEntryAt . fromIntegral) [0 .. n - 1])
        , textures, facing )

-- * Spec

spec ∷ Spec
spec = describe "scene assembly telemetry (#1921)" $ aroundAll setup $ do
    availabilitySpec
    shapeSpec
    sequenceSpec
    terrainCacheSpec
    scannedMeaningSpec
    cursorModeSpec
    structureScanSpec
    guardSpec
    zoomScanSpec
    teardownSpec
    luaQuerySpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadless
        act env

-- | Requirement 3's first half.
availabilitySpec ∷ SpecWith EngineEnv
availabilitySpec = describe "before the first completed pass" $

    it "publishes nothing at all" $ \env → do
        _ ← resetScene env gameplayCamera
        readStats env `shouldReturn` Nothing

-- | Requirement 2: the fixed order and the stable identifiers.
shapeSpec ∷ SpecWith EngineEnv
shapeSpec = describe "the published snapshot" $ do

    it "carries exactly one row per category, in the fixed order" $
        \env → do
            _ ← resetScene env gameplayCamera
            stats ← runPass env
            map scsCategory (ssCategories stats) `shouldBe` sceneCategoryOrder

    it "names the categories with the contract's identifiers" $ \env → do
        _ ← resetScene env gameplayCamera
        stats ← runPass env
        map (sceneCategoryId . scsCategory) (ssCategories stats) `shouldBe`
            [ "tiles", "cursor", "ground_items", "spoil", "blood"
            , "units", "buildings", "structures", "ghost", "zoom_map" ]

    it "reports a present, non-negative duration for every row" $ \env → do
        _ ← resetScene env gameplayCamera
        stats ← runPass env
        -- 'scsDurationNs' is a 'Word64', so this is total by
        -- construction; asserting it is what keeps the field from
        -- being quietly dropped or re-typed to something signed. A
        -- non-zero threshold is deliberately NOT asserted — that would
        -- be a scheduler-dependent claim.
        map (toInteger . scsDurationNs) (ssCategories stats)
            `shouldSatisfy` all (≥ 0)
        length (ssCategories stats) `shouldBe` 10

    it "reports non-negative counts for every row" $ \env → do
        _ ← resetScene env gameplayCamera
        stats ← runPass env
        map scsScanned (ssCategories stats) `shouldSatisfy` all (≥ 0)
        map scsEmitted (ssCategories stats) `shouldSatisfy` all (≥ 0)

-- | Requirement 4.
sequenceSpec ∷ SpecWith EngineEnv
sequenceSpec = describe "the publication sequence" $ do

    it "starts at 1 on the first completed pass" $ \env → do
        _ ← resetScene env gameplayCamera
        stats ← runPass env
        ssSequence stats `shouldBe` 1

    it "advances by exactly one per completed pass" $ \env → do
        _ ← resetScene env gameplayCamera
        first  ← runPass env
        second ← runPass env
        third  ← runPass env
        map ssSequence [first, second, third] `shouldBe` [1, 2, 3]

    it "replaces the whole snapshot rather than accumulating it" $
        \env → do
            ws ← resetScene env gameplayCamera
            writeIORef (wsSpoilRef ws) (spoilPilesAt [(0, 0), (1, 1)])
            first  ← runPass env
            second ← runPass env
            -- Two passes over an UNCHANGED fixture: the second reports
            -- the same per-pass values, not doubled ones.
            scannedOf ScSpoil second `shouldBe` scannedOf ScSpoil first
            emittedOf ScSpoil second `shouldBe` emittedOf ScSpoil first
            scannedOf ScSpoil second `shouldBe` 4

-- | Requirement 5's terrain row, and the cache-hit case the Coverage
--   section calls out by name.
terrainCacheSpec ∷ SpecWith EngineEnv
terrainCacheSpec = describe "the terrain category" $ do

    it "counts the cells a real rebuild visits" $ \env → do
        _ ← resetScene env gameplayCamera
        stats ← runPass env
        scannedOf ScTiles stats `shouldBe` expectedTerrainCells
        emittedOf ScTiles stats `shouldBe` expectedTerrainQuads

    it "reports zero scanned cells for a cache hit, keeping its\
       \ emitted count" $ \env → do
        _ ← resetScene env gameplayCamera
        rebuild ← runPass env
        reuse   ← runPass env
        scannedOf ScTiles rebuild `shouldBe` expectedTerrainCells
        scannedOf ScTiles reuse   `shouldBe` 0
        emittedOf ScTiles reuse   `shouldBe` expectedTerrainQuads
        -- And therefore: there is no generic `emitted <= scanned`
        -- invariant to assume anywhere in this snapshot.
        emittedOf ScTiles reuse `shouldSatisfy` (> scannedOf ScTiles reuse)

    it "rebuilds and re-counts after the cache is invalidated" $ \env → do
        ws ← resetScene env gameplayCamera
        _ ← runPass env
        reuse ← runPass env
        scannedOf ScTiles reuse `shouldBe` 0
        writeIORef (wsQuadCacheRef ws) Nothing
        again ← runPass env
        scannedOf ScTiles again `shouldBe` expectedTerrainCells
        emittedOf ScTiles again `shouldBe` expectedTerrainQuads

-- | Requirement 5's remaining rows, each against a deliberately created
--   source population.
scannedMeaningSpec ∷ SpecWith EngineEnv
scannedMeaningSpec = describe "the per-category scanned meanings" $ do

    it "counts cursor marker candidates only when the builder has a\
       \ texture to draw them with" $ \env → do
        ws ← resetScene env gameplayCamera
        writeIORef (wsMineDesignationsRef ws)
            (mineDesignationsAt [(0, 0), (1, 0), (2, 0)])
        withoutTexture ← runPass env
        -- No marker texture: the builder short-circuits and enumerates
        -- nothing, so the designations are not candidates at all.
        scannedOf ScCursor withoutTexture `shouldBe` 0
        cs ← readIORef (wsCursorRef ws)
        writeIORef (wsCursorRef ws)
            cs { mineDesignTexture = Just (TextureHandle 7) }
        withTexture ← runPass env
        scannedOf ScCursor withTexture `shouldBe` 3

    it "expands a construction designation to its def's whole\
       \ footprint" $ \env → do
        ws ← resetScene env gameplayCamera
        bm ← readIORef (buildingManagerRef env)
        writeIORef (buildingManagerRef env) bm
            { bmDefs = HM.singleton (bdName footprintDef) footprintDef }
        writeIORef (wsConstructDesignationsRef ws) $ HM.singleton (0, 0)
            (newConstructDesignation 0 (CtBuilding (bdName footprintDef)))
        cs ← readIORef (wsCursorRef ws)
        writeIORef (wsCursorRef ws)
            cs { constructBuildingTexture = Just (TextureHandle 9) }
        stats ← runPass env
        -- ONE anchor-only map entry (#807), six candidates: the
        -- designation's candidates are the FOOTPRINT tiles, not the
        -- map entries.
        scannedOf ScCursor stats
            `shouldBe` bdTileW footprintDef * bdTileH footprintDef

    it "counts every ground-item record on the page" $ \env → do
        ws ← resetScene env gameplayCamera
        writeIORef (wsGroundItemsRef ws)
            (groundItemsAt [(0, 0), (1, 1), (2, 2), (3, 3)])
        stats ← runPass env
        scannedOf ScGroundItems stats `shouldBe` 4

    it "counts each spoil pile once per level pass" $ \env → do
        ws ← resetScene env gameplayCamera
        writeIORef (wsSpoilRef ws) (spoilPilesAt [(0, 0), (1, 1), (2, 2)])
        stats ← runPass env
        -- Both level passes fold the whole pile map, irrespective of
        -- camera, z-band or visibility.
        scannedOf ScSpoil stats `shouldBe` 6

    it "counts every stored blood decal before any rejection" $ \env → do
        ws ← resetScene env gameplayCamera
        store ← readIORef (wsBloodStoreRef ws)
        let seeded = storeWithDecals 5 store
        writeIORef (wsBloodStoreRef ws) seeded
        HM.size (bdlDecals (bstDecals seeded)) `shouldBe` 5
        stats ← runPass env
        scannedOf ScBlood stats `shouldBe` 5
        -- No blood texture handles are registered headless, so every
        -- record is rejected at the texture lookup: scanned without
        -- emitted is exactly what the counter is for.
        emittedOf ScBlood stats `shouldBe` 0

    it "counts the whole global unit map, including units on no visible\
       \ page" $ \env → do
        _ ← resetScene env gameplayCamera
        um ← readIORef (unitManagerRef env)
        writeIORef (unitManagerRef env) um
            { umInstances = HM.fromList
                [ (UnitId 1, fixtureUnit fixturePage)
                , (UnitId 2, fixtureUnit fixturePage)
                , (UnitId 3, fixtureUnit (WorldPageId "somewhere_else")) ] }
        stats ← runPass env
        -- Three entries examined, though only two are on a visible
        -- page: the count is of the map this pass actually walks.
        scannedOf ScUnits stats `shouldBe` 3
        -- GPU-free: 'Unit.Render' returns no quads without a texture
        -- system, which is normal here and not a failure.
        emittedOf ScUnits stats `shouldBe` 0

    it "counts the whole global building map" $ \env → do
        _ ← resetScene env gameplayCamera
        bm ← readIORef (buildingManagerRef env)
        writeIORef (buildingManagerRef env) bm
            { bmInstances = HM.fromList
                [ (BuildingId 1, fixtureBuilding fixturePage (2, 2))
                , (BuildingId 2, fixtureBuilding fixturePage (3, 3)) ] }
        stats ← runPass env
        scannedOf ScBuildings stats `shouldBe` 2
        emittedOf ScBuildings stats `shouldBe` 0

    it "counts the ghost candidate before any rejection, and zero when\
       \ there is none" $ \env → do
        _ ← resetScene env gameplayCamera
        absent ← runPass env
        scannedOf ScGhost absent `shouldBe` 0
        writeIORef (buildingGhostRef env) $ Just BuildingGhost
            { bgDefName = "scene_stats_building"
            , bgGridX = 2, bgGridY = 2, bgGridZ = 0, bgValid = True }
        present ← runPass env
        -- The def is unknown to the manager, so the candidate is
        -- rejected — which is precisely the case a scanned count of one
        -- beside an emitted count of zero records.
        scannedOf ScGhost present `shouldBe` 1
        emittedOf ScGhost present `shouldBe` 0

    it "reports zero on BOTH counts for structures without a texture\
       \ system" $ \env → do
        _ ← resetScene env gameplayCamera
        stats ← runPass env
        -- 'Structure.Render' checks 'rvTextureSystemRef' BEFORE
        -- enumerating any piece, unlike units and buildings, so a
        -- GPU-free run legitimately scans nothing here.
        scannedOf ScStructures stats `shouldBe` 0
        emittedOf ScStructures stats `shouldBe` 0

-- | Requirement 5's last paragraph: a category bypassed by its
--   activation guard reports zero on both counts.
guardSpec ∷ SpecWith EngineEnv
guardSpec = describe "a category behind its activation guard" $ do

    it "reports zero for the zoom map at gameplay zoom" $ \env → do
        _ ← resetScene env gameplayCamera
        stats ← runPass env
        scannedOf ScZoomMap stats `shouldBe` 0
        emittedOf ScZoomMap stats `shouldBe` 0

    it "reports zero for every tile-space category once the map takes\
       \ over, while the zoom map counts its baked entries" $ \env → do
        ws ← resetScene env zoomedOutCamera
        seedBakedZoom ws 5
        writeIORef (wsSpoilRef ws) (spoilPilesAt [(0, 0), (1, 1)])
        writeIORef (wsGroundItemsRef ws) (groundItemsAt [(0, 0)])
        stats ← runPass env
        let tileSpace = [ ScTiles, ScCursor, ScGroundItems, ScSpoil
                        , ScBlood, ScUnits, ScBuildings, ScStructures
                        , ScGhost ]
        map (`scannedOf` stats) tileSpace `shouldSatisfy` all (≡ 0)
        map (`emittedOf` stats) tileSpace `shouldSatisfy` all (≡ 0)
        -- The zoom pass is the one that is active, and its candidates
        -- are the baked entries (this page carries no location
        -- instances and no zoom cursor).
        scannedOf ScZoomMap stats `shouldBe` 5
        emittedOf ScZoomMap stats `shouldBe` 5

-- | Requirement 3's second half, anchored to the two teardown handlers
--   that already clear the published quads.
teardownSpec ∷ SpecWith EngineEnv
teardownSpec = describe "world teardown" $ do

    it "clears the snapshot when the page is destroyed" $ \env → do
        _ ← resetScene env gameplayCamera
        _ ← runPass env
        readStats env `shouldSatisfy'` isJust
        logger ← readIORef (loggerRef env)
        handleWorldDestroyCommand env logger fixturePage
        readStats env `shouldReturn` Nothing

    it "clears the snapshot when every world is destroyed" $ \env → do
        _ ← resetScene env gameplayCamera
        _ ← runPass env
        readStats env `shouldSatisfy'` isJust
        logger ← readIORef (loggerRef env)
        handleWorldDestroyAllCommand env logger
        readStats env `shouldReturn` Nothing

    it "republishes at sequence 1 after a teardown" $ \env → do
        _ ← resetScene env gameplayCamera
        _ ← runPass env
        second ← runPass env
        ssSequence second `shouldBe` 2
        logger ← readIORef (loggerRef env)
        handleWorldDestroyAllCommand env logger
        -- The page is gone with the world, so re-install it before the
        -- next pass; the point is the SEQUENCE, which must not resume
        -- the destroyed lifecycle's count.
        _ ← resetScene env gameplayCamera
        afterTeardown ← runPass env
        ssSequence afterTeardown `shouldBe` 1

-- | 'shouldSatisfy' over an action's result.
shouldSatisfy' ∷ (HasCallStack, Show α) ⇒ IO α → (α → Bool) → Expectation
shouldSatisfy' act p = act ⌦ \a → a `shouldSatisfy` p


-- * The public Lua query

-- | Requirement 2 and requirement 3, through the verb a player-facing
--   caller actually has.
--
--   Everything above reads the published 'IORef' directly, which proves
--   what the world thread stores but not what @debug.getSceneStats()@
--   answers — and the unavailable state is a SYNTHESISED shape (ten
--   zero rows, not an absent table), so the query is the only place it
--   exists at all. This drives the real registration on a bare Lua
--   backend, no GPU and no Lua thread.
luaQuerySpec ∷ SpecWith EngineEnv
luaQuerySpec = describe "debug.getSceneStats()" $ do

    it "answers the complete zero-valued shape before the first pass" $
        \env → do
            _ ← resetScene env gameplayCamera
            ls ← bareLuaBackend env
            querySummary ls `shouldReturn` quoted unavailableSummary

    it "answers available, with the fixed order, after a pass" $ \env → do
        _ ← resetScene env gameplayCamera
        ls ← bareLuaBackend env
        _ ← runPass env
        queryShape ls `shouldReturn` quoted ("true|1|" <> idList)

    it "returns to the zero-valued shape after one page is destroyed" $
        \env → do
            _ ← resetScene env gameplayCamera
            ls ← bareLuaBackend env
            _ ← runPass env
            logger ← readIORef (loggerRef env)
            handleWorldDestroyCommand env logger fixturePage
            querySummary ls `shouldReturn` quoted unavailableSummary

    it "returns to the zero-valued shape after every world is destroyed" $
        \env → do
            _ ← resetScene env gameplayCamera
            ls ← bareLuaBackend env
            _ ← runPass env
            logger ← readIORef (loggerRef env)
            handleWorldDestroyAllCommand env logger
            querySummary ls `shouldReturn` quoted unavailableSummary

-- | The registered production Lua API on a bare backend: no GPU, no Lua
--   thread, and no scripts loaded — @registerDebugAPI@ is reached from
--   the single unconditional 'registerLuaAPI' path, so the verb is
--   present exactly as it is in a real boot.
bareLuaBackend ∷ EngineEnv → IO LuaBackendState
bareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | @available|sequence|id:scanned:emitted:durationNs,...@ — every field
--   of every row, flattened, so one comparison pins availability, the
--   sequence, the row order, the identifiers AND the zero values.
querySummary ∷ LuaBackendState → IO Text
querySummary ls = executeDebugLua (lbsLuaState ls) $ T.concat
    [ "local s = debug.getSceneStats(); local r = {}; "
    , "for i, c in ipairs(s.categories) do "
    , "  r[i] = c.id .. ':' .. c.scanned .. ':' .. c.emitted"
    , "         .. ':' .. c.durationNs end; "
    , "return tostring(s.available) .. '|' .. tostring(s.sequence)"
    , "       .. '|' .. table.concat(r, ',')" ]

-- | @available|sequence|id,...@ — the same, minus the per-row values,
--   for the available case where scanned/emitted/duration are the
--   subject of the examples above rather than of this one.
queryShape ∷ LuaBackendState → IO Text
queryShape ls = executeDebugLua (lbsLuaState ls) $ T.concat
    [ "local s = debug.getSceneStats(); local r = {}; "
    , "for i, c in ipairs(s.categories) do r[i] = c.id end; "
    , "return tostring(s.available) .. '|' .. tostring(s.sequence)"
    , "       .. '|' .. table.concat(r, ',')" ]

-- | The ten identifiers, in the published order.
idList ∷ Text
idList = T.intercalate "," (map sceneCategoryId sceneCategoryOrder)

-- | Requirement 3's exact answer: not available, sequence 0, and ten
--   complete zero-valued rows — never an empty or absent table.
unavailableSummary ∷ Text
unavailableSummary =
    "false|0|"
      <> T.intercalate ","
             [ sceneCategoryId cat <> ":0:0:0" | cat ← sceneCategoryOrder ]

-- | The debug console JSON-encodes its result, so a Lua string comes
--   back quoted.
quoted ∷ Text → Text
quoted t = "\"" <> t <> "\""


-- * The cursor category's mode discipline

-- | Requirement 5's @cursor@ row: the candidates counted are the ones
--   the ACTIVE tool mode's returned vector is built from — not every
--   candidate this @Strict@ module forces on the way there. Each
--   example pins the mode, the designation maps and the hover /
--   selection state rather than asserting an incidental total.
cursorModeSpec ∷ SpecWith EngineEnv
cursorModeSpec = describe "the cursor category's active tool mode" $ do

    it "counts the selection candidate in the mode that returns it" $
        \env → do
            ws ← resetScene env gameplayCamera
            writeIORef (wsToolModeRef ws) InfoTool
            cs ← readIORef (wsCursorRef ws)
            writeIORef (wsCursorRef ws)
                cs { worldSelectedTile = Just (0, 0, 0) }
            stats ← runPass env
            scannedOf ScCursor stats `shouldBe` 1

    it "does NOT count it in a mode whose returned vector omits it" $
        \env → do
            ws ← resetScene env gameplayCamera
            -- Identical state, different mode. 'MineTool' returns
            -- markers, hover and its own preview — never the selection
            -- quads — and this module is @Strict@, so the selection
            -- binding is forced regardless. Counting the forced set
            -- instead of the returned one would report 1 here.
            writeIORef (wsToolModeRef ws) MineTool
            cs ← readIORef (wsCursorRef ws)
            writeIORef (wsCursorRef ws)
                cs { worldSelectedTile = Just (0, 0, 0) }
            stats ← runPass env
            scannedOf ScCursor stats `shouldBe` 0

    it "counts the hover candidate the mode returns" $ \env → do
        ws ← resetScene env gameplayCamera
        writeIORef (wsToolModeRef ws) InfoTool
        _ ← hoverSomewhere env ws
        stats ← runPass env
        scannedOf ScCursor stats `shouldBe` 1

    it "counts the mine preview's whole anchored rectangle" $ \env → do
        ws ← resetScene env gameplayCamera
        writeIORef (wsToolModeRef ws) MineTool
        (hx, hy) ← hoverSomewhere env ws
        cs ← readIORef (wsCursorRef ws)
        -- A 3x3 rectangle: the anchor two tiles from the hover on both
        -- axes, stepped INWARD so both corners stay inside the one
        -- loaded chunk (the anchor's own surface z has to be readable
        -- for the builder to run at all). Every tile in it is a
        -- candidate the builder enumerates, before the per-tile
        -- surface-z and visibility filters — so the count is the AREA,
        -- not what survives them.
        let inward v = if v ≥ 2 then v - 2 else v + 2
        writeIORef (wsCursorRef ws)
            cs { mineAnchor         = Just (inward hx, inward hy)
               , worldCursorTexture = Just (TextureHandle 3) }
        stats ← runPass env
        -- One hover candidate plus the 3x3 preview.
        scannedOf ScCursor stats `shouldBe` 1 + 9

-- | Find a screen pixel this fixture's hit test actually resolves, park
--   the world cursor on it, and answer with the tile it picked.
--
--   The pixel is SEARCHED rather than assumed, because where three
--   solid tiles land on screen is a property of the projection, not
--   something an example should hard-code — and an example that
--   silently failed to pick would go on to compare zero against zero.
--   'fail' rather than a zero fallback is the point: no hover means the
--   fixture, not the counter, is broken.
hoverSomewhere ∷ EngineEnv → WorldState → IO (Int, Int)
hoverSomewhere env ws = go candidatePixels
  where
    go [] = fail "no screen pixel resolved a hover tile on this fixture"
    go (p : rest) = do
        cs ← readIORef (wsCursorRef ws)
        writeIORef (wsCursorRef ws) cs { worldCursorPos = Just p }
        _ ← updateWorldTiles env
        cs' ← readIORef (wsCursorRef ws)
        case worldHoverTile cs' of
            Just tile → pure tile
            Nothing   → go rest
    candidatePixels =
        [ (x, y)
        | y ← [0, 16 .. snd testFb - 1]
        , x ← [0, 16 .. fst testFb - 1] ]

-- * The structure category's own pure pass

-- | Requirement 5's @structures@ row, with pieces actually present.
--
--   A GPU-free 'updateWorldTiles' can never reach this: 'Structure.Render'
--   returns before enumerating anything without a texture system, which
--   is why the headless example above asserts zero on both counts. The
--   pure pass underneath it has no such gate, so this is where a piece
--   count with real pieces is provable at all.
structureScanSpec ∷ SpecWith EngineEnv
structureScanSpec = describe "the structure category's pure pass" $ do

    it "counts every piece of a chunk the visibility test admits" $ \_ → do
        let (scanned, quads) = structureScan inViewCamera
        scanned `shouldBe` length structureSlots
        quads `shouldSatisfy` (not . null)

    it "emits exactly what the untouched structureChunkQuads emits" $ \_ →
        map tshow (snd (structureScan inViewCamera))
            `shouldBe` map tshow (structureQuadsOnly inViewCamera)

    it "counts nothing for a chunk the visibility test culls" $ \_ → do
        let (scanned, quads) = structureScan outOfViewCamera
        scanned `shouldBe` 0
        quads `shouldSatisfy` null

-- | Four wall slots on one tile, so the piece count is a number no
--   other quantity in this fixture could be mistaken for.
structureSlots ∷ [StructureSlot]
structureSlots = [SWallNW, SWallNE, SWallSE, SWallSW]

structurePieces ∷ ChunkStructures
structurePieces = HM.fromList
    [ ((0, 0, fromIntegral (fromEnum slot)), StructurePieceData 0 0 0)
    | slot ← structureSlots ]

structureChunk ∷ LoadedChunk
structureChunk = fixtureChunk { lcStructures = structurePieces }

structurePalette ∷ TexPalette
structurePalette = snd (internPath "scene_stats/placeholder.png" emptyTexPalette)

-- | The camera the fixture chunk is in view of, and one far enough away
--   on the non-wrapping axis that no u-wrap alias brings it back.
inViewCamera, outOfViewCamera ∷ Camera2D
inViewCamera    = gameplayCamera { camZSlice = 0 }
outOfViewCamera = inViewCamera { camPosition = (0, 100000) }

structureScan ∷ Camera2D → (Int, [SortableQuad])
structureScan cam =
    structureChunkQuadsScanned emptyStructureWallCatalog structurePalette
        (HM.singleton 0 (TextureHandle 1)) (const 1) HM.empty
        (camFacing cam) (camZSlice cam) structureDepth 1.0 worldSizeChunks
        (boundsFor cam) (fst (camPosition cam)) (snd (camPosition cam))
        [structureChunk]

structureQuadsOnly ∷ Camera2D → [SortableQuad]
structureQuadsOnly cam =
    structureChunkQuads emptyStructureWallCatalog structurePalette
        (HM.singleton 0 (TextureHandle 1)) (const 1) HM.empty
        (camFacing cam) (camZSlice cam) structureDepth 1.0 worldSizeChunks
        (boundsFor cam) (fst (camPosition cam)) (snd (camPosition cam))
        [structureChunk]

structureDepth ∷ Int
structureDepth = 48

boundsFor ∷ Camera2D → ViewBounds
boundsFor cam = computeViewBounds cam (fst testFb) (snd testFb) structureDepth

-- * The zoom map's other two candidate sources

-- | Requirement 5's @zoom_map@ row names three sources, and the guard
--   example above exercises only the baked entries. This adds the other
--   two, each seeded to a distinct count so no two can be confused.
zoomScanSpec ∷ SpecWith EngineEnv
zoomScanSpec = describe "the zoom map's candidate sources" $ do

    it "adds the page's location instances to its baked entries" $
        \env → do
            ws ← resetScene env zoomedOutCamera
            seedBakedZoom ws 5
            writeIORef (wsGenParamsRef ws) $ Just genParams
                { wgpLocationInstances = locationInstances 3 }
            stats ← runPass env
            scannedOf ScZoomMap stats `shouldBe` 5 + 3

    it "adds a present zoom-cursor selection candidate" $ \env → do
        ws ← resetScene env zoomedOutCamera
        seedBakedZoom ws 5
        writeIORef (wsGenParamsRef ws) $ Just genParams
            { wgpLocationInstances = locationInstances 3 }
        cs ← readIORef (wsCursorRef ws)
        writeIORef (wsCursorRef ws) cs { zoomSelectedPos = Just (0, 0) }
        stats ← runPass env
        -- Counted from the cursor state, BEFORE the texture lookup that
        -- would reject it — this page has no zoom cursor texture, so
        -- the candidate emits nothing.
        scannedOf ScZoomMap stats `shouldBe` 5 + 3 + 1

    it "adds a resolvable zoom-cursor HOVER candidate" $ \env → do
        ws ← resetScene env zoomedOutCamera
        seedBakedZoom ws 5
        writeIORef (wsGenParamsRef ws) $ Just genParams
            { wgpLocationInstances = locationInstances 3 }
        cs ← readIORef (wsCursorRef ws)
        writeIORef (wsCursorRef ws)
            cs { zoomCursorPos = Just zoomHoverPixel }
        stats ← runPass env
        -- Hover and selection reach the count through INDEPENDENT
        -- paths, so the selection example above proves nothing about
        -- this one.
        scannedOf ScZoomMap stats `shouldBe` 5 + 3 + 1

    it "adds both cursor candidates when hover and selection coexist" $
        \env → do
            ws ← resetScene env zoomedOutCamera
            seedBakedZoom ws 5
            writeIORef (wsGenParamsRef ws) $ Just genParams
                { wgpLocationInstances = locationInstances 3 }
            cs ← readIORef (wsCursorRef ws)
            writeIORef (wsCursorRef ws)
                cs { zoomCursorPos   = Just zoomHoverPixel
                   , zoomSelectedPos = Just (0, 0) }
            stats ← runPass env
            scannedOf ScZoomMap stats `shouldBe` 5 + 3 + 2

-- | A screen pixel the zoom map's own unprojection resolves to a chunk.
--
--   Located through 'pixelToChunkOrigin' — the very function
--   'makeCursorQuadScanned' asks — rather than guessed, because which
--   pixels land on the map is a property of the projection and the
--   camera, not something an example should hard-code. Using the
--   engine's own oracle to choose an INPUT is not circular: what the
--   examples assert is the counter's output.
zoomHoverPixel ∷ (Int, Int)
zoomHoverPixel =
    case [ p
         | p@(x, y) ← [ (x, y) | y ← [0, 16 .. snd testFb - 1]
                               , x ← [0, 16 .. fst testFb - 1] ]
         , isJust (pixelToChunkOrigin (camFacing zoomedOutCamera)
                       zoomedOutCamera (fst testFb) (snd testFb)
                       (fst testFb) (snd testFb) worldSizeChunks x y) ] of
        (p : _) → p
        []      → error "no screen pixel resolves to a zoom-map chunk"

-- | @n@ location instances on the page, allocated through the real
--   allocator so their stored geometry is what placement would produce.
locationInstances ∷ Int → LocationInstances
locationInstances n = foldl' step emptyLocationInstances [1 .. n]
  where
    step lis i =
        case allocateLocationInstance Nothing (ChunkCoord i 0) locationDef lis of
            Right (_, lis') → lis'
            Left err        → error ("location fixture: " <> show err)

locationDef ∷ LocationDef
locationDef = LocationDef
    { ldId         = "scene_stats_ruin"
    , ldLabel      = "Scene Stats Ruin"
    , ldType       = "ruin"
    , ldBuilder    = "room_small"
    , ldAnchor     = []
    , ldMaxCount   = 0
    , ldMinSpacing = 0
    , ldContents   = []
    , ldBounds     = RelBounds (-1) (-1) 1 1
    , ldMapIcon    = Nothing
    , ldNaming     = LocationNaming
        { lnHeads = [ConceptId "KEEP"], lnModifiers = [ConceptId "ASH"] }
    }
