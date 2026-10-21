{-# LANGUAGE Strict #-}
-- | The LIVE half of the world-content tests (#2711): the commit
--   ('Scenario.World.Install') onto a real arena page through the real
--   world thread, read back from the page's own owners, then through the
--   ordinary save codec and the real staging, and captured again.
module Test.Headless.Scenario.World.Live (spec) where

import Unit.Faction.RelationStore (emptyRelationCauseStore)
import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTimeNSec)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Core.Log (LogConfig(..), LogBackend(..), initLogger, defaultLogConfig)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import World.Grid (gridToWorld)
import World.Flora.Growth (floraGrowth)
import World.Render.FloraDraws (findTopSolid)
import Item.Knowledge (emptyPortableKnowledge)
import Sim.Chunk (applyChunkEdit)
import Sim.Command.Types (SimCommand(..))
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Fluid.Types (ActiveFluidCell(..))
import Sim.State.Types (SimChunkState(..), SimWorldState(..), emptySimWorldState)
import Sim.Topology (simTopologyForParams)
import Structure.Palette (emptyTexPalette)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import World.Chunk.Queue (enqueueChunkRequest)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Snapshot (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types (nameFloraReferences)
import qualified Data.HashSet as HS
import Gameplay.Tags.Index (categoryForward, categoryTagsOf)
import Gameplay.Tags.Page
    ( CropPlotStore(..), PageTags(..), PageTagsSnapshot(..), capturePageTags )
import Gameplay.Tags.Types (plotTargetFromStored, tileTargetFromStored, mkPlantTarget)
import Scenario.Types
import Scenario.World.Build (editTile)
import Scenario.World.Capture
import Scenario.World.Install
import Scenario.World.Types
import Test.Headless.Harness (sendWorldCommand, waitForChunksAt, waitForWorldInit)
import Test.Headless.Scenario.World.Fixtures
import World.Arena.Bounds (ArenaBounds)
import World.Chunk.Admit (pageIncarnation, releaseEvictedChunks)
import World.Edit.Apply (replayEdits)
import World.Generate.Arena (clipArenaChunk, generateArenaChunkIn)
import Gameplay.Tags.PageLive (reconcileAdmittedPageTags)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), columnIndex)
import qualified Data.Vector.Unboxed as VU
import Data.List (unfoldr)
import System.Random (mkStdGen, randomR)
import Scenario.World.Terrain (baseTopZ)
import World.Command.Types (WorldCommand(..))
import World.Chop.Types (newChopDesignation)
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Flora.Designation (setChopDesignations)
import World.Flora.Types
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate.Coordinates (chunkToGlobal, globalToChunk)
import World.Material (matGranite, matLoam, matSand)
import World.Material.Id (MaterialId(..))
import World.Page.Types (WorldPageId(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldState)
import World.Tile.Types (WorldTileData(..), evictDistantChunksWithReport, insertChunk, lookupChunk)

-- * Page fixtures

uniquePage ∷ T.Text → IO WorldPageId
uniquePage t = do
    n ← getMonotonicTimeNSec
    pure (WorldPageId (t <> "_" <> T.pack (show n)))

-- | A real finite arena page, created through the world thread's own
--   init command, with this build's flora fixtures installed.
freshArena ∷ EngineEnv → T.Text → Int → Int → IO (WorldPageId, WorldState)
freshArena env t w h = do
    pid ← uniquePage t
    writeIORef (floraCatalogRef env) catalog
    -- the shipped materials by name, as a loaded session registers them
    logger ← readIORef (loggerRef env)
    loadPopulatedMaterialRegistry logger "data/materials" ≫= writeIORef (materialRegistryRef env)
    sendWorldCommand env (WorldInitArena pid (Just (bounds w h)))
    ws ← waitForWorldInit env pid 60
    pure (pid, ws)

-- | Commit a request through the world thread and demand it succeeded.
install ∷ EngineEnv → WorldPageId → WorldState → ScenarioWorldRequest → IO WorldInstallResult
install env pid ws request = do
    epoch ← pageIncarnation ws
    outcome ← requestWorldInstall (toWorldSimCapability env) pid epoch request
    case outcome of
        WorldInstalled r → pure r
        WorldInstallNotAdmitted →
            expectationFailure "the page did not admit the install" ≫ error "unreachable"
        WorldInstallNotArena →
            expectationFailure "the page is not an arena" ≫ error "unreachable"

request ∷ [TerrainPatch] → [FluidPatch] → [FloraEntry] → ScenarioWorldRequest
request ts fs ls = ScenarioWorldRequest (withPaths "terrain" ts) (withPaths "fluids" fs)
                                        (withPaths "flora" ls)

chunkAtTile ∷ WorldState → (Int, Int) → IO (Maybe LoadedChunk)
chunkAtTile ws (x, y) = lookupChunk (fst (globalToChunk x y)) ⊚ readIORef (wsTilesRef ws)

-- | The (top z, top material, slope, veg) of a resident column.
liveColumn ∷ WorldState → (Int, Int) → IO (Maybe (Int, Maybe Word8, Word8, Word8))
liveColumn ws t = fmap (`columnAt` t) ⊚ chunkAtTile ws t

liveFluid ∷ WorldState → (Int, Int) → IO (Maybe FluidCell)
liveFluid ws t@(x, y) = do
    mc ← chunkAtTile ws t
    let (_, (lx, ly)) = globalToChunk x y
    pure (mc ⌦ \lc → lcFluidMap lc V.! columnIndex lx ly)

liveOccurrences ∷ WorldState → IO [FloraInstance]
liveOccurrences ws = do
    td ← readIORef (wsTilesRef ws)
    pure [ fi | lc ← HM.elems (wtdChunks td), fi ← fcdInstances (lcFlora lc) ]

-- | The snapshot a coherent capture reads off a live page.
liveSnapshot ∷ EngineEnv → WorldState → Maybe ArenaBounds → Int → IO WorldSnapshot
liveSnapshot env ws b day = do
    td ← readIORef (wsTilesRef ws)
    edits ← readIORef (wsEditsRef ws)
    registry' ← readIORef (materialRegistryRef env)
    plots ← readIORef (wsCropPlotsRef ws)
    harvests ← readIORef (wsFloraHarvestsRef ws)
    tags ← readIORef (wsPageTagsRef ws)
    let ts = capturePageTags tags plots
    pure WorldSnapshot
        { snapBounds = b, snapSeed = 0, snapEdits = edits
        , snapResident = HM.elems (wtdChunks td)
        , snapMaterials = registry', snapFlora = catalog, snapAbsDay = day
        , snapCropPlots = cpsPlots plots, snapHarvests = harvests
        , snapTileTags = ptsTiles ts, snapOccTags = ptsOccurrences ts
        , snapPlotTags = ptsPlots ts }

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "on a live arena page (through the world thread)" $ do

    let pad = terrainPatch "pad" (RegionRect 0 0 2 1) "granite" 3 (Authored 0) [tag "pad"]
        pit = terrainPatch "pit" (RegionTiles [(5, 5)]) "sand" (-2) Omitted []
        pond = fluidPatch "pond" (RegionRect 5 5 7 5) FluidLake 35
        oak = (floraEntry "oak" "oak" 1 1)
                { feTags = [tag "grove"], feAge = Authored 120, feHealth = Authored 0.5
                , feOffsetU = Authored (-0.25), feVariant = Authored 2, feRegrowth = Authored 90 }
        beet = (floraEntry "beet" "beet" 8 8) { feRegrowth = Authored 0 }
        wheat = (floraEntry "wheat" "wheat" 9 9) { feAge = Authored 6, feTags = [tag "field"] }

    it "commits terrain, fluid, flora, plots and tags, and rebuilds the resident chunks" $ \env → do
        (pid, ws) ← freshArena env "world_commit" 40 40
        r ← install env pid ws (request [pad, pit] [pond] [oak, beet, wheat])
        wirDiagnostics r `shouldBe` []
        -- terrain and fluid, read from the page's own tile store
        liveColumn ws (0, 0) `shouldReturn` Just (3, Just (unMaterialId matGranite), 0, 0)
        liveColumn ws (5, 5) `shouldReturn` Just (-2, Just (unMaterialId matSand), 0, 0)
        liveFluid ws (5, 5) `shouldReturn` Just (FluidCell Lake 35)
        liveFluid ws (6, 5) `shouldReturn` Just (FluidCell Lake 35)
        liveFluid ws (0, 0) `shouldReturn` Nothing
        -- flora
        os ← liveOccurrences ws
        length os `shouldBe` 2
        let oakI = sole [ o | o ← os, fiSpecies o ≡ oakId ]
        (fiOffU oakI, fiVariant oakI, fiHealth oakI, fiZ oakI) `shouldBe` (-0.25, 2, 0.5, 3)
        plots ← readIORef (wsCropPlotsRef ws)
        map fst (HM.toList (cpsPlots plots)) `shouldBe` [(9, 9)]
        -- regrowth timer, planted cursor, tags
        harvests ← readIORef (wsFloraHarvestsRef ws)
        HM.toList harvests `shouldBe` [(fiInstanceId oakI, 90)]
        cursor ← readIORef (wsPlantedFloraCursorRef ws)
        cursor `shouldBe` 3
        tags ← readIORef (wsPageTagsRef ws)
        categoryTagsOf (tileTargetFromStored pid 0 0 3) (ptTiles tags)
            `shouldBe` Set.fromList [tag "pad"]
        fmap (\t → categoryTagsOf t (ptOccurrences tags)) (mkPlantTarget pid (fiInstanceId oakI))
            `shouldBe` Just (Set.fromList [tag "grove"])
        M.lookup (fiInstanceId oakI) (ptOccurrenceTiles tags) `shouldBe` Just (1, 1)
        categoryTagsOf (plotTargetFromStored pid 9 9) (cpsTags plots)
            `shouldBe` Set.fromList [tag "field"]
        -- identities for every accepted entry
        map fst (wrOccurrences (wirRefs r)) `shouldMatchList` [ex "oak", ex "beet"]
        map fst (wrPlots (wirRefs r)) `shouldBe` [ex "wheat"]

    it "REPLACES the owned baseline: no accumulation, structure entries kept in order, removed plants' state forgotten" $ \env → do
        (pid, ws) ← freshArena env "world_replace" 40 40
        _ ← install env pid ws (request [pad, pit] [pond] [oak, beet, wheat])
        let s1 = WeSetStructure 1 1 0 7 7 2
            s2 = WeClearStructure 2 2 0
        atomicModifyIORef' (wsEditsRef ws) $ \es →
            (appendEdit (ChunkCoord 0 0) s2 (appendEdit (ChunkCoord 0 0) s1 es), ())
        -- pending player work on the old oak: a chop designation
        oakIid ← fiInstanceId ∘ sole ∘ filter ((≡ oakId) ∘ fiSpecies) ⊚ liveOccurrences ws
        setChopDesignations ws [(oakIid, (1, 1), Just (newChopDesignation 3 1 1))]
        r2 ← install env pid ws
                (request [pit] [] [(floraEntry "fern" "fern" 4 4) { feTags = [tag "ferns"] }])
        wirDiagnostics r2 `shouldBe` []
        -- the pad is gone and the column is the base again, grass and all
        (top, mat, slope, veg) ← fromMaybe (error "resident") ⊚ liveColumn ws (0, 0)
        (top, mat, slope) `shouldBe` (0, Just (unMaterialId matLoam), 0)
        veg `shouldSatisfy` (> 0)
        liveFluid ws (6, 5) `shouldReturn` Nothing
        liveFluid ws (5, 5) `shouldReturn` Nothing
        liveColumn ws (5, 5) `shouldReturn` Just (-2, Just (unMaterialId matSand), 0, 0)
        -- one plant, no plot, no timer, no designation, no stale tag
        os ← liveOccurrences ws
        map fiSpecies os `shouldBe` [FloraId 2]
        map fiChopDesignated os `shouldBe` [False]
        plots ← readIORef (wsCropPlotsRef ws)
        HM.null (cpsPlots plots) `shouldBe` True
        readIORef (wsFloraHarvestsRef ws) ≫= (`shouldBe` HM.empty)
        readIORef (wsChopDesignationsRef ws) ≫= (`shouldBe` HM.empty)
        tags ← readIORef (wsPageTagsRef ws)
        categoryTagsOf (tileTargetFromStored pid 0 0 3) (ptTiles tags) `shouldBe` Set.empty
        categoryForward (ptOccurrences tags) `shouldSatisfy` (\m → length m ≡ 1)
        categoryTagsOf (plotTargetFromStored pid 9 9) (cpsTags plots) `shouldBe` Set.empty
        -- the log: structure entries intact and in order, no old terrain edit
        edits ← readIORef (wsEditsRef ws)
        filter isStructureEdit (HM.lookupDefault [] (ChunkCoord 0 0) edits) `shouldBe` [s1, s2]
        [ () | es ← HM.elems edits, WeAddTile{} ← es ] `shouldBe` []

    it "refuses a page that is not an arena, changing nothing" $ \env → do
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws) (Just defaultWorldGenParams { wgpSeed = 7 })
        pid ← uniquePage "world_notarena"
        atomicModifyIORef' (worldManagerRef env) $ \m → (m { wmWorlds = (pid, ws) : wmWorlds m }, ())
        epoch ← pageIncarnation ws
        outcome ← requestWorldInstall (toWorldSimCapability env) pid epoch (request [pad] [] [oak])
        outcome `shouldBe` WorldInstallNotArena
        readIORef (wsEditsRef ws) ≫= (`shouldBe` HM.empty)
        readIORef (wsPlantedFloraCursorRef ws) ≫= (`shouldBe` 1)

    it "forgets the state of a replaced plant that is not resident: its timer and tags" $ \env → do
        (pid, ws) ← freshArena env "world_oldfar" 120 120
        let farOak = (floraEntry "farOak" "oak" 50 51) { feTags = [tag "farTree"], feRegrowth = Authored 30 }
        _ ← install env pid ws (request [] [] [farOak])
        HM.size ⊚ readIORef (wsFloraHarvestsRef ws) ≫= (`shouldBe` 1)
        length ∘ categoryForward ∘ ptOccurrences ⊚ readIORef (wsPageTagsRef ws) ≫= (`shouldBe` 1)
        -- replaced by nothing: the plant's chunk was never resident, yet everything keyed to it goes
        _ ← install env pid ws (request [] [] [])
        readIORef (wsFloraHarvestsRef ws) ≫= (`shouldBe` HM.empty)
        length ∘ categoryForward ∘ ptOccurrences ⊚ readIORef (wsPageTagsRef ws) ≫= (`shouldBe` 0)

    it "accumulates nothing when the same content is materialized again" $ \env → do
        (pid, ws) ← freshArena env "world_twice" 40 40
        let req = request [pad, pit] [pond] [oak, beet, wheat]
        _ ← install env pid ws req
        snap1 ← liveSnapshot env ws (Just (bounds 40 40)) 0
        let cw1 = captureWorldContent Set.empty snap1
        _ ← install env pid ws req
        _ ← install env pid ws req
        snap3 ← liveSnapshot env ws (Just (bounds 40 40)) 0
        captureWorldContent Set.empty snap3 `shouldBe` cw1
        length ⊚ liveOccurrences ws ≫= (`shouldBe` 2)
        plots ← readIORef (wsCropPlotsRef ws)
        HM.size (cpsPlots plots) `shouldBe` 1
        HM.size ⊚ readIORef (wsFloraHarvestsRef ws) ≫= (`shouldBe` 1)
        tags ← readIORef (wsPageTagsRef ws)
        length (categoryForward (ptTiles tags)) `shouldBe` 6     -- the pad's six cells
        length (categoryForward (ptOccurrences tags)) `shouldBe` 1

    it "recaptures what it materialized (capture → materialize → capture) on the live page" $ \env → do
        (pid, ws) ← freshArena env "world_recap" 40 40
        _ ← install env pid ws (request [pad, pit] [pond] [oak, beet, wheat])
        snap ← liveSnapshot env ws (Just (bounds 40 40)) 0
        let cw = captureWorldContent Set.empty snap
        (pid2, ws2) ← freshArena env "world_recap2" 40 40
        _ ← install env pid2 ws2 (ScenarioWorldRequest
                (withPaths "terrain" (cwTerrain cw)) (withPaths "fluids" (cwFluids cw))
                (withPaths "flora" (cwFlora cw)))
        snap2 ← liveSnapshot env ws2 (Just (bounds 40 40)) 0
        captureWorldContent Set.empty snap2 `shouldBe` cw
        (pid ≡ pid2) `shouldBe` False

    it "admits authored fluid to the simulation exactly, and keeps a finite edge closed" $ \env → do
        (pid, ws) ← freshArena env "world_sim" 40 40
        let wsc = toWorldSimCapability env
        _ ← Q.flushQueue (wsSimQueue wsc)
        _ ← install env pid ws
                (request [] [fluidPatch "edge" (RegionRect 18 0 19 0) FluidLake 35] [])
        msgs ← Q.flushQueue (wsSimQueue wsc)
        td ← readIORef (wsTilesRef ws)
        let edited = [ (c, g, fm, tm) | SimChunkEdited p _ _ c g fm tm ← msgs, p ≡ pid ]
        -- every resident chunk is re-seeded (replacement reaches chunks the log never named)
        sort (map (\(c, _, _, _) → c) edited) `shouldBe` sort (HM.keys (wtdChunks td))
        -- the passive chunk and the sim's re-seed are the same exact map
        forM_ edited $ \(c, _, fm, tm) → do
            let lc = fromMaybe (error "resident") (lookupChunk c td)
            fm `shouldBe` lcFluidMap lc
            tm `shouldBe` lcTerrainSurfaceMap lc
        params ← fromMaybe (error "params") ⊚ readIORef (wsGenParamsRef ws)
        let sws0 = foldl' (\w (c, g, fm, tm) → applyChunkEdit c g fm tm w)
                       emptySimWorldState { swsTopology = simTopologyForParams params, swsActive = True }
                       edited
            volume w = sum [ fromIntegral (afcVolume c)
                           | scs ← HM.elems (swsChunks w), Just c ← V.toList (scsActiveFluid scs) ] ∷ Int
            sws1 = iterate simulateActiveTick sws0 !! 200
            outside w = [ (c, i) | (c, scs) ← HM.toList (swsChunks w)
                                 , (i, Just _) ← zip [0 ∷ Int ..] (V.toList (scsActiveFluid scs))
                                 , let (gx, gy) = chunkToGlobal c (i `mod` 16) (i `div` 16)
                                 , gx > 19 ∨ gy > 20 ∨ gx < -20 ∨ gy < -19 ]
        -- two tiles at exactly 35 units each over a terrain top of 0
        volume sws0 `shouldBe` 70
        -- nothing was advanced by the commit; a long run conserves it all and
        -- never lets a unit past the rectangle
        volume sws1 `shouldBe` 70
        outside sws1 `shouldBe` []

    it "keeps content for a chunk that is not resident and rebuilds it on admission" $ \env → do
        (pid, ws) ← freshArena env "world_nonres" 120 120
        let b = Just (bounds 120 120)
            far = terrainPatch "far" (RegionTiles [(50, 50)]) "granite" 2 Omitted [tag "far"]
            farOak = (floraEntry "farOak" "oak" 50 51) { feTags = [tag "farTree"] }
        isNothing ⊚ lookupChunk (ChunkCoord 3 3) ⊚ readIORef (wsTilesRef ws) ≫= (`shouldBe` True)
        r ← install env pid ws
                (request [far] [fluidPatch "farPond" (RegionTiles [(51, 50)]) FluidLake 20] [farOak])
        wirDiagnostics r `shouldBe` []
        wirChunks r `shouldSatisfy` (ChunkCoord 3 3 `notElem`)   -- the chunk that is not resident was not rebuilt
        edits ← readIORef (wsEditsRef ws)
        HM.member (ChunkCoord 3 3) edits `shouldBe` True
        tags ← readIORef (wsPageTagsRef ws)
        categoryTagsOf (tileTargetFromStored pid 50 50 2) (ptTiles tags) `shouldBe` Set.fromList [tag "far"]
        length (categoryForward (ptOccurrences tags)) `shouldBe` 1
        -- non-residency is not deletion: a capture sees all of it from the durable log
        before ← captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0
        map tpSurfaceZ (cwTerrain before) `shouldBe` [2]
        map tpTags (cwTerrain before) `shouldBe` [[tag "far"]]
        map feSpecies (cwFlora before) `shouldBe` ["oak"]
        map fpSurfaceEighths (cwFluids before) `shouldBe` [20]
        -- demand the chunk through the real queue and let the world thread admit it
        ws' ← pure ws
        n ← enqueueChunkRequest pid ws' [ChunkCoord 3 3]
        n `shouldBe` 1
        waitForChunksAt ws' (ChunkCoord 3 3) 30 `shouldReturn` True
        liveColumn ws (50, 50) `shouldReturn` Just (2, Just (unMaterialId matGranite), 0, 0)
        liveFluid ws (51, 50) `shouldReturn` Just (FluidCell Lake 20)
        map fiSpecies ⊚ liveOccurrences ws ≫= (`shouldBe` [oakId])
        -- admission's own reconciliation keeps every membership its chunk really holds
        tags' ← readIORef (wsPageTagsRef ws)
        categoryTagsOf (tileTargetFromStored pid 50 50 2) (ptTiles tags') `shouldBe` Set.fromList [tag "far"]
        length (categoryForward (ptOccurrences tags')) `shouldBe` 1
        after ← captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0
        after `shouldBe` before

    it "survives an ordinary save: encode, decode, stage and capture again, with no source file" $ \env → do
        (pid, ws) ← freshArena env "world_save" 120 120
        let b = Just (bounds 120 120)
            far = terrainPatch "far" (RegionTiles [(50, 50)]) "granite" 2 Omitted [tag "far"]
        _ ← install env pid ws
                (request [pad, pit, far] [pond] [oak, beet, wheat, (floraEntry "farOak" "oak" 50 51) { feRegrowth = Authored 30 }])
        live ← captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0
        -- the page as a save writes it: species NAMED, tags forward-only
        params ← fromMaybe (error "params") ⊚ readIORef (wsGenParamsRef ws)
        edits ← readIORef (wsEditsRef ws)
        plots ← readIORef (wsCropPlotsRef ws)
        harvests ← readIORef (wsFloraHarvestsRef ws)
        tags ← readIORef (wsPageTagsRef ws)
        cursor ← readIORef (wsPlantedFloraCursorRef ws)
        (namedEdits, namedCrops, _, _) ← either (const (error "unnamed species")) pure
            (nameFloraReferences catalog pid edits (cpsPlots plots) HM.empty HM.empty)
        let page = (blankPageSnapshot pid params)
                { pgsEdits = namedEdits, pgsCropPlots = namedCrops, pgsFloraHarvests = harvests
                , pgsPlantedFloraCursor = cursor, pgsPageTags = capturePageTags tags plots
                , pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid) }
            snap = SessionSnapshot
                { snapGameTime = 0, snapTexPalette = emptyTexPalette
                , snapNextItemId = 1, snapNextBuildingId = 1, snapNextUnitId = 1
                , snapActivePage = pid, snapVisiblePages = [pid]
                , snapLiveCamera = LiveCameraSnapshot (Just pid) 0 0 1 FaceSouth
                , snapPages = HM.singleton pid page
                , snapItemTags = mempty, snapItemRefs = mempty
                , snapPortableKnowledge = emptyPortableKnowledge
                , snapRelationCauses = emptyRelationCauseStore }
            meta = SaveRequestMeta "scenario_world" "2026-10-19T00:00:00.000000Z" False
            bytes = encodeSessionSnapshot (snapshotSaveMetadata meta snap) snap []
        back ← case decodeSessionEnvelope HS.empty HS.empty bytes of
            Left err → expectationFailure (T.unpack err) ≫ error "unreachable"
            Right (_, s, _, _) → pure s
        logger ← initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }
        matReg ← readIORef (materialRegistryRef env)
        staged ← stageSession env logger (snapshotToSaveData meta back) matReg ≫= \r → case r of
            Left e → expectationFailure (T.unpack (renderStageError e)) ≫ error "unreachable"
            Right ok → pure ok
        sp ← case filter ((≡ pid) ∘ spPageId) (ssPages staged) of
            (x : _) → pure x
            []      → expectationFailure "the staged page is missing" ≫ error "unreachable"
        -- the staged page holds the same durable state...
        let sws = spWorldState sp
        cursor' ← readIORef (wsPlantedFloraCursorRef sws)
        cursor' `shouldBe` cursor
        readIORef (wsFloraHarvestsRef sws) ≫= (`shouldBe` harvests)
        -- ...and a scenario capture of it is the live page's, including the
        -- content of the chunk that was never resident
        stagedCapture ← captureWorldContent Set.empty ⊚ liveSnapshot env sws b 0
        stagedCapture `shouldBe` live
        map (\p → tpSurfaceZ p) (cwTerrain live) `shouldSatisfy` (elem 2)

    it "follows #2705's lifetime rules: a surviving cell keeps its tags through a material change, a removed cell loses them, a replacement plant inherits none" $ \env → do
        (pid, ws) ← freshArena env "world_life" 40 40
        _ ← install env pid ws (request [pad] [] [oak])
        let tagsOf () = readIORef (wsPageTagsRef ws)
        t0 ← tagsOf ()
        oakIid0 ← fiInstanceId ∘ sole ⊚ liveOccurrences ws
        categoryTagsOf (tileTargetFromStored pid 0 0 3) (ptTiles t0) `shouldBe` Set.fromList [tag "pad"]
        -- the SAME cells in another material, untagged: they still exist, so they keep their tags
        _ ← install env pid ws (request [terrainPatch "pad" (RegionRect 0 0 2 1) "sand" 3 (Authored 0) []] [] [oak { feTags = [] }])
        t1 ← tagsOf ()
        categoryTagsOf (tileTargetFromStored pid 0 0 3) (ptTiles t1) `shouldBe` Set.fromList [tag "pad"]
        liveColumn ws (0, 0) `shouldReturn` Just (3, Just (unMaterialId matSand), 0, 0)
        -- the oak was REPLACED: a new identity, and nothing inherited
        oakIid1 ← fiInstanceId ∘ sole ⊚ liveOccurrences ws
        (oakIid1 ≡ oakIid0) `shouldBe` False
        fmap (\t → categoryTagsOf t (ptOccurrences t1)) (mkPlantTarget pid oakIid1) `shouldBe` Just Set.empty
        fmap (\t → categoryTagsOf t (ptOccurrences t1)) (mkPlantTarget pid oakIid0) `shouldBe` Just Set.empty
        -- the column is LOWERED below the tagged cell: that cell is gone, and its tags with it
        _ ← install env pid ws (request [terrainPatch "pad" (RegionRect 0 0 2 1) "sand" 1 (Authored 0) []] [] [])
        t2 ← tagsOf ()
        categoryTagsOf (tileTargetFromStored pid 0 0 3) (ptTiles t2) `shouldBe` Set.empty
        -- a cell created at that coordinate again inherits nothing
        _ ← install env pid ws (request [terrainPatch "pad" (RegionRect 0 0 2 1) "sand" 3 (Authored 0) []] [] [])
        t3 ← tagsOf ()
        categoryTagsOf (tileTargetFromStored pid 0 0 3) (ptTiles t3) `shouldBe` Set.empty

    it "keeps tags through eviction and readmission, and a capture reads the same either way" $ \env → do
        (pid, ws) ← freshArena env "world_evict" 40 40
        let b = Just (bounds 40 40)
        _ ← install env pid ws (request [pad, pit] [pond] [oak, beet, wheat])
        stored ← readIORef (wsPageTagsRef ws)
        plotStore ← readIORef (wsCropPlotsRef ws)
        before ← captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0
        -- evict chunk (0,0) through the pipeline's own pair
        params ← fromMaybe (error "params") ⊚ readIORef (wsGenParamsRef ws)
        td ← readIORef (wsTilesRef ws)
        let (td', evicted) = evictDistantChunksWithReport (ChunkCoord 1 1) 0 td { wtdMaxChunks = 1 }
        evicted `shouldSatisfy` elem (ChunkCoord 0 0)
        releaseEvictedChunks ws pid params evicted
        writeIORef (wsTilesRef ws) td' { wtdMaxChunks = 200 }
        isNothing ⊚ lookupChunk (ChunkCoord 0 0) ⊚ readIORef (wsTilesRef ws) ≫= (`shouldBe` True)
        -- kept, and the durable log alone still reads as the same scenario
        readIORef (wsPageTagsRef ws) `shouldReturn` stored
        readIORef (wsCropPlotsRef ws) `shouldReturn` plotStore
        captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0 ≫= (`shouldBe` before)
        -- readmit the regenerated, replayed chunk through the admission hook
        edits ← readIORef (wsEditsRef ws)
        let fresh = clipArenaChunk b (replayEdits edits (generateArenaChunkIn b 0 (ChunkCoord 0 0)))
        logger ← initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }
        atomicModifyIORef' (wsTilesRef ws) $ \t → (insertChunk fresh t, ())
        readIORef (wsTilesRef ws) ≫= reconcileAdmittedPageTags logger ws pid [fresh]
        readIORef (wsPageTagsRef ws) `shouldReturn` stored
        captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0 ≫= (`shouldBe` before)

    it "replaces fluid the simulation carried into a chunk the log never named" $ \env → do
        (pid, ws) ← freshArena env "world_flow" 40 40
        _ ← install env pid ws (request [] [] [])
        -- a writeback put water into chunk (0,0), which has no log entry of its own
        atomicModifyIORef' (wsTilesRef ws) $ \td →
            let lc = fromMaybe (error "resident") (lookupChunk (ChunkCoord 0 0) td)
                lc' = lc { lcFluidMap = lcFluidMap lc V.// [(columnIndex 3 3, Just (FluidCell Lake 35))] }
            in (insertChunk lc' td, ())
        liveFluid ws (3, 3) `shouldReturn` Just (FluidCell Lake 35)
        _ ← install env pid ws (request [] [] [])
        liveFluid ws (3, 3) `shouldReturn` Nothing

    it "judges tagged cells in a chunk that is not resident against the replacement: a deleted cell's tags are not inherited" $ \env → do
        (pid, ws) ← freshArena env "world_deadtag" 120 120
        let far tags = terrainPatch "far" (RegionTiles [(50, 50)]) "granite" 2 Omitted tags
            tagsAt = readIORef (wsPageTagsRef ws)
                ≫= \t → pure (categoryTagsOf (tileTargetFromStored pid 50 50 2) (ptTiles t))
        _ ← install env pid ws (request [far [tag "far"]] [] [])
        tagsAt ≫= (`shouldBe` Set.fromList [tag "far"])
        -- the SAME cell again, untagged: it survives the replacement, so it keeps its tags
        _ ← install env pid ws (request [far []] [] [])
        tagsAt ≫= (`shouldBe` Set.fromList [tag "far"])
        -- removed, then recreated before the chunk was ever admitted: a new cell inherits nothing
        _ ← install env pid ws (request [] [] [])
        tagsAt ≫= (`shouldBe` Set.empty)
        _ ← install env pid ws (request [far []] [] [])
        tagsAt ≫= (`shouldBe` Set.empty)

    it "keeps an authored slope through the real chunk admission, on a new chunk and on its resident neighbour" $ \env → do
        (pid, ws) ← freshArena env "world_slope" 120 120
        let ramp x y = terrainPatch "ramp" (RegionTiles [(x, y)]) "loam" 0 (Authored 5) []
        -- (48,48) is local (0,0) of chunk (3,3), (47,47) is local (15,15) of resident chunk (2,2)
        _ ← install env pid ws (request [ramp 48 48, (ramp 47 47) { tpId = ex "ramp2" }] [] [])
        -- (a ramp on the base shape is only a slope override: the cell, and its grass, is untouched)
        fmap (\(t, m, sl, _) → (t, m, sl)) ⊚ liveColumn ws (47, 47) ≫= (`shouldBe` Just (0, Just (unMaterialId matLoam), 5))
        isNothing ⊚ lookupChunk (ChunkCoord 3 3) ⊚ readIORef (wsTilesRef ws) ≫= (`shouldBe` True)
        n ← enqueueChunkRequest pid ws [ChunkCoord 3 3]
        n `shouldBe` 1
        waitForChunksAt ws (ChunkCoord 3 3) 30 `shouldReturn` True
        -- the admitted chunk's border strip was recomputed from terrain: the override survived it
        fmap (\(_, _, sl, _) → sl) ⊚ liveColumn ws (48, 48) ≫= (`shouldBe` Just 5)
        -- and the neighbour's border was recomputed too
        fmap (\(_, _, sl, _) → sl) ⊚ liveColumn ws (47, 47) ≫= (`shouldBe` Just 5)

    it "survives the world thread's OWN eviction and readmission: materialize, evict, readmit, capture and compare through the real paths" $ \env →
        withVisibleArena env 200 200 $ \pid ws → do
            let b = Just (bounds 200 200)
                home = ChunkCoord 0 0
                away = ChunkCoord (-5) (-5)       -- a full 5×5 window of valid chunks (−7…−3)
                ramp = (terrainPatch "ramp" (RegionTiles [(15, 15)]) "loam" 0 (Authored 5) [tag "ramp"])   -- a chunk-edge tile
                dead = (floraEntry "dead" "beet" 6 6) { feAge = Authored 365, feHealth = Authored 1 }   -- an Annual: dead window
            _ ← install env pid ws
                    (request [pad, pit, ramp] [pond] [oak, beet, wheat, dead])
            -- the page's clock runs, so a regrowth timer only ever counts DOWN between
            -- two reads; everything else must read the same
            let steady cw = cw { cwFlora = [ e { feRegrowth = fmap (const 0) (feRegrowth e) } | e ← cwFlora cw ] }
                capture = captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0
            before ← capture
            growthBefore ← map (\o → (fiSpecies o, floraGrowth (speciesOf (fiSpecies o)) 0 o)) ⊚ liveOccurrences ws
            -- evict: the camera loader spends its budget on the far window
            atomicModifyIORef' (wsTilesRef ws) $ \td → (td { wtdMaxChunks = 25 }, ())
            goToChunk env away
            _ ← awaitChunk ws away
            waitUntil 30 (isNothing ⊚ residentChunk ws home) `shouldReturn` True
            -- gone from the tile store, yet the durable log reads as the same scenario
            (steady ⊚ capture) ≫= (`shouldBe` steady before)
            -- readmit through the real drain
            goToChunk env home
            _ ← awaitChunk ws home
            after ← capture
            cwTerrain after `shouldBe` cwTerrain before
            cwFluids after `shouldBe` cwFluids before
            cwDiagnostics after `shouldBe` cwDiagnostics before
            map (\e → e { feRegrowth = Omitted }) (cwFlora after) `shouldBe` map (\e → e { feRegrowth = Omitted }) (cwFlora before)
            forM_ (zip (cwFlora before) (cwFlora after)) $ \(x, y) →
                case (feRegrowth x, feRegrowth y) of
                    (Authored a, Authored c) → (c ≤ a) `shouldBe` True
                    (a, c)                   → a `shouldBe` c
            -- through the owners the runtime reads
            -- (the pad's border column holds a DERIVED slope bit now — the runtime recomputes
            -- border slopes on admission, depending on which neighbours are resident — which
            -- is exactly why capture reads only the AUTHORED slope)
            fmap (\(t, m, _, _) → (t, m)) ⊚ liveColumn ws (0, 0) ≫= (`shouldBe` Just (3, Just (unMaterialId matGranite)))
            liveColumn ws (15, 15) ≫= (`shouldSatisfy` maybe False (\(_, _, sl, _) → sl ≡ 5))   -- the slope survived the admission recompute
            liveFluid ws (6, 5) `shouldReturn` Just (FluidCell Lake 35)
            os ← liveOccurrences ws
            map (\o → (fiSpecies o, floraGrowth (speciesOf (fiSpecies o)) 0 o)) os `shouldMatchList` growthBefore
            -- every plant stands where the renderer and picker put it
            forM_ os $ \o → do
                mc ← chunkAtTile ws (fromIntegral (fiTileX o), fromIntegral (fiTileY o))
                let col = lcTiles (fromMaybe (error "resident") mc) V.! columnIndex (fromIntegral (fiTileX o)) (fromIntegral (fiTileY o))
                findTopSolid col `shouldBe` fiZ o
            tags ← readIORef (wsPageTagsRef ws)
            categoryTagsOf (tileTargetFromStored pid 0 0 3) (ptTiles tags) `shouldBe` Set.fromList [tag "pad"]
            HM.size ⊚ readIORef (wsFloraHarvestsRef ws) ≫= (`shouldBe` 1)

    it "keeps an explicit flat slope through admission, and honours later edits when restoring authored slopes" $ \env → do
        (pid, ws) ← freshArena env "world_slope2" 120 120
        -- an explicit FLAT on a raised chunk-border tile (47,47) = local (15,15) of resident chunk (2,2)
        _ ← install env pid ws
                (request [terrainPatch "flat" (RegionTiles [(47, 47)]) "granite" 3 (Authored 0) []] [] [])
        -- hand-authored histories on three more border tiles of the same chunk, applied the way
        -- every live edit is: to the chunk and to the log, in order
        let granite = matGranite
            histories =
                [ [WeSetSlope 47 40 0 5]                                                  -- stays
                , [WeSetSlope 47 41 0 5, WeSetCell 47 41 0 granite]                       -- cleared by a later cell write
                , [WeSetSlope 47 42 0 5, WeDeleteTile 47 42, WeAddTile 47 42 granite] ]   -- delete and refill
            slopeAt t = fmap (\(_, _, sl, _) → sl) ⊚ liveColumn ws t
        forM_ (concat histories) $ \e → do
            atomicModifyIORef' (wsEditsRef ws) $ \ls → (appendEdit (ChunkCoord 2 2) e ls, ())
            atomicModifyIORef' (wsTilesRef ws) $ \td → case lookupChunk (ChunkCoord 2 2) td of
                Just lc → (insertChunk (applyEdit e lc) td, ())
                Nothing → (td, ())
        mapM slopeAt [(47, 47), (47, 40), (47, 41), (47, 42)]
            `shouldReturn` [Just 0, Just 5, Just 0, Just 0]
        -- the neighbouring chunk's admission recomputes this chunk's border strip
        n ← enqueueChunkRequest pid ws [ChunkCoord 3 2]
        n `shouldBe` 1
        waitForChunksAt ws (ChunkCoord 3 2) 30 `shouldReturn` True
        -- explicit flat (not the derived bit), the effective override, and both later edits respected
        mapM slopeAt [(47, 47), (47, 40), (47, 41), (47, 42)]
            `shouldReturn` [Just 0, Just 5, Just 0, Just 0]

    it "keeps an explicit flat override on a base-shaped tile through capture, materialize and readmission" $ \env → do
        let b = Just (bounds 40 40)
            home = ChunkCoord 0 0
            flatTile = terrainPatch "flat" (RegionTiles [(15, 8)]) "loam" baseTopZ (Authored 0) []
            pitTile = terrainPatch "pit" (RegionTiles [(16, 8)]) "granite" (baseTopZ - 1) (Authored 0) []
            slopeAt ws t = fmap (\(_, _, sl, _) → sl) ⊚ liveColumn ws t
            evictAndReadmit pid ws = do
                params ← fromMaybe (error "params") ⊚ readIORef (wsGenParamsRef ws)
                atomicModifyIORef' (wsTilesRef ws) $ \td → (td { wtdChunks = HM.delete home (wtdChunks td) }, ())
                releaseEvictedChunks ws pid params [home]
                _ ← enqueueChunkRequest pid ws [home]
                waitForChunksAt ws home 30 `shouldReturn` True
        (pid, ws) ← freshArena env "world_flat1" 40 40
        _ ← install env pid ws (request [flatTile, pitTile] [] [])
        evictAndReadmit pid ws
        slopeAt ws (15, 8) `shouldReturn` Just 0               -- the authored flat survives readmission
        cw ← captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0
        concatMap (\p → case tpRegion p of RegionTiles ts → ts; RegionRect x0 y0 x1 y1 → [ (x, y) | y ← [y0 .. y1], x ← [x0 .. x1] ]) (cwTerrain cw)
            `shouldSatisfy` elem (15, 8)
        -- materialize the capture on a fresh page: the override is a durable edit again
        (pid2, ws2) ← freshArena env "world_flat2" 40 40
        _ ← install env pid2 ws2 (ScenarioWorldRequest
                (withPaths "terrain" (cwTerrain cw)) (withPaths "fluids" (cwFluids cw))
                (withPaths "flora" (cwFlora cw)))
        evictAndReadmit pid2 ws2
        slopeAt ws2 (15, 8) `shouldReturn` Just 0
        captureWorldContent Set.empty ⊚ liveSnapshot env ws2 b 0 ≫= (`shouldBe` cw)

    it "property: random edit sequences admitted after neighbour admission and after eviction equal the in-memory replay" $ \env → do
        -- random sequences of set-slope (zero included), set-cell, dig (delete) and refill (add) on and around
        -- the border tiles of resident chunk (2,2), applied the way live edits are (chunk + log, in order)
        forM_ [1 .. 60 ∷ Int] $ \seed → do
            (pid, ws) ← freshArena env ("world_prop" <> T.pack (show seed)) 120 120
            let home = ChunkCoord 2 2
                b = Just (bounds 120 120)
                ops = randomEdits seed
            forM_ ops $ \e → do
                -- each edit goes to the chunk that owns its tile: the resident home chunk, or the
                -- not-yet-admitted neighbour's log (replayed when it is admitted)
                let owner = fst (uncurry globalToChunk (editTile e))
                atomicModifyIORef' (wsEditsRef ws) $ \ls → (appendEdit owner e ls, ())
                atomicModifyIORef' (wsTilesRef ws) $ \td → case lookupChunk owner td of
                    Just lc → (insertChunk (applyEdit e lc) td, ())
                    Nothing → (td, ())
            let agrees label = do
                    edits ← readIORef (wsEditsRef ws)
                    live ← awaitChunk ws home
                    let replay = chunkOf b edits home
                        named = Set.fromList [ (x, y, z) | WeSetSlope x y z _ ← ops ]
                    (label, sameShape live replay) `shouldBe` (label, [])
                    (label, slopesAt named live replay) `shouldBe` (label, [])
            agrees "before admission"
            -- a neighbour's admission recomputes this chunk's border strip
            forM_ [ChunkCoord 3 2, ChunkCoord 2 3, ChunkCoord 3 3] $ \c → do
                _ ← enqueueChunkRequest pid ws [c]
                waitForChunksAt ws c 30 `shouldReturn` True
            agrees "after neighbour admission"
            -- evict the chunk through the pipeline's own release, then demand it back
            params ← fromMaybe (error "params") ⊚ readIORef (wsGenParamsRef ws)
            atomicModifyIORef' (wsTilesRef ws) $ \td → (td { wtdChunks = HM.delete home (wtdChunks td) }, ())
            releaseEvictedChunks ws pid params [home]
            _ ← enqueueChunkRequest pid ws [home]
            waitForChunksAt ws home 30 `shouldReturn` True
            agrees "after eviction and readmission"
            -- the full round trip: capture the live page, materialize into a FRESH page, admit the
            -- neighbours, evict and readmit, and compare with the original and with a second capture
            edits ← readIORef (wsEditsRef ws)
            let ownedTops =
                    [ (x, y, z) | WeSetSlope x y z _ ← ops ]
            cw ← captureWorldContent Set.empty ⊚ liveSnapshot env ws b 0
            cwDiagnostics cw `shouldBe` []
            (pidB, wsB) ← freshArena env ("world_propb" <> T.pack (show seed)) 120 120
            _ ← install env pidB wsB (ScenarioWorldRequest
                    (withPaths "terrain" (cwTerrain cw)) (withPaths "fluids" (cwFluids cw))
                    (withPaths "flora" (cwFlora cw)))
            forM_ [ChunkCoord 3 2, ChunkCoord 2 3, ChunkCoord 3 3] $ \c → do
                _ ← enqueueChunkRequest pidB wsB [c]
                waitForChunksAt wsB c 30 `shouldReturn` True
            paramsB ← fromMaybe (error "params") ⊚ readIORef (wsGenParamsRef wsB)
            atomicModifyIORef' (wsTilesRef wsB) $ \td → (td { wtdChunks = HM.delete home (wtdChunks td) }, ())
            releaseEvictedChunks wsB pidB paramsB [home]
            _ ← enqueueChunkRequest pidB wsB [home]
            waitForChunksAt wsB home 30 `shouldReturn` True
            forM_ [home, ChunkCoord 3 2] $ \cc → do
                orig ← awaitChunk ws cc
                copy ← awaitChunk wsB cc
                let tilesOf = [ chunkToGlobal cc lx ly | ly ← [0 .. 15], lx ← [0 .. 15] ]
                    surface lc = [ (t, (\(tz, m, _, _) → (tz, m)) (columnAt lc t)) | t ← tilesOf ]
                    -- the slope of every tile whose top cell the log owns an override for
                    owned lc = [ (t, (\(_, _, sl, _) → sl) (columnAt lc t))
                               | t@(x, y) ← tilesOf
                               , let (tz, _, _, _) = columnAt orig t
                               , (x, y, tz) `elem` ownedTops ]
                (cc, surface copy) `shouldBe` (cc, surface orig)
                (cc, owned copy) `shouldBe` (cc, owned orig)
            captureWorldContent Set.empty ⊚ liveSnapshot env wsB b 0 ≫= (`shouldBe` cw)
            edits `shouldSatisfy` (not ∘ HM.null)
      where
        gen0 seed = mkStdGen (seed * 7919)
        randomEdits seed =
            let (n, g1) = randomR (3, 12 ∷ Int) (gen0 seed)
            in take n (unfoldr (Just ∘ nextOp) g1)
        tiles = [(47, 34), (47, 47), (40, 47), (32, 40), (33, 33), (36, 36), (47, 40), (46, 34), (46, 40), (47, 41), (46, 47), (40, 46), (48, 34), (48, 40), (48, 41), (48, 47)]
        nextOp g =
            let (ti, g1) = randomR (0, length tiles - 1) g
                (x, y) = tiles !! ti
                (kind, g2) = randomR (0, 4 ∷ Int) g1
                (dz, g3) = randomR (-1, 1 ∷ Int) g2
                z = baseTopZ + dz
                (bi, g4) = randomR (0, 4 ∷ Int) g3
                bits = [0, 0, 1, 5, 15] !! bi
                (mi, g5) = randomR (0, 1 ∷ Int) g4
                mat = [matGranite, matSand] !! mi
                edit = case kind of
                    0 → WeSetSlope x y z bits
                    1 → WeSetSlope x y z 0
                    2 → WeSetCell x y z mat
                    3 → WeDeleteTile x y
                    _ → WeAddTile x y mat
            in (edit, g5)
        columns lc = [ (columnIndex lx ly, lcTiles lc V.! columnIndex lx ly) | lx ← [0 .. 15], ly ← [0 .. 15] ]
        -- the physical shape: every column's start, materials and surface
        sameShape live replay =
            [ (idx, "shape")
            | ((idx, a), (_, c)) ← zip (columns live) (columns replay)
            , ctStartZ a ≢ ctStartZ c ∨ ctMats a ≢ ctMats c
              ∨ lcTerrainSurfaceMap live VU.! idx ≢ lcTerrainSurfaceMap replay VU.! idx ]
        -- the slope of each cell any set-slope names: the replay's value
        slopesAt named live replay =
            [ (x, y, z)
            | (x, y, z) ← Set.toList named
            , let (_, (lx, ly)) = globalToChunk x y
                  idx = columnIndex lx ly
                  at' lc = let col = lcTiles lc V.! idx; i = z - ctStartZ col
                           in if i ≥ 0 ∧ i < VU.length (ctSlopes col) then Just (ctSlopes col VU.! i) else Nothing
            , at' live ≢ at' replay ]

-- | A finite arena page that is VISIBLE (the camera loader serves visible pages).
withVisibleArena ∷ EngineEnv → Int → Int → (WorldPageId → WorldState → IO α) → IO α
withVisibleArena env w h act = do
    pid ← uniquePage "world_visible"
    writeIORef (floraCatalogRef env) catalog
    logger ← readIORef (loggerRef env)
    loadPopulatedMaterialRegistry logger "data/materials" ≫= writeIORef (materialRegistryRef env)
    sendWorldCommand env (WorldInitArena pid (Just (bounds w h)))
    ws ← waitForWorldInit env pid 60
    sendWorldCommand env (WorldInitArenaDone pid)
    visible ← waitUntil 30 (elem pid ∘ wmVisible ⊚ readIORef (worldManagerRef env))
    unless visible $ expectationFailure "the arena never became visible"
    goToChunk env (ChunkCoord 0 0)
    act pid ws `finally` do
        sendWorldCommand env (WorldDestroy pid)
        goToChunk env (ChunkCoord 0 0)

waitUntil ∷ Int → IO Bool → IO Bool
waitUntil secs cond = go (secs * 20)
  where go k | k ≤ 0 = pure False
             | otherwise = cond ≫= \ok → if ok then pure True
                                         else threadDelay 50000 ≫ go (k - 1 ∷ Int)

-- | Point the camera at the middle of a chunk.
goToChunk ∷ EngineEnv → ChunkCoord → IO ()
goToChunk env (ChunkCoord cx cy) = do
    facing ← camFacing ⊚ readIORef (cameraRef env)
    let (wx, wy) = gridToWorld facing (cx * 16 + 8) (cy * 16 + 8)
    atomicModifyIORef' (cameraRef env) $ \cam → (cam { camPosition = (wx, wy), camZoom = 0.5 }, ())

residentChunk ∷ WorldState → ChunkCoord → IO (Maybe LoadedChunk)
residentChunk ws c = lookupChunk c ⊚ readIORef (wsTilesRef ws)

awaitChunk ∷ WorldState → ChunkCoord → IO LoadedChunk
awaitChunk ws c = do
    ok ← waitForChunksAt ws c 60
    unless ok $ expectationFailure ("chunk never became resident: " ⧺ show c)
    residentChunk ws c ≫= maybe (expectationFailure "chunk vanished" ≫ error "unreachable") pure
