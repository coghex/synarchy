{-# LANGUAGE Strict #-}
-- | The LIVE half of the world-content tests (#2711): the commit
--   ('Scenario.World.Install') onto a real arena page through the real
--   world thread, read back from the page's own owners, then through the
--   ordinary save codec and the real staging, and captured again.
module Test.Headless.Scenario.World.Live (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
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
import Engine.Graphics.Camera (CameraFacing(..))
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
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), columnIndex)
import World.Command.Types (WorldCommand(..))
import World.Chop.Types (newChopDesignation)
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
        map (\(c, _, _, _) → c) edited `shouldBe` [ChunkCoord 1 0]
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
        wirChunks r `shouldBe` []                  -- nothing resident was rebuilt
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
        (namedEdits, namedCrops, _) ← either (const (error "unnamed species")) pure
            (nameFloraReferences catalog pid edits (cpsPlots plots) HM.empty)
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
                , snapPortableKnowledge = emptyPortableKnowledge }
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
