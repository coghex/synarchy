{-# LANGUAGE OverloadedStrings #-}
-- | "Gameplay.Tags.Residency" (#2705, SCN-05 of epic #2698): general
--   gameplay tags on terrain CELLS and PLANTS — wild and planted flora
--   occurrences and groundcover crop plots — owned by their page, live
--   only while resident, kept across eviction, removed only by real
--   destruction, and carried by every save and load.
--
--   Every transition is a production boundary: tag edits through the
--   world thread's own 'handleWorldEditPageTagsCommand'; eviction
--   through 'evictDistantChunksWithReport' and 'releaseEvictedChunks',
--   the pipeline's own pair; admission through
--   'reconcileAdmittedPageTags', the hook both chunk-loading commits and
--   the staged seeds run; in-place destruction through
--   'World.Edit.Apply.applyEdit' and 'replaceChunkForgettingFlora', which
--   every edit handler uses; flora removal through
--   'forgetFloraInstances'; plot removal and planting through the plot
--   store's own 'removePlot' / 'insertPlot', the exact steps the harvest
--   and planting verbs take; and the whole-session cases through the
--   real codec, 'stageSession' and 'publishStagedSession'.
--
--   The scene is hand-built, the "Test.Headless.World.FloraIdentity"
--   technique, so every cell is known: each column stores z 5 … 10 with
--   an internal AIR cell at z 7, so z 4 and z 11 are unstored.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Gameplay.Tags.Residency"'@.
module Test.Headless.Gameplay.TagsResidency (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log (LoggerState, LogEntry(..), LogConfig(..), LogBackend(..)
                       , initLogger, defaultLogConfig)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Gameplay.Tags.Edit
import Gameplay.Tags.Index
import Gameplay.Tags.Live
import Gameplay.Tags.Page
import Gameplay.Tags.PageLive (reconcileAdmittedPageTags)
import Data.List (sort)
import Gameplay.Tags.Query
import Gameplay.Tags.Registry (registryInvariantViolations)
import Gameplay.Tags.Types
import Item.Knowledge (emptyPortableKnowledge)
import Structure.Palette (emptyTexPalette)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Chunk.Admit (pageIncarnation, releaseEvictedChunks)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (TagEditReply(..))
import World.Edit.Apply (applyEdit, replayEdits)
import World.Edit.Types (WorldEdit(..))
import World.Flora.CropPlot (CropPlot, CropPlotOf(..))
import World.Flora.Designation (forgetFloraInstances, replaceChunkForgettingFlora)
import World.Flora.Identity
    (FloraInstanceId, floraInstanceIdNone, generatedFloraInstanceId, plantedFloraInstanceId)
import World.Flora.Reference (FloraRef(..))
import World.Flora.Types
    ( FloraCatalog(..), FloraChunkData(..), FloraId(..), FloraInstance(..)
    , emptyFloraCatalog, newFloraSpecies )
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams, withVolcanoCtx)
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (stageSession, renderStageError)
import World.Material.Id (MaterialId(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Component.PageEdits
    ( WorldEditsDTO(..), PageEditsDTO(..), WorldEditsDTOv4(..), PageEditsDTOv4(..)
    , worldEditsCodec )
import Gameplay.Tags.PageLive (mutatePageTargetTags)
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Integrity (IntegrityError(..), sessionIntegrityWarnings)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Thread.Command.Basic (handleWorldEditPageTagsCommand)
import World.Thread (processAuthorizedSave)
import World.Command.Types (WorldCommand(..))
import qualified Engine.Core.Queue as Q
import System.Timeout (timeout)
import World.Save.Types (nameFloraReferences)
import World.Tile.Types (WorldTileData(..), evictDistantChunksWithReport, insertChunk, lookupChunk)
import World.Thread.Command.Init (registerPageIncarnation)

-- * Vocabulary

tg ∷ Text → GameplayTag
tg t = fromMaybe (error "an empty fixture tag") (mkGameplayTag t)

tags ∷ [Text] → Set.Set GameplayTag
tags = Set.fromList . map tg

-- * The scene

worldSize ∷ Int
worldSize = 8

pageP, pageQ ∷ WorldPageId
pageP = WorldPageId "residency_page"
pageQ = WorldPageId "residency_other"

params ∷ WorldGenParams
params = defaultWorldGenParams { wgpWorldSize = worldSize }

canon ∷ ChunkCoord → ChunkCoord
canon = canonicalChunkCoord params

-- | Two resident chunks: A, which the eviction examples evict, and B,
--   which stays.
chunkA, chunkB, chunkC ∷ ChunkCoord
chunkA = ChunkCoord 0 0
chunkB = ChunkCoord 1 0
-- | Outside a restored arena's seed set (chunks −2 … 2), so its targets
--   stay absent after a load until the chunk is admitted again.
chunkC = ChunkCoord 0 3

-- | A column in A, its tiles, and the cells everything below names.
colTile, otherTile, plotTile, farTile, farRowTile, farPlotTile ∷ (Int, Int)
colTile   = (3, 4)
otherTile = (6, 9)
plotTile  = (10, 2)
farTile   = (3, 3 * 16 + 4)
farRowTile  = (5, 3 * 16 + 4)
farPlotTile = (8, 3 * 16 + 2)

-- | A cell target on @page@ at a canonical tile and z.
cell ∷ WorldPageId → (Int, Int) → Int → TileTarget
cell page (x, y) z = mkTileTarget canon page x y z

-- | Columns store z 5 … 10, with an AIR cell at z 7.
column ∷ ColumnTiles
column = ColumnTiles
    { ctStartZ = 5, ctMats = VU.fromList [1, 1, 0, 1, 1, 1]
    , ctSlopes = VU.replicate 6 0, ctVeg = VU.replicate 6 0 }

oakId, berryId, cropId ∷ FloraId
oakId = FloraId 1; berryId = FloraId 2; cropId = FloraId 3

catalog ∷ FloraCatalog
catalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, newFloraSpecies "res_oak" (TextureHandle 0))
        , (2, newFloraSpecies "res_berry" (TextureHandle 0))
        , (3, newFloraSpecies "res_wheat" (TextureHandle 0)) ]
    , fcNextId = 4 }

-- | Wild co-tenants (an oak and a berry bush on 'colTile'), a wild oak
--   on 'otherTile', and a PLANTED row crop beside it.
oakOcc, berryOcc, loneOak, rowCrop, farOak, farRow ∷ FloraInstanceId
oakOcc   = generatedFloraInstanceId (unWorldPageId pageP) 3 4 "res_oak" 0
berryOcc = generatedFloraInstanceId (unWorldPageId pageP) 3 4 "res_berry" 0
loneOak  = generatedFloraInstanceId (unWorldPageId pageP) 6 9 "res_oak" 0
rowCrop  = plantedFloraInstanceId 7
farOak   = generatedFloraInstanceId (unWorldPageId pageP) 3 52 "res_oak" 0
farRow   = plantedFloraInstanceId 11

plant ∷ (Int, Int) → FloraId → FloraInstanceId → FloraInstance
plant (gx, gy) fid iid = FloraInstance
    { fiSpecies = fid, fiTileX = fromIntegral (gx `mod` chunkSize)
    , fiTileY = fromIntegral (gy `mod` chunkSize)
    , fiOffU = 0, fiOffV = 0, fiZ = 10, fiAge = 1, fiHealth = 1
    , fiVariant = 0, fiBaseWidth = 8, fiInstanceId = iid, fiChopDesignated = False }

chunkWith ∷ ChunkCoord → [FloraInstance] → LoadedChunk
chunkWith coord flora =
    let area = chunkSize * chunkSize
    in LoadedChunk
        { lcCoord = coord, lcTiles = V.replicate area column
        , lcSurfaceMap = VU.replicate area 10
        , lcTerrainSurfaceMap = VU.replicate area 10
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = FloraChunkData flora
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.replicate area (-100)
        , lcMagma = Nothing, lcStructures = emptyChunkStructures }

floraA ∷ [FloraInstance]
floraA = [ plant colTile oakId oakOcc, plant colTile berryId berryOcc
         , plant otherTile oakId loneOak, plant (7, 9) cropId rowCrop ]

freshChunkA, freshChunkB, freshChunkC ∷ LoadedChunk
freshChunkA = chunkWith chunkA floraA
freshChunkB = chunkWith chunkB []
freshChunkC = chunkWith chunkC [plant farTile oakId farOak, plant farRowTile cropId farRow]

-- | The planting edit the row crop in chunk C came from, as the live
--   edit log holds it.
plantFarRow ∷ WorldEdit
plantFarRow = WePlaceFloraWithId (fst farRowTile) (snd farRowTile) cropId 0 8 farRow

-- | Chunk C as a fresh REGENERATION produces it, before any replay: the
--   wild oak only. The row crop comes back only by replaying a page's
--   edit log over it.
regeneratedChunkC ∷ LoadedChunk
regeneratedChunkC = chunkWith chunkC [plant farTile oakId farOak]

wheatPlot ∷ CropPlot
wheatPlot = CropPlot cropId 0 1.0

-- | One page, both chunks resident, one wheat plot, nothing tagged.
newPage ∷ IO WorldState
newPage = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just params)
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.fromList [ (chunkA, freshChunkA), (chunkB, freshChunkB)
                                  , (chunkC, freshChunkC) ]
        , wtdMaxChunks = 200, wtdBounds = Nothing }
    writeIORef (wsCropPlotsRef ws)
        (plotStoreFromPlots (HM.fromList [(plotTile, wheatPlot), (farPlotTile, wheatPlot)]))
    pure ws

-- | The session: page P, and page Q with the very same layout, so equal
--   coordinates and occurrence counters on another page are exercised.
resetScene ∷ EngineEnv → IO (WorldState, WorldState)
resetScene env = do
    wsP ← newPage
    wsQ ← newPage
    epoch ← wmSessionEpoch ⊚ readIORef (worldManagerRef env)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageP, wsP), (pageQ, wsQ)], wmVisible = [pageP]
        , wmSessionEpoch = epoch }
    writeIORef (floraCatalogRef env) catalog
    pure (wsP, wsQ)

-- * Through the production boundaries

-- | One edit through the world thread's own handler (this test thread
--   plays the world thread; nothing else writes these pages).
edit ∷ EngineEnv → WorldState → TagTarget → TagEdit → IO TagEditResult
edit env ws t e = do
    epoch ← pageIncarnation ws
    reply ← newEmptyMVar
    let page = case t of
            TargetTile x  → tileTargetPage x
            TargetPlant p → plantTargetPage p
            _             → pageP
    handleWorldEditPageTagsCommand env page epoch t e (TagEditReply reply)
    takeMVar reply

tag ∷ EngineEnv → WorldState → TagTarget → [Text] → IO ()
tag env ws t ts = edit env ws t (AddTagsEdit (tags ts)) `shouldReturn` TagEditApplied

occ ∷ WorldPageId → FloraInstanceId → TagTarget
occ page fid = TargetPlant (fromMaybe (error "none") (mkPlantTarget page fid))

plot ∷ WorldPageId → (Int, Int) → TagTarget
plot page (x, y) = TargetPlant (mkPlotTarget worldSize page x y)

view ∷ EngineEnv → IO LiveTagView
view env = captureLiveTagView (unitManagerRef env) (buildingManagerRef env) (worldManagerRef env)

-- | A query over the live tile and plant universes.
query ∷ EngineEnv → TagQuery → IO [TagTarget]
query env q = do
    v ← view env
    pure (targetSetsToList (runLiveTagQueryIn [TileCategory, PlantCategory] v id q))

-- | list / count / exists over one view agree.
agrees ∷ EngineEnv → TagQuery → IO ()
agrees env q = do
    v ← view env
    let r = liveTagRegistry v
        u = liveTagUniverseFor [TileCategory, PlantCategory] v
        listed = queryList r u q
    queryCount r u q `shouldBe` length listed
    queryExists r u q `shouldBe` not (null listed)

-- | Both index directions agree everywhere, and each named target holds
--   exactly these tags in BOTH directions.
coherent ∷ EngineEnv → [(TagTarget, [Text])] → IO ()
coherent env expected = do
    v ← view env
    let r = liveTagRegistry v
    registryInvariantViolations r `shouldBe` []
    forM_ expected $ \(t, ts) → do
        let fwd = case t of
                TargetTile x  → categoryTagsOf x (registryTiles' v)
                TargetPlant p → categoryTagsOf p (registryPlants' v)
                _             → Set.empty
        fwd `shouldBe` tags ts
        forM_ ts $ \name → case t of
            TargetTile x  → indexMembers (tg name) (registryTiles' v) `shouldSatisfy` Set.member x
            TargetPlant p → indexMembers (tg name) (registryPlants' v) `shouldSatisfy` Set.member p
            _             → pure ()
  where
    registryTiles' v = unionDisjointCategories [ ptTiles (pvTags pv) | (_, pv) ← ltvPages v ]
    registryPlants' v = unionDisjointCategories
        (concat [ [ptOccurrences (pvTags pv), cpsTags (pvPlots pv)] | (_, pv) ← ltvPages v ])

-- | Evict chunk A through the pipeline's own pair: the eviction policy
--   with the camera on B and room for one chunk, then the owner release.
evictA ∷ WorldState → IO [ChunkCoord]
evictA ws = do
    td ← readIORef (wsTilesRef ws)
    let (td', evicted) = evictDistantChunksWithReport chunkB 0 td { wtdMaxChunks = 1 }
    releaseEvictedChunks ws pageP params evicted
    writeIORef (wsTilesRef ws) td' { wtdMaxChunks = 200 }
    pure evicted

-- | Readmit a (regenerated, replayed) chunk A through the admission hook
--   the chunk pipeline commits through.
readmit, readmit' ∷ LoggerState → WorldState → LoadedChunk → IO ()
readmit' = readmit
readmit logger ws lc = do
    atomicModifyIORef' (wsTilesRef ws) $ \td → (insertChunk lc td, ())
    readIORef (wsTilesRef ws) ≫= reconcileAdmittedPageTags logger ws pageP [lc]

-- | An in-place edit of chunk A, as every edit handler applies it.
editChunkA ∷ WorldState → WorldEdit → IO ()
editChunkA ws e = do
    td ← readIORef (wsTilesRef ws)
    lc ← maybe (fail "chunk A not resident") pure (lookupChunk chunkA td)
    replaceChunkForgettingFlora ws lc (applyEdit e lc)

capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

-- * Spec

spec ∷ Spec
spec = describe "Gameplay.Tags.Residency" $ aroundAll setup $ do
    targetSpec
    querySpec
    residencySpec
    destructionSpec
    isolationSpec
    sessionSpec
  where
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        act env

-- | Requirement 2: exact live targets.
targetSpec ∷ SpecWith EngineEnv
targetSpec = describe "live targets" $ do

    it "several elevations of one column are separate cells; seam aliases \
       \are one" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (TargetTile (cell pageP colTile 5)) ["bedrock"]
        tag env ws (TargetTile (cell pageP colTile 10)) ["surface", "shared"]
        tag env ws (TargetTile (cell pageP colTile 8)) ["shared"]
        -- A u-alias of the column names the SAME cells.
        let s = worldSize * chunkSize `div` 2
            alias = (fst colTile + s, snd colTile - s)
        canonicalTile worldSize (fst alias) (snd alias) `shouldBe` colTile
        cell pageP alias 10 `shouldBe` cell pageP colTile 10
        tag env ws (TargetTile (cell pageP alias 10)) ["aliased"]
        coherent env
            [ (TargetTile (cell pageP colTile 5), ["bedrock"])
            , (TargetTile (cell pageP colTile 10), ["surface", "shared", "aliased"])
            , (TargetTile (cell pageP colTile 8), ["shared"]) ]

    it "air, unstored depths and unloaded chunks are refused, and nothing \
       \is generated or loaded to make them exist" $ \env → do
        (ws, _) ← resetScene env
        before ← readIORef (wsTilesRef ws)
        forM_ [ cell pageP colTile 7, cell pageP colTile 4, cell pageP colTile 11
              , cell pageP (40, 40) 6 ] $ \t →
            edit env ws (TargetTile t) (AddTagsEdit (tags ["x"]))
                `shouldReturn` TagEditNoTarget
        after ← readIORef (wsTilesRef ws)
        HM.keys (wtdChunks after) `shouldMatchList` HM.keys (wtdChunks before)
        query env (tagged (tg "x")) `shouldReturn` []
        coherent env []

    it "co-tenant occurrences, a planted row crop and a plot are separate \
       \typed targets; the reserved non-identity is never one" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (occ pageP oakOcc) ["tree"]
        tag env ws (occ pageP berryOcc) ["bush"]
        tag env ws (occ pageP rowCrop) ["sown"]
        tag env ws (plot pageP plotTile) ["field"]
        mkPlantTarget pageP floraInstanceIdNone `shouldBe` Nothing
        -- An absent occurrence and a tile with no plot create nothing.
        edit env ws (occ pageP (plantedFloraInstanceId 99)) (AddTagsEdit (tags ["x"]))
            `shouldReturn` TagEditNoTarget
        edit env ws (plot pageP (1, 1)) (AddTagsEdit (tags ["x"]))
            `shouldReturn` TagEditNoTarget
        coherent env
            [ (occ pageP oakOcc, ["tree"]), (occ pageP berryOcc, ["bush"])
            , (occ pageP rowCrop, ["sown"]), (plot pageP plotTile, ["field"]) ]

-- | Requirement 4: the query universe.
querySpec ∷ SpecWith EngineEnv
querySpec = describe "the live query universe" $ do

    it "tile results range over every resident non-air cell, below the \
       \surface too; air, unstored depths and absent chunks never appear" $
        \env → do
            (ws, _) ← resetScene env
            tag env ws (TargetTile (cell pageP colTile 10)) ["marked"]
            untagged ← query env (QComplement (tagged (tg "marked")))
            let tileHits = [ t | TargetTile t ← untagged, tileTargetPage t ≡ pageP ]
            -- Three resident chunks of 256 columns, five solid cells each.
            length tileHits `shouldBe` 3 * chunkSize * chunkSize * 5 - 1
            all (\t → tileTargetZ t ∈ [5, 6, 8, 9, 10]) tileHits `shouldBe` True
            TargetTile (cell pageP colTile 6) `elem` untagged `shouldBe` True
            agrees env (QComplement (tagged (tg "marked")))
            agrees env (tagged (tg "marked"))

    it "a writer that destroys and recreates a tagged cell on another \
       \thread waits out a reader's whole tags-plots-tiles read" $ \env → do
        (ws, _) ← resetScene env
        let c = cell pageP colTile 10
        tag env ws (TargetTile c) ["old"]
        started ← newEmptyMVar
        done ← newEmptyMVar
        blocked ← newIORef False
        let writerDuringRead = do
                _ ← forkIO $ do
                    putMVar started ()
                    editChunkA ws (WeDeleteTile (fst colTile) (snd colTile))
                    editChunkA ws (WeAddTile (fst colTile) (snd colTile) (MaterialId 1))
                    putMVar done ()
                takeMVar started
                -- The writer is running; it must not finish while this
                -- read holds the page lock.
                finished ← timeout 300000 (readMVar done)
                writeIORef blocked (isNothing finished)
        pv ← capturePageViewWith writerDuringRead ws
        readIORef blocked `shouldReturn` True
        -- Wholly before the transition: the original cell with its own tags.
        cellStatus (pvTiles pv) c `shouldBe` CellSolid
        categoryTagsOf c (ptTiles (pvTags pv)) `shouldBe` tags ["old"]
        timeout 5000000 (takeMVar done) `shouldReturn` Just ()
        -- Wholly after it: the recreated cell, with nothing carried over.
        pv' ← capturePageViewWith (pure ()) ws
        cellStatus (pvTiles pv') c `shouldBe` CellSolid
        categoryTagsOf c (ptTiles (pvTags pv')) `shouldBe` Set.empty

    it "the composed algebra and list/count/exists agree before and after \
       \a residency change" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (TargetTile (cell pageP colTile 10)) ["a", "b"]
        tag env ws (TargetTile (cell pageP colTile 9)) ["a"]
        tag env ws (occ pageP oakOcc) ["a"]
        let both = QIntersection (tagged (tg "a")) (tagged (tg "b"))
            either' = QUnion (tagged (tg "a")) (tagged (tg "b"))
        sort ⊚ query env both `shouldReturn` [TargetTile (cell pageP colTile 10)]
        length ⊚ query env either' `shouldReturn` 3
        mapM_ (agrees env) [both, either', QComplement either']
        _ ← evictA ws
        query env either' `shouldReturn` []
        mapM_ (agrees env) [both, either', QComplement either']

-- | Requirement 5: residency is not lifetime.
residencySpec ∷ SpecWith EngineEnv
residencySpec = describe "eviction and readmission" $ do

    it "eviction keeps every assignment out of live queries without \
       \retaining the chunk, and readmission brings them back" $ \env → do
        (ws, _) ← resetScene env
        (logger, entries) ← capturingLogger
        tag env ws (TargetTile (cell pageP colTile 10)) ["kept"]
        tag env ws (occ pageP oakOcc) ["kept"]
        tag env ws (occ pageP rowCrop) ["kept"]
        tag env ws (plot pageP plotTile) ["kept"]
        stored ← readIORef (wsPageTagsRef ws)
        evicted ← evictA ws
        evicted `shouldSatisfy` (chunkA ∈)
        isNothing ⊚ (lookupChunk chunkA ⊚ readIORef (wsTilesRef ws)) `shouldReturn` True
        -- Kept, and invisible.
        readIORef (wsPageTagsRef ws) `shouldReturn` stored
        query env (tagged (tg "kept")) `shouldReturn` []
        query env (QComplement (tagged (tg "kept")))
            ⌦ (`shouldSatisfy` all (\t → t ∉ [TargetTile (cell pageP colTile 10), occ pageP oakOcc]))
        v ← view env
        tileTagsIn v (cell pageP colTile 10) `shouldBe` Nothing
        -- Readmission of the regenerated chunk: exactly the same targets.
        readmit logger ws freshChunkA
        length ⊚ query env (tagged (tg "kept")) `shouldReturn` 4
        msgs ← map leMessage ⊚ entries
        msgs `shouldSatisfy` all (not . ("dropped tags" `T.isInfixOf`))

    it "an admission removes only the destroyed targets the admitted chunk \
       \owns, and logs each" $ \env → do
        (ws, _) ← resetScene env
        (logger, entries) ← capturingLogger
        tag env ws (TargetTile (cell pageP colTile 10)) ["gone"]
        tag env ws (TargetTile (cell pageP colTile 9)) ["stays"]
        tag env ws (occ pageP oakOcc) ["gone"]
        tag env ws (occ pageP berryOcc) ["stays"]
        tag env ws (TargetTile (cell pageP (20, 3) 10)) ["other_chunk"]
        _ ← evictA ws
        -- While evicted the cell at z 10 was dug away and the oak went
        -- with it: the replayed chunk is what admission sees.
        let replayed = (applyEdit (WeDeleteTile (fst colTile) (snd colTile)) freshChunkA)
                { lcFlora = FloraChunkData [ f | f ← floraA, fiInstanceId f ≢ oakOcc ] }
        readmit logger ws replayed
        coherent env
            [ (TargetTile (cell pageP colTile 9), ["stays"]), (occ pageP berryOcc, ["stays"])
            , (TargetTile (cell pageP (20, 3) 10), ["other_chunk"])
            , (TargetTile (cell pageP colTile 10), []), (occ pageP oakOcc, []) ]
        msgs ← map leMessage ⊚ entries
        msgs `shouldSatisfy` any ("cell(s) that are no longer stored non-air terrain: (3,4,10)" `T.isInfixOf`)
        msgs `shouldSatisfy` any ("flora occurrence(s) the admitted chunk no longer holds" `T.isInfixOf`)

-- | Requirement 6: actual destruction and replacement.
destructionSpec ∷ SpecWith EngineEnv
destructionSpec = describe "destruction and replacement" $ do

    it "a material change keeps the cell's tags; digging the top cell away \
       \removes only its own, and the exposed cell keeps its own" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (TargetTile (cell pageP colTile 10)) ["top", "shared"]
        tag env ws (TargetTile (cell pageP colTile 9)) ["below", "shared"]
        editChunkA ws (WeSetCell (fst colTile) (snd colTile) 9 (MaterialId 2))
        coherent env [ (TargetTile (cell pageP colTile 9), ["below", "shared"]) ]
        editChunkA ws (WeDeleteTile (fst colTile) (snd colTile))
        coherent env
            [ (TargetTile (cell pageP colTile 10), [])
            , (TargetTile (cell pageP colTile 9), ["below", "shared"]) ]
        sort ⊚ query env (tagged (tg "shared")) `shouldReturn` [TargetTile (cell pageP colTile 9)]

    it "a cell set to air loses its tags, and a cell recreated there \
       \inherits nothing" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (TargetTile (cell pageP colTile 8)) ["old"]
        editChunkA ws (WeSetCell (fst colTile) (snd colTile) 8 (MaterialId 0))
        editChunkA ws (WeSetCell (fst colTile) (snd colTile) 8 (MaterialId 1))
        v ← view env
        tileTagsIn v (cell pageP colTile 8) `shouldBe` Just Set.empty
        query env (tagged (tg "old")) `shouldReturn` []

    it "rooted flora removal forgets only the removed occurrence; a \
       \co-tenant elsewhere and its shared tag survive" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (occ pageP oakOcc) ["tree", "shared"]
        tag env ws (occ pageP berryOcc) ["bush"]
        tag env ws (occ pageP loneOak) ["tree", "shared"]
        -- Removing ONE co-tenant through the per-instance boundary.
        forgetFloraInstances ws [oakOcc]
        coherent env [ (occ pageP berryOcc, ["bush"]), (occ pageP loneOak, ["tree", "shared"])
                     , (occ pageP oakOcc, []) ]
        -- Digging a tile out takes every plant rooted on it.
        editChunkA ws (WeDeleteTile (fst colTile) (snd colTile))
        coherent env [ (occ pageP berryOcc, []), (occ pageP loneOak, ["tree", "shared"]) ]

    it "the harvest removes a plot with its tags in one step, and a plot \
       \replanted there starts untagged" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (plot pageP plotTile) ["field"]
        atomicModifyIORef' (wsCropPlotsRef ws) $ \ps → (removePlot plotTile ps, ())
        coherent env [ (plot pageP plotTile, []) ]
        atomicModifyIORef' (wsCropPlotsRef ws) $ \ps → (insertPlot plotTile wheatPlot ps, ())
        v ← view env
        plantTagsIn v (fromMaybe (error "plot") (case plot pageP plotTile of
                                                    TargetPlant p → Just p; _ → Nothing))
            `shouldBe` Just Set.empty
        query env (tagged (tg "field")) `shouldReturn` []

    it "the last tag removed deletes the entry from both indexes" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (TargetTile (cell pageP colTile 10)) ["only"]
        tag env ws (occ pageP oakOcc) ["only"]
        _ ← edit env ws (TargetTile (cell pageP colTile 10)) (RemoveTagsEdit (tags ["only"]))
        _ ← edit env ws (occ pageP oakOcc) ClearTagsEdit
        pt ← readIORef (wsPageTagsRef ws)
        nullCategoryIndex (ptTiles pt) `shouldBe` True
        nullCategoryIndex (ptOccurrences pt) `shouldBe` True
        ptOccurrenceTiles pt `shouldBe` Map.empty

-- | Requirement 7: page and session isolation.
isolationSpec ∷ SpecWith EngineEnv
isolationSpec = describe "page and session isolation" $ do

    it "equal coordinates and occurrence counters on another page are \
       \independent" $ \env → do
        (wsP, wsQ) ← resetScene env
        tag env wsP (TargetTile (cell pageP colTile 10)) ["p"]
        tag env wsQ (TargetTile (cell pageQ colTile 10)) ["q"]
        tag env wsQ (occ pageQ rowCrop) ["q"]
        coherent env [ (TargetTile (cell pageP colTile 10), ["p"])
                     , (TargetTile (cell pageQ colTile 10), ["q"])
                     , (occ pageP rowCrop, []), (occ pageQ rowCrop, ["q"]) ]
        -- A target of another page given to this page's store creates
        -- nothing in either.
        mutatePageTargetTags pageP wsP (TargetTile (cell pageQ colTile 9))
            (AddTagsEdit (tags ["x"])) `shouldReturn` TagEditNoTarget
        query env (tagged (tg "x")) `shouldReturn` []

    it "a tag request queued when a load publishes is answered, not left \
       \waiting, and changes nothing" $ \env → do
        (ws, _) ← resetScene env
        epoch ← pageIncarnation ws
        reply ← newEmptyMVar
        Q.writeQueue (worldQueue env)
            (WorldEditPageTags pageP epoch (TargetTile (cell pageP colTile 10))
                               (AddTagsEdit (tags ["late"])) (TagEditReply reply))
        Q.writeQueue (worldQueue env) (WorldLoadPublish 987654)
        (logger, _) ← capturingLogger
        processAuthorizedSave env logger
        timeout 5000000 (takeMVar reply) `shouldReturn` Just TagEditNoTarget
        query env (tagged (tg "late")) `shouldReturn` []

    it "a page replacement discards the old page's assignments, and an edit \
       \queued for the old incarnation never reaches the replacement" $ \env → do
        (wsP, _) ← resetScene env
        tag env wsP (TargetTile (cell pageP colTile 10)) ["old"]
        staleEpoch ← pageIncarnation wsP
        fresh ← newPage
        registerPageIncarnation env pageP $ \m →
            m { wmWorlds = [ if p ≡ pageP then (p, fresh) else (p, w) | (p, w) ← wmWorlds m ] }
        query env (tagged (tg "old")) `shouldReturn` []
        reply ← newEmptyMVar
        handleWorldEditPageTagsCommand env pageP staleEpoch
            (TargetTile (cell pageP colTile 10)) (AddTagsEdit (tags ["stale"])) (TagEditReply reply)
        takeMVar reply `shouldReturn` TagEditNoTarget
        readIORef (wsPageTagsRef fresh) `shouldReturn` emptyPageTags

-- | Requirements 8 and 9.
sessionSpec ∷ SpecWith EngineEnv
sessionSpec = describe "whole-session save and load" $ do

    it "a save made while a tagged chunk is evicted keeps its assignments, \
       \and the real codec round-trips them exactly" $ \env → do
        (ws, _) ← resetScene env
        tag env ws (TargetTile (cell pageP colTile 10)) ["evicted"]
        tag env ws (occ pageP oakOcc) ["evicted"]
        tag env ws (occ pageP rowCrop) ["sown"]
        tag env ws (plot pageP plotTile) ["field"]
        _ ← evictA ws
        snap ← capturedSession env
        let pt = pgsPageTags (pageOf snap pageP)
        Map.keys (ptsTiles pt) `shouldBe` [(3, 4, 10)]
        Map.keysSet (ptsOccurrences pt) `shouldBe` Set.fromList [oakOcc, rowCrop]
        Map.lookup oakOcc (ptsOccurrenceTiles pt) `shouldBe` Just colTile
        Map.keys (ptsPlots pt) `shouldBe` [plotTile]
        back ← throughCodec snap
        pgsPageTags (pageOf back pageP) `shouldBe` pt

    it "a load installs the saved assignments, rebuilds both indexes, \
       \replaces the outgoing ones and keeps evicted targets until their \
       \chunk is readmitted" $ \env → do
        (ws, _) ← resetScene env
        (logger, _) ← capturingLogger
        tag env ws (TargetTile (cell pageP farTile 10)) ["saved"]
        tag env ws (occ pageP farOak) ["saved"]
        tag env ws (plot pageP plotTile) ["saved"]
        snap ← capturedSession env ⌦ throughCodec
        tag env ws (occ pageP berryOcc) ["outgoing"]
        loadSession env logger snap
        live ← readIORef (worldManagerRef env)
        wsL ← maybe (fail "no page") pure (lookup pageP (wmWorlds live))
        pt ← readIORef (wsPageTagsRef wsL)
        categoryIncoherent (ptTiles pt) `shouldBe` False
        categoryIncoherent (ptOccurrences pt) `shouldBe` False
        indexMembers (tg "saved") (ptTiles pt) `shouldBe` Set.singleton (cell pageP farTile 10)
        indexMembers (tg "outgoing") (ptOccurrences pt) `shouldBe` Set.empty
        -- The restored arena does not hold chunk C, so its assignments
        -- wait, invisible, for its admission (the plot's chunk IS in the
        -- arena, so the plot is live at once).
        query env (tagged (tg "saved")) `shouldReturn` [plot pageP plotTile]
        isNothing ⊚ (lookupChunk chunkC ⊚ readIORef (wsTilesRef wsL)) `shouldReturn` True
        readmit' logger wsL freshChunkC
        sort ⊚ query env (tagged (tg "saved"))
            `shouldReturn` sort [ TargetTile (cell pageP farTile 10), occ pageP farOak
                                , plot pageP plotTile ]

    it "the connected lifecycle: assign, evict, save while absent, restore, \
       \readmit — every kind excluded while absent and queryable again" $ \env → do
        (ws, _) ← resetScene env
        (logger, _) ← capturingLogger
        -- The row crop's planting is in the page's edit log, so the save
        -- carries it and the readmission below rebuilds the crop from the
        -- RESTORED log, not from anything this test supplies.
        writeIORef (wsEditsRef ws) (HM.singleton chunkC [plantFarRow])
        writeIORef (wsPlantedFloraCursorRef ws) 12
        let targets = [ TargetTile (cell pageP farTile 10), occ pageP farOak
                      , occ pageP farRow, plot pageP farPlotTile ]
        forM_ targets $ \t → tag env ws t ["life"]
        sort ⊚ query env (tagged (tg "life")) `shouldReturn` sort targets
        evicted ← evictA ws
        evicted `shouldSatisfy` (chunkC ∈)
        query env (tagged (tg "life")) `shouldReturn` []
        agrees env (tagged (tg "life"))
        snap ← capturedSession env ⌦ throughCodec
        let pt = pgsPageTags (pageOf snap pageP)
        Map.keysSet (ptsOccurrences pt) `shouldBe` Set.fromList [farOak, farRow]
        loadSession env logger snap
        live ← readIORef (worldManagerRef env)
        wsL ← maybe (fail "no page") pure (lookup pageP (wmWorlds live))
        isNothing ⊚ (lookupChunk chunkC ⊚ readIORef (wsTilesRef wsL)) `shouldReturn` True
        query env (tagged (tg "life")) `shouldReturn` []
        restored ← readIORef (wsEditsRef wsL)
        map show (HM.lookupDefault [] chunkC restored) `shouldBe` [show plantFarRow]
        readmit logger wsL (replayEdits restored regeneratedChunkC)
        sort ⊚ query env (tagged (tg "life")) `shouldReturn` sort targets
        agrees env (tagged (tg "life"))

    it "a plot assignment naming no restored plot is a diagnostic and is \
       \dropped" $ \env → do
        _ ← resetScene env
        snap0 ← capturedSession env
        let snap = snap0 { snapPages = HM.adjust
                (\p → p { pgsPageTags = (pgsPageTags p)
                            { ptsPlots = Map.singleton (1, 1) (tags ["ghost"]) } })
                pageP (snapPages snap0) }
        [ ieRefValue e | e ← sessionIntegrityWarnings snap, ieCode e ≡ "dangling-reference"
                       , "plot-tags" `T.isInfixOf` iePath e ]
            `shouldBe` ["(1,1)"]
        (logger, entries) ← capturingLogger
        loadSession env logger snap
        msgs ← map leMessage ⊚ entries
        msgs `shouldSatisfy` any ("crop plot tile(s) that hold no plot" `T.isInfixOf`)

    it "a pre-publication staging failure keeps the outgoing assignments" $
        \env → do
            (ws, _) ← resetScene env
            tag env ws (TargetTile (cell pageP colTile 10)) ["outgoing"]
            before ← readIORef (wsPageTagsRef ws)
            snap ← capturedSession env
            -- A plot naming a species this build does not know: the load
            -- is refused before anything is published.
            let broken = snap { snapPages = HM.adjust
                    (\p → p { pgsCropPlots = HM.map (\cp → cp { cpSpecies = FloraByName "no_such_crop" })
                                                     (pgsCropPlots p) })
                    pageP (snapPages snap) }
            (logger, _) ← capturingLogger
            matReg ← readIORef (materialRegistryRef env)
            r ← stageSession env logger (snapshotToSaveData meta broken) matReg
            either (const True) (const False) r `shouldBe` True
            readIORef (wsPageTagsRef ws) `shouldReturn` before

    it "a world-edits v4 payload decodes with no tile, plant or plot tags" $ \_ → do
        let v4 = WorldEditsDTOv4 [PageEditsDTOv4 pageP HM.empty 1]
        case ccDecode worldEditsCodec 4 (S.encode v4) of
            Left e → expectationFailure (show e)
            Right (WorldEditsDTO slices) →
                [ (pedTileTags sl, pedOccurrenceTags sl, pedOccurrenceTiles sl, pedPlotTags sl)
                | sl ← slices ] `shouldBe` [([], [], [], [])]
  where
    meta = SaveRequestMeta "tags" "t" False

-- * Session helpers

pageOf ∷ SessionSnapshot → WorldPageId → PageSnapshot
pageOf snap pid = fromMaybe (error "no page") (HM.lookup pid (snapPages snap))

-- | The session as the save barrier captures it: arena-shaped pages (so
--   staging rebuilds flat chunks instead of generating terrain) carrying
--   each page's tags through the SAME 'capturePageTags' the save uses,
--   and its plots by name.
capturedSession ∷ EngineEnv → IO SessionSnapshot
capturedSession env = do
    wm ← readIORef (worldManagerRef env)
    pages ← forM [pageP, pageQ] $ \pid → do
        ws ← maybe (fail "no page") pure (lookup pid (wmWorlds wm))
        pt ← readIORef (wsPageTagsRef ws)
        store ← readIORef (wsCropPlotsRef ws)
        cursor ← readIORef (wsPlantedFloraCursorRef ws)
        -- The edit log through the save's own naming step.
        (edits, _, _) ← readIORef (wsEditsRef ws) ⌦ \es →
            either (\e → fail ("naming refused: " <> show e)) pure
                (nameFloraReferences catalog pid es HM.empty HM.empty)
        pure (blankPageSnapshot pid (withVolcanoCtx arenaParams))
            { pgsPageTags = capturePageTags pt store
            , pgsEdits = edits
            , pgsPlantedFloraCursor = cursor
            , pgsCropPlots = HM.map (\cp → cp { cpSpecies = FloraByName "res_wheat" })
                                    (cpsPlots store)
            , pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid)
            , pgsDateYear = 1, pgsDateMonth = 1, pgsDateDay = 1 }
    either (\e → fail ("capture refused: " <> show e)) pure $
        captureSessionSnapshot SessionGlobals
            { sgGameTime = 0, sgTexPalette = emptyTexPalette, sgNextItemId = 1
            , sgNextBuildingId = 1, sgNextUnitId = 1, sgActivePage = pageP
            , sgVisiblePages = [pageP]
            , sgLiveCamera = LiveCameraSnapshot (Just pageP) 0 0 1 FaceSouth
            , sgItemTags = mempty, sgItemRefs = mempty
            , sgPortableKnowledge = emptyPortableKnowledge }
            pages

arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = worldSize }

throughCodec ∷ SessionSnapshot → IO SessionSnapshot
throughCodec snap = do
    let meta  = snapshotSaveMetadata (SaveRequestMeta "tags" "t" False) snap
        bytes = encodeSessionSnapshot meta snap []
    case decodeSessionEnvelope HS.empty HS.empty bytes of
        Left err → fail ("decode: " <> T.unpack err)
        Right (_, back, _, _) → pure back

loadSession ∷ EngineEnv → LoggerState → SessionSnapshot → IO ()
loadSession env logger snap = do
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger
        (snapshotToSaveData (SaveRequestMeta "tags" "t" False) snap) matReg
        ⌦ either (\e → fail ("stage: " <> T.unpack (renderStageError e))) pure
    publishStagedSession env logger 424242 staged
