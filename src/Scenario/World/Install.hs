{-# LANGUAGE Strict #-}
-- | Committing constructed world content onto a live page (#2711, SCN-11
--   of epic #2698): the commit that makes a "Scenario.World.Build"
--   result live. It runs ON THE WORLD THREAD, the only writer of the tile
--   store and of the page's tag store ("Gameplay.Tags.Page"), so a check
--   against the tiles and the write that depends on it can never be
--   separated by a tile change; every other thread asks through
--   'requestWorldInstall', which carries the page incarnation it
--   resolved and is answered 'WorldInstallNotAdmitted' (nothing changed)
--   if the page was replaced or destroyed since.
--
--   __Replacement, not accumulation.__ A materialization establishes the
--   page's physical baseline: it REPLACES the terrain, fluid, plant
--   occurrence and crop-plot state the page already holds — in the edit
--   log every entry except the structure pieces ('WeSetStructure' /
--   'WeClearStructure', owned by the structure adapters), which keep
--   their relative order — so a second materialization or a recapture
--   can never accumulate plants, fluid, terrain edits or tags. Nothing
--   else is touched.
--
--   __The one reconstruction.__ A resident chunk is rebuilt exactly as an
--   admission rebuilds it: the arena base from the page's recorded seed
--   and bounds, the (new) edit log replayed over it, the rectangle
--   re-asserted, and the chunk swapped in through
--   'World.Flora.Designation.replaceChunkForgettingFlora' — the owner
--   that forgets the tags, regrowth timers and designations of whatever
--   the swap removed — then re-seeded into the simulation through
--   'World.Thread.Command.Edit.Sync.syncEditToSim', the path every live
--   edit takes (activating the chunk so authored fluid is the world's
--   state, not advanced: nothing here steps the simulation). A chunk
--   that is not resident gets only the log, and is rebuilt by its own
--   admission later.
--
--   __Identity and tags.__ Plant ids come from the page's planted
--   allocator and the advanced cursor is committed with them. Tile tags
--   are recorded for the cells the build proved exist; a tag on a cell
--   that does not survive is reported, never invented, and the page's
--   admission reconciliation drops any membership its chunk does not
--   really hold. Memberships are written under the page lock, together.
module Scenario.World.Install
    ( installWorldContent
    , handleWorldInstallScenarioContentCommand
    , requestWorldInstall
    , replaceOwnedEdits
    , isStructureEdit
    ) where

import UPrelude
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Engine.Core.Log (LoggerState)
import Gameplay.Tags.Index (categoryTargets, modifyCategoryTags, emptyCategoryIndex)
import Gameplay.Tags.Page
import Gameplay.Tags.PageLive (pageTagWrite)
import Gameplay.Tags.Types
    ( mkPlantTarget, plotTargetFromStored, tileTargetFromStored
    , tileTargetX, tileTargetY )
import Scenario.World.Build
import Scenario.World.Types
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration)
import World.Chunk.Types (LoadedChunk(..))
import World.Command.Types (WorldCommand(..))
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Flora.Designation (forgetFloraInstances, replaceChunkForgettingFlora)
import World.Generate.Arena (clipArenaChunk, generateArenaChunkIn)
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types (WorldGenParams(..), isArenaParams)
import World.Material (materialIdByName)
import World.Page.Types (WorldPageId)
import World.State.Types
import World.Thread.Command.Edit.Sync (syncEditToSim)
import World.Tile.Types (WorldTileData(..))
import World.Time.Types (defaultCalendarConfig, worldAbsoluteDay)

-- | Whether an edit belongs to the structure adapters, which a
--   materialization neither drops nor reorders.
isStructureEdit ∷ WorldEdit → Bool
isStructureEdit e = case e of
    WeSetStructure{}   → True
    WeClearStructure{} → True
    _                  → False

-- | The page's edit log with every owned entry replaced by @new@: per
--   chunk the new entries come first, then the retained structure
--   entries in their original relative order; a chunk left with nothing
--   has no key.
replaceOwnedEdits ∷ WorldEdits → WorldEdits → WorldEdits
replaceOwnedEdits old new =
    let retained = HM.map (filter isStructureEdit) old
        merged = HM.unionWith (⧺) new retained
    in HM.filter (not ∘ null) merged

-- | Ask the world thread to materialize @request@ on @page@ and wait
--   for the answer. Must not be called ON the world thread, which would
--   wait for itself.
requestWorldInstall ∷ WorldSimCapability → WorldPageId → ChunkGeneration
                    → ScenarioWorldRequest → IO WorldInstallOutcome
requestWorldInstall wsc page epoch request = do
    reply ← newEmptyMVar
    Q.writeQueue (wsWorldQueue wsc)
        (WorldInstallScenarioContent page epoch request (WorldInstallReply reply))
    takeMVar reply

-- | The world-thread handler: check the incarnation, commit, answer.
handleWorldInstallScenarioContentCommand
    ∷ WorldSimCapability → LoggerState → WorldPageId → ChunkGeneration
    → ScenarioWorldRequest → WorldInstallReply → IO ()
handleWorldInstallScenarioContentCommand wsc logger page epoch request (WorldInstallReply reply) = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    outcome ← case lookup page (wmWorlds mgr) of
        Nothing → pure WorldInstallNotAdmitted
        Just ws → do
            now ← pageIncarnation ws
            mParams ← readIORef (wsGenParamsRef ws)
            if now ≢ epoch
                then pure WorldInstallNotAdmitted
                else if not (maybe False isArenaParams mParams)
                    then pure WorldInstallNotArena
                    else WorldInstalled ⊚ installWorldContent wsc logger page ws request
    putMVar reply outcome

-- | Build and commit. World thread only.
installWorldContent ∷ WorldSimCapability → LoggerState → WorldPageId → WorldState
                    → ScenarioWorldRequest → IO WorldInstallResult
installWorldContent wsc _logger page ws request = do
    mParams ← readIORef (wsGenParamsRef ws)
    registry ← readIORef (wsMaterialRegistryRef wsc)
    floraCat ← readIORef (wsFloraCatalogRef wsc)
    date ← readIORef (wsDateRef ws)
    cursor ← readIORef (wsPlantedFloraCursorRef ws)
    let bounds = mParams ⌦ wgpArenaBounds
        seed = maybe 0 wgpSeed mParams
        calendar = maybe defaultCalendarConfig wgpCalender mParams
        env = WorldBuildEnv
            { wbeBounds = bounds
            , wbeMaterial = materialIdByName registry
            , wbeFlora = floraCat
            , wbeAbsDay = worldAbsoluteDay calendar date
            , wbeCursor = cursor }
        content = buildWorldContent env (swrTerrain request) (swrFluids request)
                                    (swrFlora request)
    oldEdits ← readIORef (wsEditsRef ws)
    let newEdits = replaceOwnedEdits oldEdits (wcEdits content)
        rebuild c = clipArenaChunk bounds (replayEdits newEdits (generateArenaChunkIn bounds seed c))
    -- the durable state first, so a chunk rebuilt below replays the new log
    writeIORef (wsEditsRef ws) newEdits
    atomicModifyIORef' (wsPlantedFloraCursorRef ws) $ \c → (max c (wcCursor content), ())
    atomicModifyIORef' (wsCropPlotsRef ws) $ \_ →
        ( CropPlotStore (HM.fromList (wcCropPlots content)) emptyCategoryIndex, () )
    -- EVERY resident chunk is rebuilt by the same reconstruction an
    -- admission performs, not only the ones the old or new log names: the
    -- simulation's writebacks put fluid into a chunk with no log entry of
    -- its own, and replacing the baseline must clear that too. (A chunk
    -- that is not resident is rebuilt by its own admission.)
    td ← readIORef (wsTilesRef ws)
    let resident = HM.elems (wtdChunks td)
    forM_ resident $ \old → do
        let rebuilt = rebuild (lcCoord old)
        replaceChunkForgettingFlora ws old rebuilt
        syncEditToSim wsc page ws rebuilt
    -- A tagged cell in a chunk that is NOT resident is judged against the
    -- replacement geometry too: one the replacement no longer has loses its
    -- tags now, rather than letting a cell created there later inherit them
    -- before the chunk's own admission could reconcile it. A cell that
    -- survives keeps its tags (D-37).
    pt0 ← readIORef (wsPageTagsRef ws)
    let residentCoords = HS.fromList (HM.keys (wtdChunks td))
        taggedElsewhere = Set.toAscList $ Set.fromList
            [ c | t ← Set.toList (categoryTargets (ptTiles pt0))
                , let c = fst (globalToChunk (tileTargetX t) (tileTargetY t))
                , not (HS.member c residentCoords) ]
    pageTagWrite ws $ atomicModifyIORef' (wsPageTagsRef ws) $ \pt →
        (foldl' (\acc c → fst (forgetDestroyedCells (rebuild c) acc)) pt taggedElsewhere, ())
    -- Every plant the REPLACED content placed is gone, resident or not: its
    -- regrowth timer, tags and designation go with it. A resident chunk's
    -- swap already forgot its own; a plant in a chunk that is not resident
    -- would otherwise leave them behind.
    forgetFloraInstances ws
        [ iid | es ← HM.elems oldEdits, WePlaceFloraWithId _ _ _ _ _ iid ← es ]
    -- regrowth timers: the new plants' timers beside whatever survived
    atomicModifyIORef' (wsFloraHarvestsRef ws) $ \hs →
        (foldl' (\m (iid, secs) → HM.insert iid secs m) hs (wcHarvests content), ())
    installTags ws page content
    bumpQuadCacheGen ws
    writeIORef (wsZoomQuadCacheRef ws) Nothing
    writeIORef (wsBgQuadCacheRef ws) Nothing
    pure WorldInstallResult
        { wirRefs = wcRefs content
        , wirDiagnostics = wcDiagnostics content
        , wirChunks = map lcCoord resident }

-- | Record the content's memberships, under the page lock, together.
installTags ∷ WorldState → WorldPageId → WorldContent → IO ()
installTags ws page content = pageTagWrite ws $ do
    atomicModifyIORef' (wsPageTagsRef ws) $ \pt →
        let tiles = foldl'
                (\ix ((x, y, z), tags) →
                    modifyCategoryTags (applyTagEdit' tags)
                        (tileTargetFromStored page x y z) ix)
                (ptTiles pt) (wcTileTags content)
            (occ, occTiles) = foldl'
                (\(ix, m) (iid, tile, tags) → case mkPlantTarget page iid of
                    Nothing → (ix, m)
                    Just t → ( modifyCategoryTags (applyTagEdit' tags) t ix
                             , Map.insert iid tile m ))
                (ptOccurrences pt, ptOccurrenceTiles pt) (wcOccurrenceTags content)
        in (pt { ptTiles = tiles, ptOccurrences = occ, ptOccurrenceTiles = occTiles }, ())
    atomicModifyIORef' (wsCropPlotsRef ws) $ \store →
        ( store { cpsTags = foldl'
                    (\ix ((x, y), tags) →
                        modifyCategoryTags (applyTagEdit' tags)
                            (plotTargetFromStored page x y) ix)
                    (cpsTags store) (wcPlotTags content) }
        , () )
  where applyTagEdit' tags = Set.union tags
