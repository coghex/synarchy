{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Gameplay tags on TERRAIN CELLS and PLANTS (#2705, SCN-05 of epic
--   #2698): the page-owned state, and every pure transition over it.
--
--   == Owners
--
--   Following #2701's pattern — the state that owns the objects holds a
--   'CategoryIndex' — each page owns two stores:
--
--     * 'PageTags', in 'World.State.Types.wsPageTagsRef': the tile index
--       (canonical @(x, y, z)@ cells) and the flora-OCCURRENCE index
--       (wild plants and planted row crops, by 'FloraInstanceId'), with
--       the canonical tile each tagged occurrence stands on. Like the
--       chop designations it sits beside, it is WORLD-THREAD state:
--       every writer — a tag edit (through
--       'World.Command.Types.WorldEditPageTags'), an edit's destruction
--       cleanup, admission reconciliation and the load — runs on the
--       world thread, which is also the only writer of the tile store
--       the targets live in. That is the synchronization boundary: a
--       check against the tile store and the write that depends on it
--       can never be separated by a tile change.
--     * 'CropPlotStore', in 'World.State.Types.wsCropPlotsRef': the
--       page's groundcover plots AND their tag index in ONE ref. Plots
--       are written from both the Lua and the world thread, so a plot
--       tag write and a plot removal each take one atomic step of that
--       ref, and a plot's tags can never outlive it or reach its
--       replacement.
--
--   Nothing is added to a cell, a flora instance or a plot, and nothing
--   to 'Engine.Core.State.EngineEnv'. Keys carry the page, so equal
--   coordinates or occurrence counters on another page stay
--   independent.
--
--   == Residency is not lifetime
--
--   Eviction drops chunks, never assignments: both stores are page
--   state, not chunk data. A target is LIVE — editable, inspectable, in
--   a query universe — only while it is resident: a stored, non-air
--   cell of a resident chunk; an occurrence in a resident chunk; a plot
--   whose chunk is resident. Assignments of non-resident targets are
--   kept and simply invisible. Destruction is decided only by what
--   actually removed a target ('forgetDestroyedCells',
--   'forgetOccurrences', the plot removal) or by the admission of the
--   very chunk that owns it ('reconcileAdmittedChunks'); a missing
--   chunk alone is never evidence of destruction.
module Gameplay.Tags.Page
    ( -- * Page-owned state
      PageTags(..)
    , emptyPageTags
    , CropPlotStore(..)
    , emptyCropPlotStore
    , plotStoreFromPlots
      -- * Residency
    , CellStatus(..)
    , cellStatus
    , residentCells
    , occurrenceTile
    , residentOccurrences
    , residentPlots
    , plotResident
      -- * Edits (pure; see the module haddock for which thread)
    , editTileTags
    , editOccurrenceTags
    , editPlotTags
      -- * Destruction and reconciliation
    , forgetDestroyedCells
    , forgetOccurrences
    , reconcileAdmittedChunks
    , removePlot
    , insertPlot
      -- * Persistence
    , PageTagsSnapshot(..)
    , emptyPageTagsSnapshot
    , capturePageTags
    , RestoredPageTags(..)
    , restorePageTags
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Gameplay.Tags.Index
import Gameplay.Tags.Edit (TagEdit, TagEditResult(..), applyTagEdit)
import Gameplay.Tags.Types
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize, columnIndex)
import World.Flora.CropPlot (CropPlot, CropPlots)
import World.Flora.Identity (FloraInstanceId)
import World.Flora.Types (FloraChunkData(..), FloraInstance(..))
import World.Generate.Coordinates (globalToChunk)
import World.Page.Types (WorldPageId)
import World.Tile.Types (WorldTileData(..), lookupChunk)

-- * Page-owned state

-- | One page's terrain-cell and flora-occurrence assignments.
data PageTags = PageTags
    { ptTiles ∷ !(CategoryIndex TileTarget)
    , ptOccurrences ∷ !(CategoryIndex PlantTarget)
      -- ^ 'PlantOccurrence' targets only; plots live with their plots.
    , ptOccurrenceTiles ∷ !(Map.Map FloraInstanceId (Int, Int))
      -- ^ The canonical tile of every TAGGED occurrence, recorded when
      --   it was tagged (a plant never moves). It is what bounds an
      --   admission's reconciliation by chunk ownership — the id itself
      --   says nothing about where the plant stands — exactly as a chop
      --   designation records its tile. Exactly the occurrence index's
      --   keys.
    } deriving (Show, Eq)

emptyPageTags ∷ PageTags
emptyPageTags = PageTags emptyCategoryIndex emptyCategoryIndex Map.empty

-- | A page's groundcover plots and their tags, together (see the module
--   haddock for why one ref).
data CropPlotStore = CropPlotStore
    { cpsPlots ∷ !CropPlots
    , cpsTags  ∷ !(CategoryIndex PlantTarget)
      -- ^ 'PlantPlot' targets only, every key a plot in 'cpsPlots'.
    } deriving (Show, Eq)

emptyCropPlotStore ∷ CropPlotStore
emptyCropPlotStore = CropPlotStore HM.empty emptyCategoryIndex

-- | Plots with no tags (a fresh or restored-without-tags page).
plotStoreFromPlots ∷ CropPlots → CropPlotStore
plotStoreFromPlots ps = CropPlotStore ps emptyCategoryIndex

-- * Residency

-- | What a canonical cell is right now.
data CellStatus
    = CellUnloaded      -- ^ its chunk is not resident
    | CellUnstored      -- ^ outside its column's stored z range
    | CellAir           -- ^ stored, but air
    | CellSolid         -- ^ a stored, non-air, resident cell: LIVE
    deriving (Show, Eq)

-- | The status of a 'TileTarget''s cell in this tile store. The target
--   is already in the stored (canonical) frame, so its chunk is the
--   store's own key. Never generates or loads anything.
cellStatus ∷ WorldTileData → TileTarget → CellStatus
cellStatus td t =
    let (coord, (lx, ly)) = globalToChunk (tileTargetX t) (tileTargetY t)
    in case lookupChunk coord td of
        Nothing → CellUnloaded
        Just lc → columnStatus (lcTiles lc V.! columnIndex lx ly) (tileTargetZ t)

columnStatus ∷ ColumnTiles → Int → CellStatus
columnStatus col z =
    let i = z - ctStartZ col
    in if i < 0 ∨ i ≥ VU.length (ctMats col) then CellUnstored
       else if ctMats col VU.! i ≡ 0 then CellAir
       else CellSolid

-- | Every live cell of every resident chunk: the tile query universe.
--   Underground cells count; air, unstored depths and absent chunks do
--   not. Built from the store as read, never by generating anything.
residentCells ∷ WorldPageId → WorldTileData → Set.Set TileTarget
residentCells page td = Set.fromList
    [ tileTargetFromStored page gx gy (ctStartZ col + i)
    | lc ← HM.elems (wtdChunks td)
    , let ChunkCoord cx cy = lcCoord lc
    , ly ← [0 .. chunkSize - 1], lx ← [0 .. chunkSize - 1]
    , let col = lcTiles lc V.! columnIndex lx ly
          gx  = cx * chunkSize + lx
          gy  = cy * chunkSize + ly
    , (i, m) ← zip [0 ..] (VU.toList (ctMats col))
    , m ≢ 0 ]

-- | The canonical tile of a resident occurrence, if it is resident.
occurrenceTile ∷ WorldTileData → FloraInstanceId → Maybe (Int, Int)
occurrenceTile td fid = listToMaybe
    [ (cx * chunkSize + fromIntegral (fiTileX fi), cy * chunkSize + fromIntegral (fiTileY fi))
    | lc ← HM.elems (wtdChunks td)
    , let ChunkCoord cx cy = lcCoord lc
    , fi ← fcdInstances (lcFlora lc)
    , fiInstanceId fi ≡ fid ]

-- | Every real occurrence in a resident chunk (the reserved non-identity
--   never appears in chunk data, and is skipped regardless).
residentOccurrences ∷ WorldPageId → WorldTileData → Set.Set PlantTarget
residentOccurrences page td = Set.fromList
    [ t
    | lc ← HM.elems (wtdChunks td)
    , fi ← fcdInstances (lcFlora lc)
    , Just t ← [mkPlantTarget page (fiInstanceId fi)] ]

-- | Is the plot at this canonical tile on a resident chunk?
plotResident ∷ WorldTileData → (Int, Int) → Bool
plotResident td (gx, gy) = isJust (lookupChunk (fst (globalToChunk gx gy)) td)

-- | Every plot whose chunk is resident.
residentPlots ∷ WorldPageId → WorldTileData → CropPlotStore → Set.Set PlantTarget
residentPlots page td store = Set.fromList
    [ plotTargetFromStored page gx gy
    | (gx, gy) ← HM.keys (cpsPlots store), plotResident td (gx, gy) ]

-- * Edits

-- | A cell edit: the target must be a stored, non-air cell of a
--   resident chunk; anything else answers 'TagEditNoTarget' and changes
--   nothing (no assignment, no terrain, no load request).
editTileTags ∷ WorldTileData → TileTarget → TagEdit → PageTags
             → (PageTags, TagEditResult)
editTileTags td t edit pt
    | cellStatus td t ≢ CellSolid = (pt, TagEditNoTarget)
    | otherwise = ( pt { ptTiles = modifyCategoryTags (applyTagEdit edit) t (ptTiles pt) }
                  , TagEditApplied )

-- | An occurrence edit: the occurrence must be in a resident chunk. Its
--   tile is recorded while it has tags and dropped with its last one.
editOccurrenceTags ∷ WorldTileData → PlantTarget → TagEdit → PageTags
                   → (PageTags, TagEditResult)
editOccurrenceTags td t edit pt = case plantTargetId t of
  Nothing → (pt, TagEditNoTarget)
  Just fid → case occurrenceTile td fid of
    Just tile →
        let ix = modifyCategoryTags (applyTagEdit edit) t (ptOccurrences pt)
            tiles = if Set.null (categoryTagsOf t ix)
                        then Map.delete fid (ptOccurrenceTiles pt)
                        else Map.insert fid tile (ptOccurrenceTiles pt)
        in (pt { ptOccurrences = ix, ptOccurrenceTiles = tiles }, TagEditApplied)
    Nothing → (pt, TagEditNoTarget)

-- | A plot edit: the plot must exist in the store and stand on a
--   resident chunk. One pure step of the store, so the caller applies it
--   with one atomic modify.
editPlotTags ∷ WorldTileData → PlantTarget → TagEdit → CropPlotStore
             → (CropPlotStore, TagEditResult)
editPlotTags td t edit store = case plantTargetPlotTile t of
    Just tile
        | HM.member tile (cpsPlots store), plotResident td tile →
            ( store { cpsTags = modifyCategoryTags (applyTagEdit edit) t (cpsTags store) }
            , TagEditApplied )
    _ → (store, TagEditNoTarget)

-- * Destruction and reconciliation

-- | After a chunk changed IN PLACE: every tagged cell this chunk owns
--   that is no longer a stored, non-air cell of it lost its object, so
--   its memberships go from both indexes. Cells that survived keep their
--   tags through material, slope or vegetation changes; a cell exposed
--   by digging above it is its own key and is untouched. Answers what it
--   removed.
forgetDestroyedCells ∷ LoadedChunk → PageTags → (PageTags, [TileTarget])
forgetDestroyedCells lc pt =
    let doomed = [ t | t ← Set.toList (categoryTargets (ptTiles pt))
                     , ownsCell lc t
                     , cellStatus (singleChunk lc) t ≢ CellSolid ]
    in (dropTiles doomed pt, doomed)

-- | Occurrences that actually ceased to exist: their memberships and
--   recorded tiles go. Co-tenants are separate ids and untouched.
forgetOccurrences ∷ [FloraInstanceId] → PageTags → PageTags
forgetOccurrences fids pt
    | null fids = pt
    | otherwise =
        let gone = Set.fromList fids
            keep t = maybe True (not . (`Set.member` gone)) (plantTargetId t)
        in pt { ptOccurrences = restrictCategoryIndex keep (ptOccurrences pt)
              , ptOccurrenceTiles = Map.withoutKeys (ptOccurrenceTiles pt) gone }

-- | An ADMISSION (a regenerated chunk, its edits replayed and its flora
--   hydrated) is the one point where an evicted target's existence can
--   be checked again. Bounded by chunk ownership: only assignments whose
--   cell, or whose occurrence's recorded tile, the admitted chunk owns
--   are examined; everything owned by a non-resident chunk is left
--   alone. A cell that is not stored and non-air, or an occurrence the
--   chunk no longer holds, is destroyed and forgotten. Answers what it
--   removed, for the log.
reconcileAdmittedChunks ∷ [LoadedChunk] → PageTags
                        → (PageTags, [TileTarget], [FloraInstanceId])
reconcileAdmittedChunks admitted pt0 = foldl' step (pt0, [], []) admitted
  where
    step (pt, ts, fs) lc =
        let (pt1, deadCells) = forgetDestroyedCells lc pt
            present = Set.fromList [ fiInstanceId fi | fi ← fcdInstances (lcFlora lc) ]
            deadOcc = [ fid | (fid, tile) ← Map.toList (ptOccurrenceTiles pt1)
                            , ownsTile lc tile
                            , not (Set.member fid present) ]
        in (forgetOccurrences deadOcc pt1, ts ⧺ deadCells, fs ⧺ deadOcc)

-- | Remove a plot and its tags in one step: the production plot removal
--   (an annual harvest) is that plot's destruction.
removePlot ∷ (Int, Int) → CropPlotStore → CropPlotStore
removePlot tile store = CropPlotStore
    { cpsPlots = HM.delete tile (cpsPlots store)
    , cpsTags = restrictCategoryIndex ((≢ Just tile) . plantTargetPlotTile) (cpsTags store) }

-- | Plant a plot. A new plot starts untagged: anything still keyed by
--   its tile belonged to an earlier plot and is dropped in the same step.
insertPlot ∷ (Int, Int) → CropPlot → CropPlotStore → CropPlotStore
insertPlot tile cp store =
    let cleared = removePlot tile store
    in cleared { cpsPlots = HM.insert tile cp (cpsPlots cleared) }

-- * Internals

dropTiles ∷ [TileTarget] → PageTags → PageTags
dropTiles [] pt = pt
dropTiles ts pt =
    let gone = Set.fromList ts
    in pt { ptTiles = restrictCategoryIndex (not . (`Set.member` gone)) (ptTiles pt) }

ownsCell ∷ LoadedChunk → TileTarget → Bool
ownsCell lc t = ownsTile lc (tileTargetX t, tileTargetY t)

ownsTile ∷ LoadedChunk → (Int, Int) → Bool
ownsTile lc (gx, gy) = fst (globalToChunk gx gy) ≡ lcCoord lc

singleChunk ∷ LoadedChunk → WorldTileData
singleChunk lc = WorldTileData { wtdChunks = HM.singleton (lcCoord lc) lc, wtdMaxChunks = 1 }

-- * Persistence

-- | One page's assignments as a save carries them (#2705): FORWARD
--   only (every reverse index is rebuilt), keyed without the page,
--   which the page slice holding this names.
data PageTagsSnapshot = PageTagsSnapshot
    { ptsTiles ∷ !(Map.Map (Int, Int, Int) (Set.Set GameplayTag))
      -- ^ canonical cell → tags
    , ptsOccurrences ∷ !(Map.Map FloraInstanceId (Set.Set GameplayTag))
    , ptsOccurrenceTiles ∷ !(Map.Map FloraInstanceId (Int, Int))
      -- ^ the recorded canonical tile of each tagged occurrence
    , ptsPlots ∷ !(Map.Map (Int, Int) (Set.Set GameplayTag))
      -- ^ plot tile → tags
    } deriving (Show, Eq, Generic, Serialize)

emptyPageTagsSnapshot ∷ PageTagsSnapshot
emptyPageTagsSnapshot = PageTagsSnapshot Map.empty Map.empty Map.empty Map.empty

-- | Capture a page's assignments, the tile and occurrence stores and the
--   plot store as read together under the save barrier. Assignments of
--   EVICTED targets are captured exactly like resident ones: residency
--   is not lifetime.
capturePageTags ∷ PageTags → CropPlotStore → PageTagsSnapshot
capturePageTags pt store = PageTagsSnapshot
    { ptsTiles = Map.fromList
        [ ((tileTargetX t, tileTargetY t, tileTargetZ t), ts)
        | (t, ts) ← Map.toList (categoryForward (ptTiles pt)) ]
    , ptsOccurrences = Map.fromList
        [ (fid, ts)
        | (t, ts) ← Map.toList (categoryForward (ptOccurrences pt))
        , Just fid ← [plantTargetId t] ]
    , ptsOccurrenceTiles = ptOccurrenceTiles pt
    , ptsPlots = Map.fromList
        [ (tile, ts)
        | (t, ts) ← Map.toList (categoryForward (cpsTags store))
        , Just tile ← [plantTargetPlotTile t] ]
    }

-- | What a restore installs, and what it dropped.
data RestoredPageTags = RestoredPageTags
    { rptPageTags ∷ !PageTags
      -- ^ reverse indexes rebuilt from the forward assignments
    , rptPlotTags ∷ !(CategoryIndex PlantTarget)
    , rptDroppedPlots ∷ ![(Int, Int)]
      -- ^ plot assignments naming no plot the page restored: a tag on
      --   nothing is not restored (plots are page state, so their
      --   existence is known at load, unlike an evicted cell's)
    , rptDroppedOccurrences ∷ ![FloraInstanceId]
      -- ^ occurrence assignments with no recorded tile, which an
      --   admission could never reconcile
    }

-- | Rebuild a page's stores from a snapshot. Cells and occurrences are
--   NOT checked against residency — no chunk is resident during a load,
--   and an evicted target is still a target; the admission of the chunk
--   that owns each one reconciles it.
restorePageTags ∷ WorldPageId → CropPlots → PageTagsSnapshot → RestoredPageTags
restorePageTags page plots snap =
    let tiles = categoryFromAssignments
            [ (tileTargetFromStored page x y z, ts)
            | ((x, y, z), ts) ← Map.toList (ptsTiles snap) ]
        (keptOcc, droppedOcc) = Map.partitionWithKey
            (\fid _ → Map.member fid (ptsOccurrenceTiles snap)) (ptsOccurrences snap)
        occs = categoryFromAssignments
            [ (t, ts) | (fid, ts) ← Map.toList keptOcc, Just t ← [mkPlantTarget page fid] ]
        (keptPlots, droppedPlots) = Map.partitionWithKey
            (\tile _ → HM.member tile plots) (ptsPlots snap)
        plotIx = categoryFromAssignments
            [ (plotTargetFromStored page x y, ts) | ((x, y), ts) ← Map.toList keptPlots ]
    in RestoredPageTags
        { rptPageTags = PageTags
            { ptTiles = tiles
            , ptOccurrences = occs
            , ptOccurrenceTiles = Map.restrictKeys (ptsOccurrenceTiles snap)
                                                  (Map.keysSet keptOcc) }
        , rptPlotTags = plotIx
        , rptDroppedPlots = Map.keys droppedPlots
        , rptDroppedOccurrences = Map.keys droppedOcc
        }
