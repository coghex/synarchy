{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Applying mine-dig state to loaded chunks.
--
--   A partially-dug tile's corner pattern maps onto the slope-id edge
--   mask ('digSlopeMask') and is written into the column's ctSlopes at
--   the designation's z, so the regular tile renderer shows the tile
--   progressively excavating. Two writers use this:
--
--     * the live dig command ('handleWorldDigTileCommand') after each
--       progress application, and
--     * chunk loading ('World.Thread.ChunkLoading'), right after
--       'replayEdits' — dig slopes are DERIVED state, lost on chunk
--       eviction/regen, so fresh chunks re-apply them from the
--       persisted designations the same way edits replay.
module World.Mine.Apply
    ( applyDigSlopeToChunk
    , applyDigSlopes
    , applyDigSlopesTd
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), ChunkCoord(..)
                         , chunkSize, columnIndex)
import World.Flora.Types (FloraChunkData(..), FloraInstance(..))
import World.Mine.Types (MineDesignation(..), MineDesignations, digSlopeMask)
import World.Tile.Types (WorldTileData(..))

-- | Write one designation's dig-slope mask into the chunk (no-op when
--   the tile's z is outside the stored strata band). Once digging has
--   actually started (any corner below full), the tile also sheds its
--   surface vegetation and any flora rooted on it — grass and trees
--   don't survive the ground being cut out from under them. Runs on
--   both the live dig path and the chunk-(re)load re-apply, so the
--   clearing replays deterministically.
applyDigSlopeToChunk ∷ (Int, Int) → MineDesignation → LoadedChunk → LoadedChunk
applyDigSlopeToChunk (gx, gy) md lc =
    let lx = gx - cx * chunkSize
        ly = gy - cy * chunkSize
        ChunkCoord cx cy = lcCoord lc
        idx = columnIndex lx ly
        col = lcTiles lc V.! idx
        i = mdZ md - ctStartZ col
        progressed = mdCorners md ≠ (1.0, 1.0, 1.0, 1.0)
    in if lx < 0 ∨ lx ≥ chunkSize ∨ ly < 0 ∨ ly ≥ chunkSize
        ∨ i < 0 ∨ i ≥ VU.length (ctSlopes col)
       then lc
       else
        let col' = col
                { ctSlopes = ctSlopes col VU.//
                      [(i, digSlopeMask (mdCorners md))]
                , ctVeg = if progressed
                          then ctVeg col VU.// [(i, 0)]
                          else ctVeg col
                }
            flora' = if progressed
                     then FloraChunkData
                         [ fi | fi ← fcdInstances (lcFlora lc)
                              , fromIntegral (fiTileX fi) ≠ lx
                                ∨ fromIntegral (fiTileY fi) ≠ ly ]
                     else lcFlora lc
        in lc { lcTiles = lcTiles lc V.// [(idx, col')]
              , lcFlora = flora' }

-- | Re-apply every designation that falls inside this chunk (chunk
--   load path). Designations with untouched corners produce mask 0 —
--   same as the gen-time flat default for a full tile — so this is a
--   no-op for never-dug designations.
applyDigSlopes ∷ MineDesignations → LoadedChunk → LoadedChunk
applyDigSlopes desigs lc =
    let ChunkCoord cx cy = lcCoord lc
        inChunk (gx, gy) =
            gx - cx * chunkSize ≥ 0 ∧ gx - cx * chunkSize < chunkSize
          ∧ gy - cy * chunkSize ≥ 0 ∧ gy - cy * chunkSize < chunkSize
        mine = [ (k, md) | (k, md) ← HM.toList desigs, inChunk k
               , mdCorners md ≠ (1.0, 1.0, 1.0, 1.0) ]
    in foldl' (\c (k, md) → applyDigSlopeToChunk k md c) lc mine

-- | Tile-data-level variant for the chunk-load pipeline: re-apply dig
--   slopes to the listed (just-inserted) chunks. MUST run after
--   'recomputeNeighborSlopes', which rebuilds slopes from terrain and
--   would otherwise erase mid-dig slopes.
applyDigSlopesTd ∷ MineDesignations → [ChunkCoord] → WorldTileData
                 → WorldTileData
applyDigSlopesTd desigs coords td
    | HM.null desigs = td
    | otherwise = foldl' step td coords
  where
    step acc c = case HM.lookup c (wtdChunks acc) of
        Just lc → acc { wtdChunks =
            HM.insert c (applyDigSlopes desigs lc) (wtdChunks acc) }
        Nothing → acc
