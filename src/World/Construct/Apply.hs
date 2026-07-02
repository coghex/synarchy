{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Applying construction build progress to loaded chunks (#96) — the
--   mining corner-progress display, reused for building UP instead of
--   digging down.
--
--   A structure designation's build progress derives a 4-corner state
--   ('World.Construct.Types.constructCorners'); the corners map onto
--   the slope-id edge mask and land in ctSlopes through the SAME
--   'World.Mine.Apply.applyCornerSlopeToChunk' the dig display uses,
--   so a tile under construction visibly works corner-by-corner (site
--   prep: vegetation sheds, the ground cuts in) until the piece
--   appears and the tile returns to flat. Three writers:
--
--     * the live progress command
--       ('handleWorldAddConstructProgressCommand') after each delta,
--     * chunk loading ('World.Thread.ChunkLoading') right after the
--       dig-slope re-apply — construction slopes are DERIVED state,
--       lost on chunk eviction/regen, re-applied from the persisted
--       designations, and
--     * the save-load chunk builders ('World.Thread.Command.Save').
--
--   The display only engages on tiles whose slope is FLAT (0) or
--   already showing this designation's own mask: a natural hillside
--   slope or an authored 'world.setSlope' ramp is never stomped, and
--   the complete/cancel reset ('clearConstructSlope') can safely
--   return the tile to flat because a non-flat prior slope means the
--   mask was never applied.
module World.Construct.Apply
    ( applyConstructSlopeToChunk
    , applyConstructSlopes
    , applyConstructSlopesTd
    , clearConstructSlope
    , constructSlopeAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), ChunkCoord(..)
                         , chunkSize, columnIndex)
import World.Construct.Types (ConstructDesignation(..), ConstructTarget(..)
                             , ConstructDesignations, constructCorners)
import World.Mine.Types (digSlopeMask)
import World.Mine.Apply (applyCornerSlopeToChunk)
import World.Tile.Types (WorldTileData(..))

-- | The tile's current slope id at (gx, gy, z), if the position is
--   inside this chunk's strata band.
constructSlopeAt ∷ (Int, Int) → Int → LoadedChunk → Maybe Word8
constructSlopeAt (gx, gy) z lc =
    let ChunkCoord cx cy = lcCoord lc
        lx = gx - cx * chunkSize
        ly = gy - cy * chunkSize
    in if lx < 0 ∨ lx ≥ chunkSize ∨ ly < 0 ∨ ly ≥ chunkSize
       then Nothing
       else
        let col = lcTiles lc V.! columnIndex lx ly
            i = z - ctStartZ col
        in if i < 0 ∨ i ≥ VU.length (ctSlopes col)
           then Nothing
           else Just (ctSlopes col VU.! i)

-- | Only structure designations render corner progress: building
--   designations never accrue progress (they're STAKED into a real
--   building, which has its own construction visuals).
isStructure ∷ ConstructDesignation → Bool
isStructure cd = case cdTarget cd of
    CtStructure _ → True
    CtBuilding  _ → False

-- | Stamp one designation's corner mask, guarded to flat-or-ours
--   tiles. @prevProgress@ is the progress the tile was last stamped
--   with (equal to current progress on the (re)load path, where the
--   guard reduces to "the recomputed natural slope is flat").
applyConstructSlopeToChunk ∷ (Int, Int) → Float → ConstructDesignation
                           → LoadedChunk → LoadedChunk
applyConstructSlopeToChunk (gx, gy) prevProgress cd lc
    | not (isStructure cd) = lc
    | otherwise =
        let prevMask = digSlopeMask (constructCorners prevProgress)
        in case constructSlopeAt (gx, gy) (cdZ cd) lc of
            Just cur | cur ≡ 0 ∨ cur ≡ prevMask →
                applyCornerSlopeToChunk (gx, gy) (cdZ cd)
                    (constructCorners (cdProgress cd)) lc
            _ → lc

-- | Re-apply every progressed structure designation that falls inside
--   this chunk (chunk-load path). Untouched designations derive full
--   corners → mask 0 → the gen-time flat default, so they're skipped.
applyConstructSlopes ∷ ConstructDesignations → LoadedChunk → LoadedChunk
applyConstructSlopes desigs lc =
    let ChunkCoord cx cy = lcCoord lc
        inChunk (gx, gy) =
            gx - cx * chunkSize ≥ 0 ∧ gx - cx * chunkSize < chunkSize
          ∧ gy - cy * chunkSize ≥ 0 ∧ gy - cy * chunkSize < chunkSize
        live = [ (k, cd) | (k, cd) ← HM.toList desigs, inChunk k
               , isStructure cd, cdProgress cd > 0 ]
    in foldl' (\c (k, cd) →
            applyConstructSlopeToChunk k (cdProgress cd) cd c) lc live

-- | Tile-data-level variant for the chunk-load pipeline. MUST run
--   after 'recomputeNeighborSlopes' (which would erase the masks) —
--   same ordering contract as 'World.Mine.Apply.applyDigSlopesTd'.
applyConstructSlopesTd ∷ ConstructDesignations → [ChunkCoord]
                       → WorldTileData → WorldTileData
applyConstructSlopesTd desigs coords td
    | HM.null desigs = td
    | otherwise = foldl' step td coords
  where
    step acc c = case HM.lookup c (wtdChunks acc) of
        Just lc → acc { wtdChunks =
            HM.insert c (applyConstructSlopes desigs lc) (wtdChunks acc) }
        Nothing → acc

-- | Completion / cancellation reset: return the tile to flat, but only
--   when it currently shows this designation's own mask — the guard's
--   counterpart, so an authored ramp or natural slope (where the mask
--   never applied) is left alone. Vegetation/flora shed by the site
--   prep stays gone, mirroring mining.
clearConstructSlope ∷ (Int, Int) → ConstructDesignation → LoadedChunk
                    → LoadedChunk
clearConstructSlope (gx, gy) cd lc
    | not (isStructure cd) = lc
    | otherwise =
        let mask = digSlopeMask (constructCorners (cdProgress cd))
        in case constructSlopeAt (gx, gy) (cdZ cd) lc of
            Just cur | cur ≡ mask ∧ mask ≠ 0 →
                applyCornerSlopeToChunk (gx, gy) (cdZ cd)
                    (1.0, 1.0, 1.0, 1.0) lc
            _ → lc
