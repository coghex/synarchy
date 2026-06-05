{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Thin lava convenience layer. The Magma-overlay system in
--   "World.Magma" owns lava placement now; this module exists to give
--   render / zoom-cache / debug callers a single place to ask "does
--   this chunk have any lava?".
--
--   See "World.Magma.Init.discoverChunkLava" + 'World.Magma.Types.MagmaOverlay'
--   for the placement pipeline. Old per-feature pool / fissure
--   stampers were removed in the lava-v1 phase-3 cleanup.
module World.Fluid.Lava
    ( hasAnyLavaQuick
    , chunkHasLavaQuick
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (LoadedChunk(..), ChunkCoord, chunkSize)
import World.Magma.Types (VolcanoCtx)
import World.Magma.Init (discoverChunkLava)

-- | Cheap predicate: is there any surface lava in this chunk? Equal
--   to @isJust . lcMagma@ — relies on chunk-gen having populated the
--   overlay via 'discoverChunkLava'.
hasAnyLavaQuick ∷ LoadedChunk → Bool
hasAnyLavaQuick = isJust . lcMagma

-- | Zoom-cache predicate: same question as 'hasAnyLavaQuick' but
--   without a 'LoadedChunk'. Computes the overlay using a synthetic
--   flat surface at @avgElev@ — same machinery the real chunk-gen
--   runs, just approximated to one elevation per chunk. Over-flags
--   slightly when the chunk's real surface is higher than a chute's
--   reach but the average dips below it; for a chunk-level color
--   indicator that's fine.
--
--   Since the pool rework, 'discoverChunkLava' emits only basalt
--   caps (underwater breaches); visible surface lava lives in the
--   global 'gtWorldLavaPools' table, which the zoom-cache caller
--   ORs in separately. This predicate therefore flags cap chunks
--   (sub-sea/sub-lake seamounts). The synthetic water map is all
--   'minBound' — at chunk-colour resolution the lake/river cap
--   distinction doesn't matter, sub-sea capping still fires off
--   @avgElev ≤ seaLevel@.
chunkHasLavaQuick ∷ VolcanoCtx → ChunkCoord → Int → Bool
chunkHasLavaQuick ctx coord avgElev =
    isJust $ discoverChunkLava ctx coord
                (VU.replicate (chunkSize * chunkSize) avgElev)
                (VU.replicate (chunkSize * chunkSize) minBound)
