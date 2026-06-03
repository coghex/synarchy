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
--   Note: passes @isOceanic = False@ so the overlay's @moSurface@
--   field captures every chamber breach — we use this purely to ask
--   \"is there ANY chamber/chute touching this chunk's surface?\". The
--   actual cap-vs-lava decision is made later by the per-tile chunk
--   gen pipeline.
chunkHasLavaQuick ∷ VolcanoCtx → ChunkCoord → Int → Bool
chunkHasLavaQuick ctx coord avgElev =
    isJust $ discoverChunkLava ctx coord False
                (VU.replicate (chunkSize * chunkSize) avgElev)
