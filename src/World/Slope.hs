{-# LANGUAGE UnicodeSyntax #-}
-- | Slope computation and rendering support, split (issue #567) into
--   focused submodules under "World.Slope.*":
--
--     * "World.Slope.Compute" — per-tile / per-chunk slope determination.
--     * "World.Slope.Roughness" — probabilistic terrace roughness and the
--       exposed-hard-rock jaggedness rule (#224).
--     * "World.Slope.Recompute" — incremental re-slide of chunk border
--       strips when a neighbouring chunk loads or evicts.
--     * "World.Slope.EdgePatch" — post-load strata extension at chunk
--       boundaries (coastal-erosion cliff-face voids).
--     * "World.Slope.FaceMaps" — procedural slope face-map textures.
--
--   This module re-exports the public API unchanged.
module World.Slope
    ( -- * Slope Computation
      computeChunkSlopes
    , recomputeNeighborSlopes
    , slopeRecomputeAffected
    , wrapChunkCoordU
    , patchEdgeStrata
    , chunkNeighbors
      -- * Slope Face Map Generation
    , SlopeFaceMaps(..)
    , generateSlopeFaceMaps
    , generateSideFaceMapLeft
    , generateSideFaceMapRight
      -- * Slope → Face Map Mapping
    , slopeToFaceMapIndex
      -- * Constants
    , slopeHardnessThreshold
      -- * Internals (exposed for testing)
    , slopeBit
    ) where

import World.Types (wrapChunkCoordU)
import World.Slope.Compute (computeChunkSlopes, chunkNeighbors, slopeBit)
import World.Slope.Roughness (slopeHardnessThreshold)
import World.Slope.Recompute (recomputeNeighborSlopes, slopeRecomputeAffected)
import World.Slope.EdgePatch (patchEdgeStrata)
import World.Slope.FaceMaps
    ( SlopeFaceMaps(..)
    , generateSlopeFaceMaps
    , generateSideFaceMapLeft
    , generateSideFaceMapRight
    , slopeToFaceMapIndex
    )
