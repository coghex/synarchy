{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope.Recompute
    ( slopeRecomputeAffected
    , recomputeNeighborSlopes
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Types
import World.Material (MaterialRegistry)
import World.Slope.Compute (computeChunkSlopesCols, chunkNeighbors)

-- * Recompute Neighbor Slopes

-- | How many tiles in from a chunk edge a column's slope can depend on
--   a NEIGHBOURING chunk. The slope rule reads the immediate 4-neighbour
--   (N/E/S/W) — see 'World.Slope.Compute.computeTileSlope' — so radius 1:
--   only the 1-tile border of a chunk ever reads across a chunk boundary;
--   interior columns read in-chunk data only. 'recomputeNeighborSlopes' uses
--   this to re-slide just the boundary strip when a neighbour loads. Bump it
--   if the slope rule ever grows a wider horizontal stencil (e.g. future
--   underground slopes); the perimeter strip widens to match and the
--   recompute stays correct.
slopeStencilRadius ∷ Int
slopeStencilRadius = 1

-- | Column indices within 'slopeStencilRadius' of any chunk edge — the
--   only columns whose slope can change when a neighbour loads. Computed
--   once (constant for a given radius / chunk size).
perimeterColumnSet ∷ HS.HashSet Int
perimeterColumnSet = HS.fromList
    [ ly * chunkSize + lx
    | ly ← [0 .. chunkSize - 1]
    , lx ← [0 .. chunkSize - 1]
    , let r = slopeStencilRadius
    , lx < r ∨ lx ≥ chunkSize - r ∨ ly < r ∨ ly ≥ chunkSize - r
    ]

-- | The loaded chunks whose border slopes 'recomputeNeighborSlopes' will
--   rewrite for the given changed coords — the changed chunks themselves
--   plus their loaded (seam-wrapped) neighbours. Exposed so a caller can
--   re-apply, over EXACTLY this set, any pass the recompute clobbers
--   (notably the mid-dig slope masks restored by
--   'World.Mine.Apply.applyDigSlopesTd'); keying that restore off a
--   narrower set would silently drop overrides on evicted-neighbour or
--   wrapped-seam-neighbour tiles.
slopeRecomputeAffected ∷ Int → [ChunkCoord] → WorldTileData → [ChunkCoord]
slopeRecomputeAffected worldSize changedCoords wtd =
    let chunks = wtdChunks wtd
        wrap = wrapChunkCoordU worldSize
    in HS.toList $ HS.fromList $
        [ c | c ← changedCoords, HM.member c chunks ] <>
        [ nb
        | chg ← changedCoords
        , raw ← chunkNeighbors chg
        , let nb = wrap raw
        , HM.member nb chunks
        ]

recomputeNeighborSlopes ∷ Word64 → Int → MaterialRegistry
                        → [ChunkCoord]
                        → WorldTileData
                        → WorldTileData
recomputeNeighborSlopes seed worldSize registry changedCoords wtd =
    let chunks = wtdChunks wtd
        wrap = wrapChunkCoordU worldSize
        -- 'changedCoords' are chunks whose presence just changed — loaded
        -- OR evicted. A loaded chunk's own border strip plus its loaded
        -- neighbours' strips may need re-sloping; an evicted chunk's
        -- former neighbours must re-slope to drop the slope that pointed
        -- at it (so the surface depends only on the currently loaded set,
        -- not load order). Cross-SEAM neighbours live under a wrapped
        -- coord, so wrap before testing membership and before lookup.
        affected = slopeRecomputeAffected worldSize changedCoords wtd
        neighborLookup coord = case HM.lookup (wrap coord) chunks of
            Just lc → Just (lcTerrainSurfaceMap lc)
            Nothing → Nothing
        -- Parallel fluid lookup so the dry-rock bank rule (jagged path)
        -- can see a wet neighbour ACROSS a chunk seam, not just in-chunk.
        fluidNeighborLookup coord = case HM.lookup (wrap coord) chunks of
            Just lc → Just (lcFluidMap lc)
            Nothing → Nothing
        -- Only the boundary strip can change when a neighbour loads —
        -- interior columns read in-chunk data only, so recomputing them
        -- would reproduce their stored value. Restricting to the
        -- perimeter is output-identical but skips ~the chunk interior.
        updatedChunks = foldl' (\acc coord →
            case HM.lookup coord acc of
                Just lc →
                    let newTiles = computeChunkSlopesCols seed coord
                            (lcTerrainSurfaceMap lc) registry
                            (lcFluidMap lc) (lcTiles lc) neighborLookup
                            fluidNeighborLookup perimeterColumnSet
                    in HM.insert coord (lc { lcTiles = newTiles }) acc
                Nothing → acc
            ) chunks affected
    in wtd { wtdChunks = updatedChunks }
