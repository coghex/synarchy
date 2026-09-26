{-# LANGUAGE Strict #-}
-- | Minimal river-only bed repair, AFTER historical terrain smoothing.
-- Feeding these additional cuts into the despiker can collapse unrelated
-- lake banks. Old saves explicitly retain the historical generation policy.
module World.Generate.Chunk.RiverBed (fitExactRiverBeds) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Generate.Types (WorldGenParams(..))
import World.Geology.Timeline.Types (GeoTimeline(..))
import World.Fluid.River.Types (RiverChunkEntry(..), riversInChunk)
import World.Fluid.OceanMask (oceanBitInChunk)
import World.Fluid.Exact (exactSurfaceCeilZ, exactSurfaceOfZ)
import World.Ocean.Types (chunkOrNeighborOceanicAt)
import World.Constants (seaLevel)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Generate.Coordinates (chunkToGlobal)

-- | The same bordered map is used by chunk columns and zoom terrain.
-- Unwrapped keys match the carve tables' seam aliases. Non-river tiles and
-- the absent-terrain sentinel are untouched. The actual ocean plane takes
-- priority if the repaired bed would reach sea level in an oceanic column.
fitExactRiverBeds ∷ WorldGenParams → ChunkCoord → Int → VU.Vector Int → VU.Vector Int
fitExactRiverBeds params coord border terrain
    | not (wgpExactRiverBeds params) = terrain
    | otherwise = VU.imap fit terrain
  where
    side = chunkSize + 2 * border
    timeline = wgpGeoTimeline params
    fit i z
        | z ≡ minBound = z
        | otherwise =
            let (gx,gy) = chunkToGlobal coord (i `mod` side - border)
                                               (i `div` side - border)
                cc = ChunkCoord (gx `div` chunkSize) (gy `div` chunkSize)
                li = (gy `mod` chunkSize) * chunkSize + gx `mod` chunkSize
                surfaces = V.map (\e → rcePerTileSurfZ e VU.! li) $
                    V.filter (\e → rceBitmask e VU.! li)
                        (riversInChunk (gtWorldRivers timeline) cc)
            in if V.null surfaces then z else
                let h = V.minimum surfaces
                    bed = min z (exactSurfaceCeilZ h - 1)
                    ocean = oceanBitInChunk (gtWorldOcean timeline) cc li
                        ∨ chunkOrNeighborOceanicAt (wgpWorldSize params) (wgpOceanDist params) cc
                    plane = if ocean ∧ bed ≤ seaLevel
                            then min h (exactSurfaceOfZ seaLevel) else h
                in min bed (exactSurfaceCeilZ plane - 1)
