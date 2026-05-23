{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Lake-related helpers for zoom-map rendering.
--
--   The chunk-level lake fluid placement that used to live here has
--   been replaced by the water-table-driven pipeline in
--   `World.Generate.Chunk` — lakes now emerge automatically from the
--   sink-fill step in `World.Hydrology.WaterTable` (see
--   @src\/World\/Hydrology\/DESIGN.md@). What remains is a fast
--   "is there a lake near this chunk?" predicate used by the zoom
--   cache to draw indicators without computing the full chunk.
module World.Fluid.Lake
    ( hasAnyLakeQuick
    ) where

import UPrelude
import World.Base
import World.Types
import World.Fluid.Internal (wrappedDeltaUVFluid)
import World.Hydrology.Types
import World.Geology.Types

-- | Quick boolean check: does this chunk have any persistent-feature
--   lake near it? Used for zoom-map water indicators. Note that this
--   only sees lakes from the geological simulation's persistent
--   feature list — water-table-fill lakes (where the wt rises into a
--   depression with no explicit feature) are not detected here. Good
--   enough for the zoom-map indicator since the zoom view also pulls
--   fluid placement from `World.Generate.Chunk.generateZoomTerrain`,
--   which uses the full water-table compute.
hasAnyLakeQuick ∷ [PersistentFeature] → Int → ChunkCoord → Bool
hasAnyLakeQuick features worldSize coord =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
    in any (isNearbyLake worldSize chunkMinGX chunkMinGY) features

isNearbyLake ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyLake worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape (LakeFeature lk) →
            case pfActivity pf of
                FActive  → checkLakeRange worldSize chunkGX chunkGY lk
                FDormant → checkLakeRange worldSize chunkGX chunkGY lk
                _        → False
        _ → False

checkLakeRange ∷ Int → Int → Int → LakeParams → Bool
checkLakeRange worldSize chunkGX chunkGY lk =
    let GeoCoord fx fy = lkCenter lk
        maxR = round (fromIntegral (lkRadius lk) * 1.25 ∷ Float)
        (dxi, dyi) = wrappedDeltaUVFluid worldSize chunkGX chunkGY fx fy
        dx = abs dxi
        dy = abs dyi
    in dx < maxR + chunkSize ∧ dy < maxR + chunkSize
