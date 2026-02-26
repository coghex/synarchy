{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Lake
    ( computeChunkLakes
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import World.Base
import World.Types
import World.Plate (TectonicPlate(..), elevationAtGlobal)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal
import World.Hydrology.Types (HydroFeature(..), LakeParams(..))

-----------------------------------------------------------
-- Chunk-Level Lake Computation
-----------------------------------------------------------

computeChunkLakes ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                  → Int → ChunkCoord
                  → VU.Vector Int
                  → FluidMap
computeChunkLakes features seed plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyLakes = filter (isNearbyLake worldSize chunkMinGX chunkMinGY) features
    in withFluidMap $ \mv →
        forM_ nearbyLakes $ \pf →
            fillLakeFromFeature mv pf seed plates worldSize chunkMinGX chunkMinGY surfaceMap

-----------------------------------------------------------
-- Lake Proximity
-----------------------------------------------------------

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
        maxR = lkRadius lk
        (dxi, dyi) = wrappedDeltaUVFluid worldSize chunkGX chunkGY fx fy
        dx = abs dxi
        dy = abs dyi
    in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

-----------------------------------------------------------
-- Lake Fill
-----------------------------------------------------------

fillLakeFromFeature ∷ MV.MVector s (Maybe FluidCell)
                    → PersistentFeature → Word64 → [TectonicPlate]
                    → Int → Int → Int
                    → VU.Vector Int
                    → ST s ()
fillLakeFromFeature mv pf seed plates worldSize chunkGX chunkGY surfaceMap =
    case pfFeature pf of
        HydroShape (LakeFeature lk) →
            let GeoCoord fx fy = lkCenter lk
                poolRadius = lkRadius lk
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lakeSurface = baseElev + lkSurface lk
            in fillLakePool mv seed plates worldSize chunkGX chunkGY
                   fx fy poolRadius lakeSurface surfaceMap
        _ → pure ()

fillLakePool ∷ MV.MVector s (Maybe FluidCell)
             → Word64 → [TectonicPlate] → Int → Int → Int
             → Int → Int → Int → Int
             → VU.Vector Int
             → ST s ()
fillLakePool mv seed plates worldSize chunkGX chunkGY fx fy poolRadius lakeSurface surfaceMap =
    let pr = fromIntegral poolRadius ∷ Float
        rimSamples = 32 ∷ Int

        spillway = foldl' (\minElev i →
            let angle = fromIntegral i * 2.0 * π / fromIntegral rimSamples
                rimGX = fx + round (pr * cos angle)
                rimGY = fy + round (pr * sin angle)
                rimLX = rimGX - chunkGX
                rimLY = rimGY - chunkGY
                rimElev =
                    if rimLX ≥ 0 ∧ rimLX < chunkSize ∧ rimLY ≥ 0 ∧ rimLY < chunkSize
                    then surfaceMap VU.! columnIndex rimLX rimLY
                    else
                        let (e, _) = elevationAtGlobal seed plates worldSize rimGX rimGY
                        in e
            in min minElev rimElev
            ) lakeSurface [0 .. rimSamples - 1]

        clampedSurface = min lakeSurface spillway

    in if clampedSurface ≤ seaLevel
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                (dxi, dyi) = wrappedDeltaUVFluid worldSize gx gy fx fy
                dx = fromIntegral dxi ∷ Float
                dy = fromIntegral dyi ∷ Float
                dist = sqrt (dx * dx + dy * dy)
            in when (dist < pr ∧ surfZ < clampedSurface) $
                MV.write mv idx (Just (FluidCell Lake clampedSurface))
