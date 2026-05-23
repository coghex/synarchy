{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Lava
    ( computeChunkLava
    , hasAnyLavaQuick
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import World.Base
import World.Types
import World.Plate (TectonicPlate(..), elevationAtGlobal)
import World.Geology.Evolution (getFeatureCenter, getFeatureRadius)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal

-- * Chunk-Level Lava Computation

computeChunkLava ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                 → Int → ChunkCoord
                 → VU.Vector Int
                 → FluidMap
computeChunkLava features seed plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyActive = filter (isNearbyActive worldSize chunkMinGX chunkMinGY) features
    in withFluidMap $ \mv →
        forM_ nearbyActive $ \pf →
            fillLavaFromFeature mv pf seed plates worldSize chunkMinGX chunkMinGY surfaceMap

-- * Lava Proximity

isNearbyActive ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyActive worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape _ → False
        VolcanicShape _ → case pfActivity pf of
            FActive    → checkRange
            FCollapsed → checkRange
            _          → False
  where
    checkRange = let maxR = getFeatureRadius (pfFeature pf)
                     GeoCoord fx fy = getFeatureCenter (pfFeature pf)
                     (dxi, dyi) = wrappedDeltaUVFluid worldSize chunkGX chunkGY fx fy
                     dx = abs dxi
                     dy = abs dyi
                 in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

-- * Lava Fill

fillLavaFromFeature ∷ MV.MVector s (Maybe FluidCell)
                    → PersistentFeature → Word64 → [TectonicPlate]
                    → Int → Int → Int
                    → VU.Vector Int
                    → ST s ()
fillLavaFromFeature mv pf seed plates worldSize chunkGX chunkGY surfaceMap =
    case (pfFeature pf) of
        VolcanicShape (SuperVolcano p) →
            let (fx, fy) = let GeoCoord x y = svCenter p in (x, y)
                poolRadius = svCalderaRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + svRimHeight p - 5
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface baseElev surfaceMap

        VolcanicShape (Caldera p) →
            let (fx, fy) = let GeoCoord x y = caCenter p in (x, y)
                poolRadius = caInnerRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + caRimHeight p - 3
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface baseElev surfaceMap

        VolcanicShape (ShieldVolcano p) | shSummitPit p →
            let (fx, fy) = let GeoCoord x y = shCenter p in (x, y)
                poolRadius = shPitRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + shPeakHeight p - shPitDepth p + 2
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface baseElev surfaceMap

        VolcanicShape (CinderCone p) →
            let (fx, fy) = let GeoCoord x y = ccCenter p in (x, y)
                poolRadius = ccCraterRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + ccPeakHeight p - ccCraterDepth p + 2
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface baseElev surfaceMap

        VolcanicShape (FissureVolcano p) | fpHasMagma p →
            let GeoCoord sx sy = fpStart p
                GeoCoord ex ey = fpEnd p
                midX = (sx + ex) `div` 2
                midY = (sy + ey) `div` 2
                (baseElev, _) = elevationAtGlobal seed plates worldSize midX midY
                poolWidth = fpWidth p `div` 2
                lavaSurface = baseElev + fpRidgeHeight p - 3
            in fillFissurePool mv seed plates worldSize chunkGX chunkGY
                   sx sy ex ey poolWidth lavaSurface baseElev surfaceMap

        VolcanicShape (HydrothermalVent p) →
            let (fx, fy) = let GeoCoord x y = htCenter p in (x, y)
                poolRadius = max 2 (htRadius p `div` 3)
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + htChimneyHeight p - 2
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface baseElev surfaceMap
        _ → pure ()

-- * Circular Pool (shared by most volcanic features)

fillPool ∷ MV.MVector s (Maybe FluidCell)
         → Word64 → [TectonicPlate] → Int → Int → Int → Int → Int → Int → Int
         → Int → VU.Vector Int
         → ST s ()
fillPool mv seed plates worldSize chunkGX chunkGY fx fy poolRadius lavaSurface baseElev surfaceMap =
    let pr = fromIntegral poolRadius ∷ Float
        rimSamples = 32 ∷ Int
        -- Sample the rim ALWAYS using elevationAtGlobal (pre-erosion
        -- plate elevation). Earlier this branched between surfaceMap
        -- and elevationAtGlobal depending on whether the sample fell
        -- inside the current chunk — which made the spillway
        -- chunk-dependent: two chunks computing lava for the same
        -- volcano would derive different spillway heights and place
        -- lava at different surface elevations, causing visible
        -- seams. Lava placement is determined at world init and
        -- shouldn't shift with later erosion of the rim anyway, so
        -- globally-deterministic plate elevation is the right input.
        spillway = foldl' (\minElev i →
            let angle = fromIntegral i * 2.0 * π / fromIntegral rimSamples
                rimGX = fx + round (pr * cos angle)
                rimGY = fy + round (pr * sin angle)
                (rimElev, _) = elevationAtGlobal seed plates worldSize
                                   rimGX rimGY
            in min minElev rimElev
            ) lavaSurface [0 .. rimSamples - 1]

        clampedSurface = min lavaSurface spillway

        -- Reject tiles whose terrain is far below the volcano's base
        -- elevation — they are ocean floor, not caldera interior.
        minTerrain = baseElev - 20

    in if clampedSurface ≤ seaLevel
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                (dxi, dyi) = wrappedDeltaUVFluid worldSize gx gy fx fy
                dx = fromIntegral dxi ∷ Float
                dy = fromIntegral dyi ∷ Float
                dist = sqrt (dx * dx + dy * dy)
            in when (dist < pr ∧ surfZ < clampedSurface ∧ surfZ ≥ minTerrain) $
                MV.write mv idx (Just (FluidCell Lava clampedSurface))

-- * Fissure Pool (linear lava fill)

fillFissurePool ∷ MV.MVector s (Maybe FluidCell)
                → Word64 → [TectonicPlate] → Int → Int → Int
                → Int → Int → Int → Int
                → Int → Int → Int
                → VU.Vector Int
                → ST s ()
fillFissurePool mv seed plates worldSize chunkGX chunkGY sx sy ex ey halfWidth lavaSurface baseElev surfaceMap =
    let lineLen = sqrt (fromIntegral ((ex-sx)*(ex-sx) + (ey-sy)*(ey-sy))) ∷ Float
    in if lineLen < 0.001 then pure ()
    else
    let edgeSamples = 16 ∷ Int
        hw = fromIntegral halfWidth ∷ Float
        perpX = negate (fromIntegral (ey - sy)) / lineLen ∷ Float
        perpY = fromIntegral (ex - sx) / lineLen ∷ Float

        -- Edge sampling uses globally-deterministic plate elevation
        -- so two chunks computing the same fissure agree on the
        -- spillway. See note in `fillPool`.
        spillway = foldl' (\minElev i →
            let t = fromIntegral i / fromIntegral (edgeSamples - 1) ∷ Float
                mx = fromIntegral sx + t * fromIntegral (ex - sx)
                my = fromIntegral sy + t * fromIntegral (ey - sy)
                e1gx = round (mx + perpX * hw) ∷ Int
                e1gy = round (my + perpY * hw) ∷ Int
                e2gx = round (mx - perpX * hw) ∷ Int
                e2gy = round (my - perpY * hw) ∷ Int
                elev1 = fst (elevationAtGlobal seed plates worldSize e1gx e1gy)
                elev2 = fst (elevationAtGlobal seed plates worldSize e2gx e2gy)
            in min minElev (min elev1 elev2)
            ) lavaSurface [0 .. edgeSamples - 1]

        clampedSurface = min lavaSurface spillway

        minTerrain = baseElev - 20

    in if clampedSurface ≤ seaLevel
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                (dxi, dyi) = wrappedDeltaUVFluid worldSize gx gy sx sy
                dx = fromIntegral dxi ∷ Float
                dy = fromIntegral dyi ∷ Float
                lx' = fromIntegral (ex - sx) ∷ Float
                ly' = fromIntegral (ey - sy) ∷ Float
                t = max 0 (min 1 ((dx * lx' + dy * ly') / (lineLen * lineLen)))
                projX = t * lx'
                projY = t * ly'
                perpDist = sqrt ((dx - projX) * (dx - projX) + (dy - projY) * (dy - projY))
            in when (perpDist < hw ∧ surfZ < clampedSurface ∧ surfZ ≥ minTerrain) $
                MV.write mv idx (Just (FluidCell Lava clampedSurface))

-- * Quick Check

-- | Quick boolean check: does this chunk have any lava?
--   Avoids allocating a full FluidMap — just checks if any
--   active volcanic feature is near enough to produce lava.
hasAnyLavaQuick ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                → Int → ChunkCoord → Int → Bool
hasAnyLavaQuick features seed plates worldSize coord _avgElev =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
    in any (isNearbyActive worldSize chunkMinGX chunkMinGY) features
