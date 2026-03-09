{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Lake
    ( computeChunkLakes
    , hasAnyLakeQuick
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
        maxR = round (fromIntegral (lkRadius lk) * 1.25 ∷ Float)
        (dxi, dyi) = wrappedDeltaUVFluid worldSize chunkGX chunkGY fx fy
        dx = abs dxi
        dy = abs dyi
    in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

-- | Quick boolean check: does this chunk have any lake?
hasAnyLakeQuick ∷ [PersistentFeature] → Int → ChunkCoord → Bool
hasAnyLakeQuick features worldSize coord =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
    in any (isNearbyLake worldSize chunkMinGX chunkMinGY) features

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
                lakeSurface = lkSurface lk
            in fillLakePool mv seed plates worldSize chunkGX chunkGY
                   fx fy poolRadius lakeSurface surfaceMap
        _ → pure ()

fillLakePool ∷ MV.MVector s (Maybe FluidCell)
             → Word64 → [TectonicPlate] → Int → Int → Int
             → Int → Int → Int → Int
             → VU.Vector Int
             → ST s ()
fillLakePool mv seed plates worldSize chunkGX chunkGY fx fy poolRadius lakeSurface surfaceMap =
    let -- Expand fill radius by 25% to cover perturbed basin boundary.
        -- The carving in Event.hs uses angular noise that can extend
        -- the basin up to ~20% beyond the nominal radius. The surfZ
        -- check ensures we only fill tiles actually carved below water.
        pr = fromIntegral poolRadius * 1.25 ∷ Float
        rimSamples = 32 ∷ Int

        -- Always use elevationAtGlobal for rim samples so every chunk
        -- computes the same spillway height deterministically.
        spillway = foldl' (\minElev i →
            let angle = fromIntegral i * 2.0 * π / fromIntegral rimSamples
                rimGX = fx + round (pr * cos angle)
                rimGY = fy + round (pr * sin angle)
                (e, _) = elevationAtGlobal seed plates worldSize rimGX rimGY
            in min minElev e
            ) lakeSurface [0 .. rimSamples - 1]

        clampedSurface = min lakeSurface spillway

    in if clampedSurface ≤ seaLevel
       then pure ()
       else do
            -- Pass 1: fill tiles within radius that are at/below lake surface
            forEachSurface surfaceMap $ \idx lx ly surfZ →
                let gx = chunkGX + lx
                    gy = chunkGY + ly
                    (dxi, dyi) = wrappedDeltaUVFluid worldSize gx gy fx fy
                    dx = fromIntegral dxi ∷ Float
                    dy = fromIntegral dyi ∷ Float
                    dist = sqrt (dx * dx + dy * dy)
                in when (dist < pr ∧ surfZ ≤ clampedSurface ∧ surfZ > minBound) $
                    MV.write mv idx (Just (FluidCell Lake clampedSurface))
            -- Pass 2: extend one tile to adjacent empty tiles whose terrain
            -- is strictly BELOW the water surface. This covers exposed
            -- side-faces at the shoreline without jumping over ridges
            -- (ridges have terrain above the water surface).
            forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
                val ← MV.read mv idx
                when (isNothing val) $ do
                    let surfZ = surfaceMap VU.! idx
                    when (surfZ < clampedSurface ∧ surfZ > minBound) $ do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                        adj ← adjacentHasFluid mv lx ly
                        when adj $
                            MV.write mv idx (Just (FluidCell Lake clampedSurface))

adjacentHasFluid ∷ MV.MVector s (Maybe FluidCell) → Int → Int → ST s Bool
adjacentHasFluid mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = return False
          | otherwise = isJust ⊚ MV.read mv (y * chunkSize + x)
    n ← check lx (ly - 1)
    s ← check lx (ly + 1)
    e ← check (lx + 1) ly
    w ← check (lx - 1) ly
    return (n ∨ s ∨ e ∨ w)
