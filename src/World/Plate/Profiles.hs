{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Boundary elevation profiles (convergent/divergent/transform) and
--   the continental-shelf smoothing pass, split out of "World.Plate"
--   (issue #560).
module World.Plate.Profiles
    ( boundaryElevation
    , continentalShelf
    , coastLowlandRange
    ) where

import UPrelude
import World.Scale (WorldScale(..), scaleDist)
import World.Constants (seaLevel)
import World.Plate.Types (TectonicPlate(..))
import World.Plate.Query (BoundaryType(..))

-- * Boundary Elevation Profiles

boundaryElevation ∷ WorldScale → BoundaryType
                  → TectonicPlate → TectonicPlate
                  → Float → Int
boundaryElevation wsc boundary plateA plateB boundaryDist =
    let bothLand  = plateIsLand plateA ∧ plateIsLand plateB
        bothOcean = not (plateIsLand plateA) ∧ not (plateIsLand plateB)
        aIsLand   = plateIsLand plateA
        bIsLand   = plateIsLand plateB
    in case boundary of
        Convergent strength → convergentEffect wsc
            aIsLand bIsLand bothLand bothOcean
            (plateDensity plateA) (plateDensity plateB)
            strength boundaryDist
        Divergent strength → divergentEffect wsc
            aIsLand bothOcean strength boundaryDist
        Transform _shear → transformEffect wsc boundaryDist

-- | Super-linear vertical scaling for boundary effects (mountain peaks,
--   trench depths, ridge heights). Uses `wsScale^1.5` rather than the
--   linear `wsScale` used by `scaleElev`, so a w512 world keeps its full
--   1000m+ mountains but a w32 world (whose plates are only 256 tiles
--   wide) gets much smaller peaks — preventing single-tile-wide tower
--   walls that fragment the playable lowland.
--
--   At reference scale this returns 1.0 (no change). It shrinks faster
--   than `wsScale` for any worldSize < 512 and amplifies for any
--   worldSize > 512 (unlikely in practice but consistent).
boundaryHeightScale ∷ WorldScale → Float
boundaryHeightScale wsc =
    let s = wsScale wsc
    in s * sqrt s   -- == s ** 1.5

-- | Like `scaleElev` but for boundary peak/trench magnitudes. Combines
--   the standard linear `scaleElev` with the super-linear factor so
--   small worlds get gentler mountains while keeping reference values
--   recognizable in the source (still expressed as raw meters).
scaleBoundaryElev ∷ WorldScale → Float → Float
scaleBoundaryElev wsc v = v * boundaryHeightScale wsc

-- | Convergent boundary elevation effect.
--   All reference values are for worldSize=512.
--   The WorldScale factor handles smaller/larger planets.
--
--   BUG FIX: previously t' was applied twice (once inside
--   peakElev, once as the multiplier). Now t' is only the
--   spatial falloff, and strength only scales the peak magnitude.

convergentEffect ∷ WorldScale → Bool → Bool → Bool → Bool
                 → Float → Float → Float → Float → Int
convergentEffect wsc aIsLand bIsLand bothLand bothOcean
                 densityA densityB strength boundaryDist =
    let fadeRange = scaleDist wsc 200.0
        peakRange = scaleDist wsc 30.0
        t = max 0.0 (1.0 - max 0.0 (abs boundaryDist - peakRange) / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        s = min 2.0 (abs strength)
        metersPerTile = 10.0

    in if bothLand then
        -- Land-land convergent: mountain range (Himalayas)
        -- Reference: 3000–13000m → 300–1300 tiles at scale 1.0
        let peakElev = scaleBoundaryElev wsc ((3000.0 + 5000.0 * s) / metersPerTile)
        in round (peakElev * t')

    else if bothOcean then
        let isSubducting = densityA > densityB
        in if isSubducting
           -- Trench: reference -2000 to -8000m → -200 to -800 tiles
           then let trenchDepth = scaleBoundaryElev wsc ((2000.0 + 3000.0 * s) / metersPerTile)
                in round (negate trenchDepth * t')
           -- Island arc: reference 500m → 50 tiles
           else round (scaleBoundaryElev wsc (500.0 * s / metersPerTile) * t')

    else if aIsLand ∧ not bIsLand then
        -- Andes-style: reference 2000–10000m → 200–1000 tiles
        let peakElev = scaleBoundaryElev wsc ((2000.0 + 4000.0 * s) / metersPerTile)
        in round (peakElev * t')

    else
        -- Mariana-style trench: reference -3000 to -13000m → -300 to -1300 tiles
        let trenchDepth = scaleBoundaryElev wsc ((3000.0 + 5000.0 * s) / metersPerTile)
        in round (negate trenchDepth * t')

divergentEffect ∷ WorldScale → Bool → Bool → Float → Float → Int
divergentEffect wsc aIsLand bothOcean strength boundaryDist =
    let fadeRange = scaleDist wsc 150.0
        peakRange = scaleDist wsc 20.0
        t = max 0.0 (1.0 - max 0.0 (abs boundaryDist - peakRange) / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        s = min 2.0 (abs strength)
        metersPerTile = 10.0
    in if bothOcean then
        -- Mid-ocean ridge: reference 500–2500m → 50–250 tiles
        let ridgeHeight = scaleBoundaryElev wsc ((500.0 + 1000.0 * s) / metersPerTile)
        in round (ridgeHeight * t')
    else if aIsLand then
        -- Continental rift: reference -500 to -2500m → -50 to -250 tiles
        let riftDepth = scaleBoundaryElev wsc ((500.0 + 2000.0 * s) / metersPerTile)
        in round (negate riftDepth * t')
    else
        -- Ocean side of rift: reference -200 to -1000m → -20 to -100 tiles
        let depth = scaleBoundaryElev wsc ((200.0 + 800.0 * s) / metersPerTile)
        in round (negate depth * t')

transformEffect ∷ WorldScale → Float → Int
transformEffect wsc boundaryDist =
    let fadeRange = scaleDist wsc 50.0
        t = max 0.0 (1.0 - abs boundaryDist / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        metersPerTile = 10.0
    in round (scaleBoundaryElev wsc (100.0 / metersPerTile) * t' * (if boundaryDist > 0 then 1.0 else -1.0))

-- * Continental Shelf

-- | Range over which the land side of a mixed margin is pulled down
--   toward sea level. Shared by 'continentalShelf' and the gate in
--   'World.Plate.Elevation.elevationAtGlobal' that decides whether the
--   coastal steepness field is worth sampling.
coastLowlandRange ∷ WorldScale → Float
coastLowlandRange wsc = max 4.0 (scaleDist wsc 20.0)

-- | Smooth the base elevation gap at land-ocean plate boundaries.
--   On the ocean side: raises the deep ocean floor toward sea level,
--   creating a gradual continental shelf slope.
--   On the land side: lowers coastal terrain toward sea level,
--   creating coastal lowlands instead of flat plateaus. @steepMod@
--   (the #220 coastal steepness field, 0 = gentle margin, 1 = fully
--   convergent) scales that pull away so convergent margins keep
--   their natural mountains all the way to the waterline.
continentalShelf ∷ WorldScale → TectonicPlate → TectonicPlate
                 → Float → Float → Int
continentalShelf wsc plateA plateB boundaryDist steepMod
    -- Ocean plate near land plate: continental shelf
    | not (plateIsLand plateA) ∧ plateIsLand plateB =
        let shelfRange = max 6.0 (scaleDist wsc 40.0)
            dist = abs boundaryDist
            t = clamp01 (dist / shelfRange)
            -- Steep curve: shelf drops off quickly into the abyss
            t' = t * t
            -- At boundary (t'=0): raise floor to just below sea level
            -- Far from boundary (t'=1): no effect (deep ocean)
            shelfTop = fromIntegral seaLevel - 3.0
            deepFloor = fromIntegral (plateBaseElev plateA)
            gap = shelfTop - deepFloor
        in if gap > 0
           then round (gap * (1.0 - t'))
           else 0
    -- Land plate near ocean plate: coastal lowlands
    | plateIsLand plateA ∧ not (plateIsLand plateB) =
        let lowlandRange = coastLowlandRange wsc
            dist = abs boundaryDist
            t = clamp01 (dist / lowlandRange)
            t' = t * t
            -- At boundary (t'=0): pull land down near sea level
            -- Far from boundary (t'=1): no effect (normal land)
            landElev = fromIntegral (plateBaseElev plateA)
            coastTarget = fromIntegral seaLevel + 2.0
            drop' = landElev - coastTarget
        in if drop' > 0
           then negate (round (drop' * (1.0 - t') * (1.0 - steepMod)))
           else 0
    | otherwise = 0
