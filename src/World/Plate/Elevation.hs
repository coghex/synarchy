{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Global per-tile elevation query, split out of "World.Plate" (issue
--   #560).
--
--   Land elevation pipeline (RTS-friendly):
--
--     1. Smooth trend:   baseElev + shelfEffect + boundaryEffect
--     2. Ridge noise:    jagged peaks near plate boundaries (mountainness > 0)
--     3. Value noise:    mountain-side texture only; lowlands get zero noise
--     4. Plateau snap:   lowland tiles quantize to step bands of the plate
--                        base elevation, turning each plate into a plateau
--
--   `mountainness` blends in the noise/ridge contributions and gates the
--   plateau snap: 1.0 right at a plate boundary, 0.0 once we're more than
--   `mountainReach` tiles into the plate interior. Below ~0.3 we treat the
--   tile as lowland and snap; above ~0.3 we keep the smooth peak silhouette.
--
--   Ocean uses standard value noise (organic seabed); no terracing.
module World.Plate.Elevation
    ( elevationAtGlobal
    ) where

import UPrelude
import World.Material (MaterialId(..), matGlacier)
import World.Scale (WorldScale(..), computeWorldScale, scaleElev, scaleElevI, scaleDist)
import World.Plate.Types (TectonicPlate(..))
import World.Plate.Wrap (wrapGlobalU)
import World.Plate.Glacier (isGlacierZone, isBeyondGlacier)
import World.Plate.Query (twoNearestPlates, classifyBoundary)
import World.Plate.Coast (coastTectonicSteepness)
import World.Plate.Profiles (boundaryElevation, continentalShelf, coastLowlandRange)
import World.Plate.Noise (wrappedValueNoise2D, wrappedRidgeNoise2D)

-- | Reach over which a plate boundary is felt for the purposes of
--   mountain shaping. Smaller than the boundary-elevation fadeRange (200)
--   on purpose — we want the ridge texture concentrated near the actual
--   peak, not spread out over the foothills.
mountainReach ∷ WorldScale → Float
mountainReach wsc = scaleDist wsc 80.0

-- | How likely a tile is to be "in the mountains".
--   1.0 right at the boundary, 0.0 once boundaryDist ≥ mountainReach.
--   Smoothstepped so the transition into the plateau is gradual rather
--   than a hard ring at the threshold.
mountainness ∷ WorldScale → Float → Float
mountainness wsc boundaryDist =
    let t = 1.0 - clamp01 (abs boundaryDist / mountainReach wsc)
    in t * t * (3.0 - 2.0 * t)

-- | Tiles with mountainness above this threshold keep their raw elevation
--   (no plateau snap). Below it, lowland-style quantization applies.
plateauCutoff ∷ Float
plateauCutoff = 0.30

-- | Snap a lowland elevation onto step bands so each plate forms a flat
--   plateau at its `plateBaseElev` level. Step size scales with the world
--   so small worlds get shallower terraces (preserving differentiation
--   between adjacent plates) and large worlds get visible landings — at
--   scale=1.0 we land on ~4-tile bands.
plateauSnap ∷ WorldScale → Int → Int
plateauSnap wsc e =
    let step = max 2 (scaleElevI wsc 4)
        -- Round to nearest rather than `div` toward zero, which would
        -- bias negative elevations (rare on land, but coastal lowlands
        -- can dip slightly below sea level after shelf softening).
        halfStep = step `div` 2
        snapped  = if e ≥ 0
                   then ((e + halfStep) `div` step) * step
                   else negate (((negate e + halfStep) `div` step) * step)
    in snapped

-- | Per-tile ridge contribution at land mountains.
--   Two octaves so the ridges have both broad backbone (scale 18) and
--   fine crenellation (scale 7). Weighted toward the larger features so
--   peaks read as connected ranges rather than scattered spikes.
ridgeContribution ∷ Word64 → Int → Int → Int → WorldScale → Float → Int
ridgeContribution seed worldSize gx gy wsc mness
    | mness ≤ 0.0 = 0
    | otherwise =
        let r1 = wrappedRidgeNoise2D (seed + 20) worldSize gx gy 18
            r2 = wrappedRidgeNoise2D (seed + 21) worldSize gx gy 7
            -- Power-curve the blend to sharpen the peaks further: r^1.5
            -- pushes mid-values down and keeps the highest crest tiles
            -- near 1.0, producing jagged silhouettes instead of smooth domes.
            ridge = (r1 * 0.65 + r2 * 0.35) ** 1.5
            -- Mountainness gates both the magnitude and the existence —
            -- foothills get a fraction of the amplitude so the ridge fades
            -- naturally into the plateau.
            amp = scaleElev wsc 40.0 * mness
        in round (ridge * amp)

-- | Per-tile value-noise contribution (the soft +/- texture).
--   Zero in lowland zones (so plateaus stay flat). Modest in mountain
--   zones to break up the ridge geometry. Full strength in oceans.
valueContribution ∷ Word64 → Int → Int → Int → WorldScale → Bool → Float → Int
valueContribution seed worldSize gx gy wsc isLand mness =
    let raw = elevationNoise seed worldSize gx gy  -- integer in [-2, 2]
        amp
          | not isLand   = scaleElev wsc 20.0
          | mness > 0.0  = scaleElev wsc 6.0 * mness
          | otherwise    = 0.0
    in raw * round amp

-- | Gated sampling of the coastal steepness field for the shelf
--   modulation: only mixed land→ocean margins within the lowland
--   pull range pay for the field lookup (4 cell samples, each a
--   'twoNearestPlates' + 'classifyBoundary').
shelfSteepMod ∷ Word64 → Int → [TectonicPlate] → WorldScale
              → TectonicPlate → TectonicPlate → Float → Int → Int → Float
shelfSteepMod seed worldSize plates wsc plateA plateB boundaryDist gx gy
    | plateIsLand plateA ∧ not (plateIsLand plateB)
      ∧ abs boundaryDist < coastLowlandRange wsc
        = coastTectonicSteepness seed worldSize plates gx gy
    | otherwise = 0.0

elevationAtGlobal ∷ Word64 → [TectonicPlate] → Int → Int → Int → (Int, MaterialId)
elevationAtGlobal seed plates worldSize gx gy =
    let (gx', gy') = wrapGlobalU worldSize gx gy
        wsc = computeWorldScale worldSize
    in if isBeyondGlacier worldSize gx' gy' then (0, matGlacier)
    else if isGlacierZone worldSize gx' gy' then
        -- Glacier zone: natural plate terrain, matGlacier material.
        let ((plateA, distA), (plateB, distB)) = twoNearestPlates seed worldSize plates gx' gy'
            myPlate = plateA
            baseElev = plateBaseElev myPlate
            boundaryDist = (distB - distA) / 2.0
            boundary = classifyBoundary worldSize plateA plateB
            boundaryEffect = boundaryElevation wsc boundary plateA plateB boundaryDist
            shelfEffect = continentalShelf wsc plateA plateB boundaryDist
                (shelfSteepMod seed worldSize plates wsc plateA plateB
                    boundaryDist gx' gy')
            mness = mountainness wsc boundaryDist
            ridge = ridgeContribution seed worldSize gx' gy' wsc mness
            valN  = valueContribution seed worldSize gx' gy' wsc
                        (plateIsLand myPlate) mness
            rawElev = baseElev + shelfEffect + boundaryEffect + ridge + valN
            -- Plateau snap on lowland glacier zone tiles so the polar
            -- shelf reads as flat ice rather than rolling tundra.
            finalElev = if plateIsLand myPlate ∧ mness < plateauCutoff
                        then plateauSnap wsc rawElev
                        else rawElev
        in (finalElev, matGlacier)
    else
    let ((plateA, distA), (plateB, distB)) = twoNearestPlates seed worldSize plates gx' gy'

        myPlate = plateA
        material = plateMaterial myPlate
        baseElev = plateBaseElev myPlate

        boundaryDist = (distB - distA) / 2.0
        boundary = classifyBoundary worldSize plateA plateB
        boundaryEffect = boundaryElevation wsc boundary
                           plateA plateB boundaryDist
        shelfEffect = continentalShelf wsc plateA plateB boundaryDist
            (shelfSteepMod seed worldSize plates wsc plateA plateB
                boundaryDist gx' gy')

        mness = mountainness wsc boundaryDist
        ridge = ridgeContribution seed worldSize gx' gy' wsc mness
        valN  = valueContribution seed worldSize gx' gy' wsc
                    (plateIsLand myPlate) mness

        rawElev = baseElev + shelfEffect + boundaryEffect + ridge + valN

        -- Plateau snap on land tiles in lowland zones (mountainness below
        -- threshold). Mountains keep their raw, jagged silhouette;
        -- oceans are never snapped (organic seabed).
        finalElev = if plateIsLand myPlate ∧ mness < plateauCutoff
                    then plateauSnap wsc rawElev
                    else rawElev

    in (finalElev, material)

-- * Local Noise

elevationNoise ∷ Word64 → Int → Int → Int → Int
elevationNoise seed worldSize gx gy =
    let e1 = wrappedValueNoise2D (seed + 10) worldSize gx gy 12
        e2 = wrappedValueNoise2D (seed + 11) worldSize gx gy 5
        raw = e1 * 0.7 + e2 * 0.3
        mapped = (raw - 0.5) * 3.0
    in clampInt (-2) 2 (round mapped)

clampInt ∷ Int → Int → Int → Int
clampInt lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x
