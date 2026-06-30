{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Plate
    ( -- * Plate data
      TectonicPlate(..)
    , generatePlates
    , defaultPlatesFor
      -- * Queries
    , twoNearestPlates
    , elevationAtGlobal
      -- * Boundary classification
    , BoundaryType(..)
    , classifyBoundary
    , isGlacierZone
    , isBeyondGlacier
      -- * wrapping
    , wrapGlobalX
    , wrapGlobalU
    , worldWidthTiles
      -- * noise
    , wrappedValueNoise2D
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)
import World.Material (MaterialId(..), matGranite, matDiorite, matGabbro, matGlacier)
import World.Scale (WorldScale(..), computeWorldScale, scaleElev, scaleDist, scaleElevI)
import World.Constants (seaLevel)
import World.Chunk.Types (chunkSize)
import World.Plate.Types (TectonicPlate(..))

-- * Boundary Classification

data BoundaryType
    = Convergent !Float
    | Divergent  !Float
    | Transform  !Float
    deriving (Show)

-- * Plate Generation

-- | Default plate count scaled to worldSize. Linear in side length
--   (sub-linear in area), so plates get bigger with larger worlds
--   but not as fast as the area itself — preventing 10 fixed plates
--   from producing tiny noisy plates at worldSize=64 or huge
--   featureless continents at worldSize=512 (audit #17).
--
--   Calibrated so worldSize=128 yields ~10 plates (the legacy
--   default). Floor at 4 keeps even tiny worlds from collapsing
--   to a single landmass.
defaultPlatesFor ∷ Int → Int
defaultPlatesFor worldSize = max 4 (worldSize `div` 13)

generatePlates ∷ Word64 → Int → Int → [TectonicPlate]
generatePlates seed worldSize plateCount =
    map (generateOnePlate seed worldSize) [0 .. plateCount - 1]

generateOnePlate ∷ Word64 → Int → Int → TectonicPlate
generateOnePlate seed worldSize plateIndex =
    let wsc = computeWorldScale worldSize
        halfTiles = (worldSize * chunkSize) `div` 2
        h1 = plateHash seed plateIndex 1
        h2 = plateHash seed plateIndex 2
        h3 = plateHash seed plateIndex 3
        h4 = plateHash seed plateIndex 4
        h5 = plateHash seed plateIndex 5
        h6 = plateHash seed plateIndex 6
        h7 = plateHash seed plateIndex 7
        h8 = plateHash seed plateIndex 8

        cx = hashToRange h1 (-halfTiles) (halfTiles - 1)
        cy = hashToRange h2 (-halfTiles) (halfTiles - 1)

        isLand = hashToFloat' h3 > 0.4

        -- Raw values are in meters. Convert to tiles at 10m/tile,
        -- then apply world-size scaling.
        --   Land:  200–1000m  → 20–100 tiles (200m floor keeps small worlds
        --                       above sea level — see plateBaseScale below)
        --   Ocean: -6000– -3000m → -600– -300 tiles
        --
        -- Base elevation scales sub-linearly with world size (sqrt) so a
        -- w32 world's plate sits at ~25 tiles rather than ~6, giving each
        -- plate a recognizable plateau above sea level. Mountains still
        -- shrink (see boundaryHeightScale in convergentEffect/divergentEffect)
        -- but plate baselines stay perceptible.
        metersPerTile = 10.0 ∷ Float
        plateBaseScale = sqrt (wsScale wsc) ∷ Float
        baseElev = if isLand
                   then round (plateBaseScale
                              * fromIntegral (hashToRange h4 200 1000)
                              / metersPerTile)
                   else round (scaleElev wsc
                              (fromIntegral (hashToRange h4 (-6000) (-3000))
                               / metersPerTile))

        matChoice = hashToRange h5 0 2
        material = case matChoice of
            0 → matGranite
            1 → matDiorite
            _ → matGabbro

        density = if isLand
                  then 2.7 + hashToFloat' h8 * 0.2
                  else 3.0 + hashToFloat' h8 * 0.3

        driftX = hashToFloat' h6 * 2.0 - 1.0
        driftY = hashToFloat' h7 * 2.0 - 1.0

    in TectonicPlate
        { plateCenterX  = cx
        , plateCenterY  = cy
        , plateIsLand   = isLand
        , plateBaseElev = baseElev
        , plateMaterial = material
        , plateDensity  = density
        , plateDriftX   = driftX
        , plateDriftY   = driftY
        }

-- * Plate Queries

twoNearestPlates ∷ Word64 → Int → [TectonicPlate] → Int → Int
                 → ((TectonicPlate, Float), (TectonicPlate, Float))
twoNearestPlates seed worldSize plates gx gy =
    let -- Cheap raw distance: 2 mults + 1 sqrt, no noise lookups.
        rawDist plate =
            let du = fromIntegral (wrappedDeltaU worldSize gx gy
                        (plateCenterX plate) (plateCenterY plate)) ∷ Float
                dv = fromIntegral ((gx + gy) - (plateCenterX plate + plateCenterY plate)) ∷ Float
            in sqrt (du * du + dv * dv)

        -- plateJitter outputs in [-40, +40]. A plate's actual
        -- distance is at least rawDist - 40, so any plate with
        -- rawDist - 40 > best2.dist cannot beat the runner-up —
        -- we skip its expensive jitter computation (audit #18,
        -- which was the hot path: 2 noise lookups per plate per
        -- elevationAtGlobal call).
        jitterMax = 40.0 ∷ Float

        jitter plate =
            plateJitter seed worldSize gx gy
                (plateCenterX plate) (plateCenterY plate)

        -- best2Real flag tracks whether best2 holds a real plate or
        -- the initial placeholder. The skip predicate only fires once
        -- best2 is real — otherwise the placeholder distance (matching
        -- the original `maxDist = fromIntegral worldSize`) is a small
        -- finite "infinity" specifically chosen to feed downstream
        -- boundary calculations correctly when the first plate has no
        -- competitor. Preserving that placeholder keeps elevation
        -- bit-identical to the pre-optimization output.
        go !best1 !best2 !best2Real [] = (best1, best2)
        go !best1 !best2 !best2Real (p:rest) =
            let !raw = rawDist p
            in if best2Real ∧ raw - jitterMax > snd best2
               then go best1 best2 best2Real rest
               else let !d = raw + jitter p
                        !cand = (p, d)
                    in if d < snd best1
                       then go cand best1 True rest
                       else if d < snd best2
                            then go best1 cand True rest
                            else go best1 best2 best2Real rest

        maxDist = fromIntegral worldSize ∷ Float
    in case plates of
        (p:ps) →
            let !raw0 = rawDist p
                !d0   = raw0 + jitter p
                first  = (p, d0)
                second = (p, maxDist)
            in go first second False ps
        _ → error "no plates"

-- * Boundary Classification

classifyBoundary ∷ Int → TectonicPlate → TectonicPlate → BoundaryType
classifyBoundary worldSize plateA plateB =
    let duRaw = fromIntegral (wrappedDeltaU worldSize 
                    (plateCenterX plateA) (plateCenterY plateA)
                    (plateCenterX plateB) (plateCenterY plateB)) ∷ Float
        dvRaw = fromIntegral ((plateCenterX plateB + plateCenterY plateB) 
                            - (plateCenterX plateA + plateCenterY plateA)) ∷ Float
        nLen  = sqrt (duRaw * duRaw + dvRaw * dvRaw)
        (nx, ny) = if nLen > 0.001
                   then (duRaw / nLen, dvRaw / nLen)
                   else (1.0, 0.0)

        (tx, ty) = (-ny, nx)

        approachA = plateDriftX plateA * nx + plateDriftY plateA * ny
        approachB = plateDriftX plateB * nx + plateDriftY plateB * ny
        approach  = approachA - approachB

        shearA = plateDriftX plateA * tx + plateDriftY plateA * ty
        shearB = plateDriftX plateB * tx + plateDriftY plateB * ty
        shear  = abs (shearA - shearB)

        convergentThreshold = 0.3
        divergentThreshold  = -0.3

    in if approach > convergentThreshold
       then Convergent approach
       else if approach < divergentThreshold
            then Divergent (abs approach)
            else Transform shear

-- * Glacier Border

glacierWidthRows ∷ Int
glacierWidthRows = chunkSize

isGlacierZone ∷ Int → Int → Int → Bool
isGlacierZone worldSize gx gy =
    let halfTiles = (worldSize * chunkSize) `div` 2
        glacierEdge = halfTiles - glacierWidthRows
        screenRow = gx + gy
    in abs screenRow ≥ glacierEdge

isBeyondGlacier ∷ Int → Int → Int → Bool
isBeyondGlacier worldSize gx gy =
    let halfTiles = (worldSize * chunkSize) `div` 2
        screenRow = gx + gy
    in abs screenRow > halfTiles

-- * Cylindrical Wrapping

worldWidthTiles ∷ Int → Int
worldWidthTiles worldSize = worldSize * chunkSize

-- | Wrap a global coordinate pair so that the isometric u-axis
--   (gx - gy, which maps to screen X) wraps by worldTiles,
--   while the v-axis (gx + gy, which maps to screen Y) stays fixed.
--
--   This produces a vertical seam on screen instead of a diagonal one.
wrapGlobalU ∷ Int → Int → Int → (Int, Int)
wrapGlobalU worldSize gx gy =
    let w = worldWidthTiles worldSize    -- worldTiles
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        -- Wrap u into [-halfW, halfW)
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
        -- Recover gx, gy from (wrappedU, v)
        gx' = (wrappedU + v) `div` 2
        gy' = (v - wrappedU) `div` 2
    in (gx', gy')

-- | Old wrap: wraps gx only. Produces diagonal seam.
--   Use wrapGlobalU instead for screen-aligned wrapping.
wrapGlobalX ∷ Int → Int → Int
wrapGlobalX worldSize gx =
    let w = worldWidthTiles worldSize
        halfW = w `div` 2
        wrapped = ((gx + halfW) `mod` w + w) `mod` w - halfW
    in wrapped

-- | Wrapped distance in the u-axis between two points
wrappedDeltaU ∷ Int → Int → Int → Int → Int → Int
wrappedDeltaU worldSize gx1 gy1 gx2 gy2 =
    let w = worldWidthTiles worldSize
        u1 = gx1 - gy1
        u2 = gx2 - gy2
        raw = u2 - u1
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW

wrappedValueNoise2D ∷ Word64 → Int → Int → Int → Int → Float
wrappedValueNoise2D seed worldSize gx gy scale =
    let w = worldWidthTiles worldSize
        -- Work in u-space for wrapping. To make the cell grid tile
        -- exactly across the cylindrical seam, snap cellsInU to the
        -- nearest integer count and derive an effectiveScale that
        -- divides w cleanly — otherwise `w mod scale` tiles at the
        -- seam fall into a defective zone whose lerp pairs don't
        -- match the deep-interior wrap (audit #8). The shift in
        -- noise feature size is at most one tile per `scale`, ≪1%
        -- for the scales used here.
        u = gx - gy
        v = gx + gy
        cellsInU = max 1 (round (fromIntegral w / fromIntegral scale ∷ Float))
        effectiveScale = fromIntegral w / fromIntegral cellsInU ∷ Float
        fu = fromIntegral u / effectiveScale ∷ Float
        -- v doesn't wrap, so the integer scale is fine there.
        fv = fromIntegral v / fromIntegral scale ∷ Float
        iu = floor fu ∷ Int
        iv = floor fv ∷ Int
        tu = fu - fromIntegral iu
        tv = fv - fromIntegral iv
        su = smoothstep tu
        sv = smoothstep tv
        wrapIu i = ((i `mod` cellsInU) + cellsInU) `mod` cellsInU
        iu0 = wrapIu iu
        iu1 = wrapIu (iu + 1)
        v00 = hashToFloat' (hashCoord seed iu0     iv)
        v10 = hashToFloat' (hashCoord seed iu1     iv)
        v01 = hashToFloat' (hashCoord seed iu0     (iv + 1))
        v11 = hashToFloat' (hashCoord seed iu1     (iv + 1))
        top    = lerp su v00 v10
        bottom = lerp su v01 v11
    in lerp sv top bottom

-- * Global Elevation Query

-- | Land elevation pipeline (RTS-friendly):
--
--   1. Smooth trend:   baseElev + shelfEffect + boundaryEffect
--   2. Ridge noise:    jagged peaks near plate boundaries (mountainness > 0)
--   3. Value noise:    mountain-side texture only; lowlands get zero noise
--   4. Plateau snap:   lowland tiles quantize to step bands of the plate
--                      base elevation, turning each plate into a plateau
--
--   `mountainness` blends in the noise/ridge contributions and gates the
--   plateau snap: 1.0 right at a plate boundary, 0.0 once we're more than
--   `mountainReach` tiles into the plate interior. Below ~0.3 we treat the
--   tile as lowland and snap; above ~0.3 we keep the smooth peak silhouette.
--
--   Ocean uses standard value noise (organic seabed); no terracing.

-- | Reach over which a plate boundary is felt for the purposes of
--   mountain shaping. Smaller than the boundary-elevation fadeRange (200)
--   on purpose — we want the ridge texture concentrated near the actual
--   peak, not spread out over the foothills.
mountainReach ∷ WorldScale → Float
mountainReach wsc = scaleDist wsc 80.0

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

-- | Sharp-ridged noise in [0,1]. Standard value noise produces rounded
--   peaks; ridge noise produces sharp creases where the underlying
--   value-noise crosses 0.5. Used as the mountain-peak silhouette.
wrappedRidgeNoise2D ∷ Word64 → Int → Int → Int → Int → Float
wrappedRidgeNoise2D seed worldSize gx gy scale =
    let n = wrappedValueNoise2D seed worldSize gx gy scale
    in 1.0 - 2.0 * abs (n - 0.5)

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

-- | Smooth the base elevation gap at land-ocean plate boundaries.
--   On the ocean side: raises the deep ocean floor toward sea level,
--   creating a gradual continental shelf slope.
--   On the land side: lowers coastal terrain toward sea level,
--   creating coastal lowlands instead of flat plateaus.
continentalShelf ∷ WorldScale → TectonicPlate → TectonicPlate
                 → Float → Int
continentalShelf wsc plateA plateB boundaryDist
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
        let lowlandRange = max 4.0 (scaleDist wsc 20.0)
            dist = abs boundaryDist
            t = clamp01 (dist / lowlandRange)
            t' = t * t
            -- At boundary (t'=0): pull land down near sea level
            -- Far from boundary (t'=1): no effect (normal land)
            landElev = fromIntegral (plateBaseElev plateA)
            coastTarget = fromIntegral seaLevel + 2.0
            drop' = landElev - coastTarget
        in if drop' > 0
           then negate (round (drop' * (1.0 - t')))
           else 0
    | otherwise = 0

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

-- * Jitter

-- | Per-plate jitter: each plate gets a different noise field
--   keyed by its center coordinates. This creates irregular
--   plate boundaries instead of straight Voronoi edges.
plateJitter ∷ Word64 → Int → Int → Int → Int → Int → Float
plateJitter seed worldSize gx gy plateCX plateCY =
    let plateSeed = seed `xor` fromIntegral (plateCX * 7919 + plateCY * 6271)
        n1 = wrappedValueNoise2D plateSeed worldSize gx gy 20
        n2 = wrappedValueNoise2D (plateSeed + 99) worldSize gx gy 8
        combined = n1 * 0.7 + n2 * 0.3
    in (combined - 0.5) * 80.0

-- * Noise & Hash

plateHash ∷ Word64 → Int → Int → Word32
plateHash seed plateIdx propIdx =
    hashCoord (seed + fromIntegral propIdx * 7919) plateIdx propIdx

hashCoord ∷ Word64 → Int → Int → Word32
hashCoord seed x y =
    let h0 = fromIntegral seed ∷ Word64
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
        h6 = h5 * 0xc4ceb9fe1a85ec53
        h7 = h6 `xor` (h6 `shiftR` 33)
    in fromIntegral (h7 .&. 0xFFFFFFFF)

hashToFloat' ∷ Word32 → Float
hashToFloat' h = fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF ∷ Word32)

hashToRange ∷ Word32 → Int → Int → Int
hashToRange h lo hi =
    let f = hashToFloat' h
        span' = hi - lo + 1
    in min hi (lo + floor (f * fromIntegral span'))

smoothstep ∷ Float → Float
smoothstep t = t * t * (3.0 - 2.0 * t)

lerp ∷ Float → Float → Float → Float
lerp t a b = a + t * (b - a)
