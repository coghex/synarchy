{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Plate
    ( -- * Plate data
      TectonicPlate(..)
    , generatePlates
      -- * Queries
    , plateAt
    , twoNearestPlates
    , elevationAtGlobal
      -- * Boundary classification
    , isGlacierZone
    , isBeyondGlacier
      -- * wrapping
    , wrapGlobalX
    , wrapGlobalU
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)
import Data.List (sortBy)
import Data.Ord (comparing)
import World.Material (MaterialId(..), matGranite, matDiorite, matGabbro, matGlacier)
import World.Scale (WorldScale(..), computeWorldScale, scaleElev, scaleDist)
import World.Types (chunkSize, TectonicPlate(..))

-----------------------------------------------------------
-- Boundary Classification
-----------------------------------------------------------

data BoundaryType
    = Convergent !Float
    | Divergent  !Float
    | Transform  !Float
    deriving (Show)

data BoundarySide = SidePlateA | SidePlateB
    deriving (Show, Eq)

-----------------------------------------------------------
-- Plate Generation
-----------------------------------------------------------

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

        cx = hashToRange h1 (-halfTiles) halfTiles
        cy = hashToRange h2 (-halfTiles) halfTiles

        isLand = hashToFloat' h3 > 0.4

        -- Raw values are in meters. Convert to tiles at 10m/tile,
        -- then apply world-size scaling.
        --   Land:  0–1000m  → 0–100 tiles
        --   Ocean: -6000– -3000m → -600– -300 tiles
        metersPerTile = 10.0 ∷ Float
        baseElev = if isLand
                   then round (scaleElev wsc (fromIntegral (hashToRange h4 0 1000) / metersPerTile))
                   else round (scaleElev wsc (fromIntegral (hashToRange h4 (-6000) (-3000)) / metersPerTile))

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

-----------------------------------------------------------
-- Plate Queries
-----------------------------------------------------------

plateAt ∷ Word64 → Int → [TectonicPlate] → Int → Int → (TectonicPlate, Float)
plateAt seed worldSize plates gx gy =
    let ranked = rankPlates seed worldSize plates gx gy
    in head ranked

twoNearestPlates ∷ Word64 → Int → [TectonicPlate] → Int → Int
                 → ((TectonicPlate, Float), (TectonicPlate, Float))
twoNearestPlates seed worldSize plates gx gy =
    let jitter = jitterAmount seed worldSize gx gy
        dist plate =
            let du = fromIntegral (wrappedDeltaU worldSize gx gy
                        (plateCenterX plate) (plateCenterY plate)) ∷ Float
                dv = fromIntegral ((gx + gy) - (plateCenterX plate + plateCenterY plate)) ∷ Float
            in sqrt (du * du + dv * dv) + jitter
    in case plates of
        (p:ps) →
            let d0 = dist p
                go !best1 !d1 !best2 !d2 [] = (best1, best2)
                go !best1 !d1 !best2 !d2 (q:qs) =
                    let dq = dist q
                    in if dq < d1
                       then go (q, dq) dq best1 d1 qs
                       else if dq < d2
                            then go best1 d1 (q, dq) dq qs
                            else go best1 d1 best2 d2 qs
                ((a, da), (b, db)) = go (p, d0) d0 (p, d0) (1/0) ps
            in ((a, da), (b, db))
        _ → error "no plates"

rankPlates seed worldSize plates gx gy =
    let jitter = jitterAmount seed worldSize gx gy
        withDist plate =
            let du = fromIntegral (wrappedDeltaU worldSize gx gy
                        (plateCenterX plate) (plateCenterY plate)) ∷ Float
                dv = fromIntegral ((gx + gy) - (plateCenterX plate + plateCenterY plate)) ∷ Float
            in (plate, sqrt (du * du + dv * dv) + jitter)
    in sortBy (comparing snd) (map withDist plates)

-----------------------------------------------------------
-- Boundary Classification
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Glacier Border
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Cylindrical Wrapping
-----------------------------------------------------------

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
        -- Work in u-space for wrapping
        u = gx - gy
        v = gx + gy
        fu = fromIntegral u / fromIntegral scale ∷ Float
        fv = fromIntegral v / fromIntegral scale ∷ Float
        iu = floor fu ∷ Int
        iv = floor fv ∷ Int
        tu = fu - fromIntegral iu
        tv = fv - fromIntegral iv
        su = smoothstep tu
        sv = smoothstep tv
        cellsInU = w `div` scale
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

-----------------------------------------------------------
-- Global Elevation Query
-----------------------------------------------------------

elevationAtGlobal ∷ Word64 → [TectonicPlate] → Int → Int → Int → (Int, MaterialId)
elevationAtGlobal seed plates worldSize gx gy =
    let (gx', gy') = wrapGlobalU worldSize gx gy
        wsc = computeWorldScale worldSize
    in if isBeyondGlacier worldSize gx' gy' then (0, matGlacier)
    else if isGlacierZone worldSize gx' gy' then
        let ((plateA, distA), (plateB, distB)) = twoNearestPlates seed worldSize plates gx' gy'
            myPlate = plateA
            baseElev = plateBaseElev myPlate
            boundaryDist = (distB - distA) / 2.0
            boundary = classifyBoundary worldSize plateA plateB
            side = SidePlateA
            boundaryEffect = boundaryElevation wsc boundary side plateA plateB boundaryDist
            localNoise = elevationNoise seed worldSize gx' gy'
            noiseScale = if plateIsLand myPlate
                         then round (scaleElev wsc 5.0)
                         else round (scaleElev wsc 2.0)
            terrainElev = baseElev + boundaryEffect + localNoise * noiseScale
        in (terrainElev + 3, matGlacier)
    else
    let ((plateA, distA), (plateB, distB)) = twoNearestPlates seed worldSize plates gx' gy'

        myPlate = plateA
        material = plateMaterial myPlate
        baseElev = plateBaseElev myPlate

        boundaryDist = (distB - distA) / 2.0

        boundary = classifyBoundary worldSize plateA plateB

        side = SidePlateA

        boundaryEffect = boundaryElevation wsc boundary side
                           plateA plateB boundaryDist

        localNoise = elevationNoise seed worldSize gx' gy'
        noiseScale = if plateIsLand myPlate
                     then round (scaleElev wsc 50.0)
                     else round (scaleElev wsc 20.0)

        finalElev = baseElev + boundaryEffect + localNoise * noiseScale

    in (finalElev, material)

-----------------------------------------------------------
-- Boundary Elevation Profiles
-----------------------------------------------------------

boundaryElevation ∷ WorldScale → BoundaryType → BoundarySide
                  → TectonicPlate → TectonicPlate
                  → Float → Int
boundaryElevation wsc boundary _side plateA plateB boundaryDist =
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
        let peakElev = scaleElev wsc ((3000.0 + 5000.0 * s) / metersPerTile)
        in round (peakElev * t')

    else if bothOcean then
        let isSubducting = densityA > densityB
        in if isSubducting
           -- Trench: reference -2000 to -8000m → -200 to -800 tiles
           then let trenchDepth = scaleElev wsc ((2000.0 + 3000.0 * s) / metersPerTile)
                in round (negate trenchDepth * t')
           -- Island arc: reference 500m → 50 tiles
           else round (scaleElev wsc (500.0 * s / metersPerTile) * t')

    else if aIsLand ∧ not bIsLand then
        -- Andes-style: reference 2000–10000m → 200–1000 tiles
        let peakElev = scaleElev wsc ((2000.0 + 4000.0 * s) / metersPerTile)
        in round (peakElev * t')

    else
        -- Mariana-style trench: reference -3000 to -13000m → -300 to -1300 tiles
        let trenchDepth = scaleElev wsc ((3000.0 + 5000.0 * s) / metersPerTile)
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
        let ridgeHeight = scaleElev wsc ((500.0 + 1000.0 * s) / metersPerTile)
        in round (ridgeHeight * t')
    else if aIsLand then
        -- Continental rift: reference -500 to -2500m → -50 to -250 tiles
        let riftDepth = scaleElev wsc ((500.0 + 2000.0 * s) / metersPerTile)
        in round (negate riftDepth * t')
    else
        -- Ocean side of rift: reference -200 to -1000m → -20 to -100 tiles
        let depth = scaleElev wsc ((200.0 + 800.0 * s) / metersPerTile)
        in round (negate depth * t')

transformEffect ∷ WorldScale → Float → Int
transformEffect wsc boundaryDist =
    let fadeRange = scaleDist wsc 50.0
        t = max 0.0 (1.0 - abs boundaryDist / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        metersPerTile = 10.0
    in round (scaleElev wsc (100.0 / metersPerTile) * t' * (if boundaryDist > 0 then 1.0 else -1.0))

-----------------------------------------------------------
-- Local Noise
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Jitter
-----------------------------------------------------------

jitterAmount ∷ Word64 → Int → Int → Int → Float
jitterAmount seed worldSize gx gy =
    let n1 = wrappedValueNoise2D seed worldSize gx gy 20
        n2 = wrappedValueNoise2D (seed + 99) worldSize gx gy 8
        combined = n1 * 0.7 + n2 * 0.3
    in (combined - 0.5) * 80.0

-----------------------------------------------------------
-- Noise & Hash
-----------------------------------------------------------

valueNoise2D ∷ Word64 → Int → Int → Int → Float
valueNoise2D seed x y scale =
    let fx = fromIntegral x / fromIntegral scale ∷ Float
        fy = fromIntegral y / fromIntegral scale ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy
        sx = smoothstep tx
        sy = smoothstep ty
        v00 = hashToFloat' (hashCoord seed ix       iy)
        v10 = hashToFloat' (hashCoord seed (ix + 1) iy)
        v01 = hashToFloat' (hashCoord seed ix       (iy + 1))
        v11 = hashToFloat' (hashCoord seed (ix + 1) (iy + 1))
        top    = lerp sx v00 v10
        bottom = lerp sx v01 v11
    in lerp sy top bottom

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
    in lo + floor (f * fromIntegral span')

smoothstep ∷ Float → Float
smoothstep t = t * t * (3.0 - 2.0 * t)

lerp ∷ Float → Float → Float → Float
lerp t a b = a + t * (b - a)
