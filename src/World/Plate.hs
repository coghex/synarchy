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
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)
import Data.List (sortBy)
import Data.Ord (comparing)
import World.Material (MaterialId(..), matGranite, matDiorite, matGabbro, matGlacier)
import World.Scale (WorldScale(..), computeWorldScale, scaleElev, scaleDist)
import World.Types (chunkSize)

-----------------------------------------------------------
-- Tectonic Plate
-----------------------------------------------------------

data TectonicPlate = TectonicPlate
    { plateCenterX  ∷ !Int
    , plateCenterY  ∷ !Int
    , plateIsLand   ∷ !Bool
    , plateBaseElev ∷ !Int
    , plateMaterial ∷ !MaterialId
    , plateDensity  ∷ !Float
    , plateDriftX   ∷ !Float
    , plateDriftY   ∷ !Float
    } deriving (Show)

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

        -- Reference values (at worldSize=512):
        --   Land: 0 to 1000m    Ocean: -6000 to -3000m
        -- Scaled by worldSize/512.
        baseElev = if isLand
                   then round (scaleElev wsc (fromIntegral (hashToRange h4 0 1000)))
                   else round (scaleElev wsc (fromIntegral (hashToRange h4 (-6000) (-3000))))

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
    let ranked = rankPlates seed worldSize plates gx gy
    in case ranked of
        (a:b:_) → (a, b)
        [a]     → (a, a)
        _       → error "no plates"

rankPlates ∷ Word64 → Int → [TectonicPlate] → Int → Int → [(TectonicPlate, Float)]
rankPlates seed worldSize plates gx gy =
    let jitter = jitterAmount seed worldSize gx gy
        withDist plate =
            let dx = fromIntegral (wrappedDeltaX worldSize gx (plateCenterX plate)) ∷ Float
                dy = fromIntegral (gy - plateCenterY plate) ∷ Float
            in (plate, sqrt (dx * dx + dy * dy) + jitter)
    in sortBy (comparing snd) (map withDist plates)

-----------------------------------------------------------
-- Boundary Classification
-----------------------------------------------------------

classifyBoundary ∷ Int → TectonicPlate → TectonicPlate → BoundaryType
classifyBoundary worldSize plateA plateB =
    let nxRaw = fromIntegral (wrappedDeltaX worldSize
                    (plateCenterX plateA) (plateCenterX plateB)) ∷ Float
        nyRaw = fromIntegral (plateCenterY plateB - plateCenterY plateA) ∷ Float
        nLen  = sqrt (nxRaw * nxRaw + nyRaw * nyRaw)
        (nx, ny) = if nLen > 0.001
                   then (nxRaw / nLen, nyRaw / nLen)
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

wrapGlobalX ∷ Int → Int → Int
wrapGlobalX worldSize gx =
    let w = worldWidthTiles worldSize
        halfW = w `div` 2
        wrapped = ((gx + halfW) `mod` w + w) `mod` w - halfW
    in wrapped

wrappedDeltaX ∷ Int → Int → Int → Int
wrappedDeltaX worldSize x1 x2 =
    let w = worldWidthTiles worldSize
        raw = x2 - x1
        halfW = w `div` 2
        wrapped = ((raw + halfW) `mod` w + w) `mod` w - halfW
    in wrapped

wrappedValueNoise2D ∷ Word64 → Int → Int → Int → Int → Float
wrappedValueNoise2D seed worldSize x y scale =
    let w = worldWidthTiles worldSize
        fx = fromIntegral x / fromIntegral scale ∷ Float
        fy = fromIntegral y / fromIntegral scale ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy
        sx = smoothstep tx
        sy = smoothstep ty
        cellsInX = w `div` scale
        wrapIx i = ((i `mod` cellsInX) + cellsInX) `mod` cellsInX
        ix0 = wrapIx ix
        ix1 = wrapIx (ix + 1)
        v00 = hashToFloat' (hashCoord seed ix0     iy)
        v10 = hashToFloat' (hashCoord seed ix1     iy)
        v01 = hashToFloat' (hashCoord seed ix0     (iy + 1))
        v11 = hashToFloat' (hashCoord seed ix1     (iy + 1))
        top    = lerp sx v00 v10
        bottom = lerp sx v01 v11
    in lerp sy top bottom

-----------------------------------------------------------
-- Global Elevation Query
-----------------------------------------------------------

elevationAtGlobal ∷ Word64 → [TectonicPlate] → Int → Int → Int → (Int, MaterialId)
elevationAtGlobal seed plates worldSize gx gy =
    let gx' = wrapGlobalX worldSize gx
        wsc = computeWorldScale worldSize
    in if isBeyondGlacier worldSize gx' gy then (0, matGlacier)
    else if isGlacierZone worldSize gx' gy then
        let ((plateA, distA), (plateB, distB)) = twoNearestPlates seed worldSize plates gx' gy
            myPlate = plateA
            baseElev = plateBaseElev myPlate
            boundaryDist = (distB - distA) / 2.0
            boundary = classifyBoundary worldSize plateA plateB
            side = SidePlateA
            boundaryEffect = boundaryElevation wsc boundary side plateA plateB boundaryDist
            localNoise = elevationNoise seed worldSize gx' gy
            noiseScale = if plateIsLand myPlate
                         then round (scaleElev wsc 50.0)
                         else round (scaleElev wsc 20.0)
            terrainElev = baseElev + boundaryEffect + localNoise * noiseScale
        in (terrainElev + 3, matGlacier)
    else
    let ((plateA, distA), (plateB, distB)) = twoNearestPlates seed worldSize plates gx' gy

        myPlate = plateA
        material = plateMaterial myPlate
        baseElev = plateBaseElev myPlate

        boundaryDist = (distB - distA) / 2.0

        boundary = classifyBoundary worldSize plateA plateB

        side = SidePlateA

        boundaryEffect = boundaryElevation wsc boundary side
                           plateA plateB boundaryDist

        localNoise = elevationNoise seed worldSize gx' gy
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
    let -- Scale horizontal distances with world size
        fadeRange = scaleDist wsc 200.0
        peakRange = scaleDist wsc 30.0
        t = max 0.0 (1.0 - max 0.0 (abs boundaryDist - peakRange) / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        -- Clamp strength to prevent runaway values
        s = min 2.0 (abs strength)

    in if bothLand then
        -- Land-land convergent: mountain range (Himalayas)
        -- Reference: 3000 + 5000*s = 3000-13000m peaks
        let peakElev = scaleElev wsc (3000.0 + 5000.0 * s)
        in round (peakElev * t')

    else if bothOcean then
        let isSubducting = densityA > densityB
        in if isSubducting
           -- Trench: reference -2000 to -8000m
           then let trenchDepth = scaleElev wsc (2000.0 + 3000.0 * s)
                in round (negate trenchDepth * t')
           -- Island arc: reference 500*s uplift
           else round (scaleElev wsc (500.0 * s) * t')

    else if aIsLand ∧ not bIsLand then
        -- Andes-style: reference 2000 + 4000*s
        let peakElev = scaleElev wsc (2000.0 + 4000.0 * s)
        in round (peakElev * t')

    else
        -- Mariana-style trench: reference -3000 to -13000m
        let trenchDepth = scaleElev wsc (3000.0 + 5000.0 * s)
        in round (negate trenchDepth * t')

-- | Divergent boundary elevation effect.
divergentEffect ∷ WorldScale → Bool → Bool → Float → Float → Int
divergentEffect wsc aIsLand bothOcean strength boundaryDist =
    let fadeRange = scaleDist wsc 150.0
        peakRange = scaleDist wsc 20.0
        t = max 0.0 (1.0 - max 0.0 (abs boundaryDist - peakRange) / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        s = min 2.0 (abs strength)
    in if bothOcean then
        -- Mid-ocean ridge: reference 500 + 1000*s
        let ridgeHeight = scaleElev wsc (500.0 + 1000.0 * s)
        in round (ridgeHeight * t')
    else if aIsLand then
        -- Continental rift: reference -500 to -2500m
        let riftDepth = scaleElev wsc (500.0 + 2000.0 * s)
        in round (negate riftDepth * t')
    else
        -- Ocean side of rift: reference -200 to -1000m
        let depth = scaleElev wsc (200.0 + 800.0 * s)
        in round (negate depth * t')

-- | Transform boundary: minimal elevation effect.
transformEffect ∷ WorldScale → Float → Int
transformEffect wsc boundaryDist =
    let fadeRange = scaleDist wsc 50.0
        t = max 0.0 (1.0 - abs boundaryDist / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        -- Reference: ±100m ridges/valleys
    in round (scaleElev wsc 100.0 * t' * (if boundaryDist > 0 then 1.0 else -1.0))

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
