{-# LANGUAGE Strict #-}
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

-----------------------------------------------------------
-- Tectonic Plate
-----------------------------------------------------------

data TectonicPlate = TectonicPlate
    { plateCenterX  :: !Int
    , plateCenterY  :: !Int
    , plateIsLand   :: !Bool
    , plateBaseElev :: !Int
    , plateMaterial :: !MaterialId
    , plateDensity  :: !Float     -- ^ Crust density, determines subduction
    , plateDriftX   :: !Float
    , plateDriftY   :: !Float
    } deriving (Show)

-----------------------------------------------------------
-- Boundary Classification
-----------------------------------------------------------

data BoundaryType
    = Convergent !Float    -- ^ approach speed (positive = converging faster)
    | Divergent  !Float    -- ^ separation speed
    | Transform  !Float    -- ^ shear magnitude
    deriving (Show)

-- | Which side of the boundary this column is on.
data BoundarySide = SidePlateA | SidePlateB
    deriving (Show, Eq)

-----------------------------------------------------------
-- Plate Generation
-----------------------------------------------------------

generatePlates :: Word64 -> Int -> Int -> [TectonicPlate]
generatePlates seed worldSize plateCount =
    map (generateOnePlate seed worldSize) [0 .. plateCount - 1]

generateOnePlate :: Word64 -> Int -> Int -> TectonicPlate
generateOnePlate seed worldSize plateIndex =
    let halfTiles = (worldSize * 16) `div` 2
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

        -- ~40% oceanic, 60% continental
        isLand = hashToFloat' h3 > 0.4

        -- Land: 0 to 1000m, Ocean: -6000 to -3000m
        baseElev = if isLand
                   then hashToRange h4 0 1000
                   else hashToRange h4 (-6000) (-3000)

        matChoice = hashToRange h5 0 2
        material = case matChoice of
            0 -> matGranite
            1 -> matDiorite
            _ -> matGabbro

        -- Crust density: ocean crust is denser than continental
        density = if isLand
                  then 2.7 + hashToFloat' h8 * 0.2    -- 2.7 - 2.9
                  else 3.0 + hashToFloat' h8 * 0.3    -- 3.0 - 3.3

        -- Drift direction (not normalized — magnitude implies speed)
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

plateAt :: Word64 -> Int -> [TectonicPlate] -> Int -> Int -> (TectonicPlate, Float)
plateAt seed worldSize plates gx gy =
    let ranked = rankPlates seed worldSize plates gx gy
    in head ranked

twoNearestPlates :: Word64 -> Int -> [TectonicPlate] -> Int -> Int
                 -> ((TectonicPlate, Float), (TectonicPlate, Float))
twoNearestPlates seed worldSize plates gx gy =
    let ranked = rankPlates seed worldSize plates gx gy
    in case ranked of
        (a:b:_) -> (a, b)
        [a]     -> (a, a)
        _       -> error "no plates"

rankPlates :: Word64 -> Int -> [TectonicPlate] -> Int -> Int -> [(TectonicPlate, Float)]
rankPlates seed worldSize plates gx gy =
    let jitter = jitterAmount seed worldSize gx gy
        withDist plate =
            let dx = fromIntegral (wrappedDeltaX worldSize gx (plateCenterX plate)) :: Float
                dy = fromIntegral (gy - plateCenterY plate) :: Float
            in (plate, sqrt (dx * dx + dy * dy) + jitter)
    in sortBy (comparing snd) (map withDist plates)

-----------------------------------------------------------
-- Boundary Classification
-----------------------------------------------------------

-- | Classify the boundary between two plates.
--   Uses the relative drift vectors projected onto the boundary normal.
classifyBoundary :: Int -> TectonicPlate -> TectonicPlate -> BoundaryType
classifyBoundary worldSize plateA plateB =
    let nxRaw = fromIntegral (wrappedDeltaX worldSize
                    (plateCenterX plateA) (plateCenterX plateB)) :: Float
        nyRaw = fromIntegral (plateCenterY plateB - plateCenterY plateA) :: Float
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

-- | How many tile-rows wide the glacier zone is at each pole.
--   Scales proportionally with world size.
--   At worldSize=128 this gives 16 (one chunk), preserving original behavior.
glacierWidthRows :: Int
glacierWidthRows = 16

-- | Check if a global tile position is in the glacier zone.
--   The glacier border runs horizontally on screen (constant screenY).
--   In grid space, screenY is proportional to (gx + gy).
--   The world extends from roughly -(worldSize*16) to +(worldSize*16)
--   in the (gx+gy) axis, so we place the glacier at the extremes.
isGlacierZone :: Int -> Int -> Int -> Bool
isGlacierZone worldSize gx gy =
    let halfTiles = (worldSize * 16) `div` 2
        glacierEdge = halfTiles - glacierWidthRows
        screenRow = gx + gy
    in abs screenRow >= glacierEdge

-- | True if this tile is completely outside the playable world
--   (past the glacier border). These tiles should not be generated.

isBeyondGlacier :: Int -> Int -> Int -> Bool
isBeyondGlacier worldSize gx gy =
    let halfTiles = (worldSize * 16) `div` 2
        screenRow = gx + gy
    in abs screenRow > halfTiles

-----------------------------------------------------------
-- Cylindrical Wrapping
-----------------------------------------------------------

-- | Total width of the world in tiles (for X-axis wrapping).
worldWidthTiles :: Int -> Int
worldWidthTiles worldSize = worldSize * 16

-- | Wrap a global X coordinate into the valid range [-halfTiles, halfTiles).
wrapGlobalX :: Int -> Int -> Int
wrapGlobalX worldSize gx =
    let w = worldWidthTiles worldSize
        halfW = w `div` 2
        -- Shift into [0, w) then back to [-halfW, halfW)
        wrapped = ((gx + halfW) `mod` w + w) `mod` w - halfW
    in wrapped

-- | Compute the shortest wrapped distance in X between two points.
wrappedDeltaX :: Int -> Int -> Int -> Int
wrappedDeltaX worldSize x1 x2 =
    let w = worldWidthTiles worldSize
        raw = x2 - x1
        -- Shift into [-w/2, w/2)
        halfW = w `div` 2
        wrapped = ((raw + halfW) `mod` w + w) `mod` w - halfW
    in wrapped

-- | Value noise that tiles seamlessly in X with period = worldWidthTiles.
--   Works by wrapping the noise grid cell X index so that cells at the
--   seam boundary reference the same hash values.
wrappedValueNoise2D :: Word64 -> Int -> Int -> Int -> Int -> Float
wrappedValueNoise2D seed worldSize x y scale =
    let w = worldWidthTiles worldSize
        fx = fromIntegral x / fromIntegral scale :: Float
        fy = fromIntegral y / fromIntegral scale :: Float
        ix = floor fx :: Int
        iy = floor fy :: Int
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy
        sx = smoothstep tx
        sy = smoothstep ty
        -- Wrap the noise cell X coordinates so they tile
        cellsInX = w `div` scale  -- how many noise cells span the world
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

-- | Pure elevation query for any global tile position.
--   Returns (surfaceZ, materialId).
--   Glacier zones at north/south edges override normal plate generation.
elevationAtGlobal :: Word64 -> [TectonicPlate] -> Int -> Int -> Int -> (Int, MaterialId)
elevationAtGlobal seed plates worldSize gx gy =
    let gx' = wrapGlobalX worldSize gx
    in if isBeyondGlacier worldSize gx' gy then (0, matGlacier)
    else if isGlacierZone worldSize gx' gy then
        let ((plateA, distA), (plateB, distB)) = twoNearestPlates seed worldSize plates gx' gy
            myPlate = plateA
            baseElev = plateBaseElev myPlate
            boundaryDist = (distB - distA) / 2.0
            boundary = classifyBoundary worldSize plateA plateB
            side = SidePlateA
            boundaryEffect = boundaryElevation boundary side plateA plateB boundaryDist
            localNoise = elevationNoise seed worldSize gx' gy
            noiseScale = if plateIsLand myPlate then 50 else 20
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

        boundaryEffect = boundaryElevation boundary side
                           plateA plateB boundaryDist

        localNoise = elevationNoise seed worldSize gx' gy
        noiseScale = if plateIsLand myPlate then 50 else 20

        finalElev = baseElev + boundaryEffect + localNoise * noiseScale

    in (finalElev, material)

-----------------------------------------------------------
-- Boundary Elevation Profiles
-----------------------------------------------------------

-- | Compute the elevation modification from a plate boundary.
--   boundaryDist: distance from the boundary (0 = at boundary, positive = inside plateA)
boundaryElevation :: BoundaryType -> BoundarySide
                  -> TectonicPlate -> TectonicPlate
                  -> Float -> Int
boundaryElevation boundary _side plateA plateB boundaryDist =
    let bothLand  = plateIsLand plateA && plateIsLand plateB
        bothOcean = not (plateIsLand plateA) && not (plateIsLand plateB)
        -- oceanMeetsLand: plateA is always the one we're on
        aIsLand   = plateIsLand plateA
        bIsLand   = plateIsLand plateB
    in case boundary of

        -- CONVERGENT: plates pushing together
        Convergent strength -> convergentEffect
            aIsLand bIsLand bothLand bothOcean
            (plateDensity plateA) (plateDensity plateB)
            strength boundaryDist

        -- DIVERGENT: plates pulling apart
        Divergent strength -> divergentEffect
            aIsLand bothOcean strength boundaryDist

        -- TRANSFORM: plates sliding past each other
        Transform _shear -> transformEffect boundaryDist

-- | Convergent boundary elevation effect.
convergentEffect :: Bool -> Bool -> Bool -> Bool
                 -> Float -> Float -> Float -> Float -> Int
convergentEffect aIsLand bIsLand bothLand bothOcean
                 densityA densityB strength boundaryDist =
    let -- Smooth falloff: 1.0 at boundary, 0.0 at fadeRange
        fadeRange = 200.0  -- tiles over which the effect fades
        peakRange = 30.0   -- tiles of peak elevation near boundary
        t = max 0.0 (1.0 - max 0.0 (abs boundaryDist - peakRange) / fadeRange)
        -- Smooth the falloff
        t' = t * t * (3.0 - 2.0 * t)

    in if bothLand then
        -- Land-land convergent: mountain range on both sides
        -- Neither subducts easily, both push up
        -- Peak: 3000 - 8000m depending on strength
        let peakElev = 3000 + round (5000.0 * strength * t')
        in round (fromIntegral peakElev * t')

    else if bothOcean then
        -- Ocean-ocean: denser plate subducts
        -- Trench on the subducting side, island arc on the other
        let isSubducting = densityA > densityB
        in if isSubducting
           -- We're the denser plate: trench
           then let trenchDepth = -2000 - round (3000.0 * strength)
                in round (fromIntegral trenchDepth * t')
           -- We're the overriding plate: slight uplift (island arc)
           else round (500.0 * strength * t')

    else if aIsLand && not bIsLand then
        -- We're on the land plate, ocean is subducting under us
        -- Mountain range on land side (Andes-style)
        let peakElev = 2000 + round (4000.0 * strength)
        in round (fromIntegral peakElev * t')

    else
        -- We're on the ocean plate, land is the other
        -- Deep trench on ocean side (Mariana-style)
        let trenchDepth = -3000 - round (5000.0 * strength)
        in round (fromIntegral trenchDepth * t')

-- | Divergent boundary elevation effect.
--   Rift valley / mid-ocean ridge.
divergentEffect :: Bool -> Bool -> Float -> Float -> Int
divergentEffect aIsLand bothOcean strength boundaryDist =
    let fadeRange = 150.0
        peakRange = 20.0
        t = max 0.0 (1.0 - max 0.0 (abs boundaryDist - peakRange) / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
    in if bothOcean then
        -- Mid-ocean ridge: slight uplift at boundary
        let ridgeHeight = 500 + round (1000.0 * strength)
        in round (fromIntegral ridgeHeight * t')
    else if aIsLand then
        -- Continental rift valley: depression
        let riftDepth = -500 - round (2000.0 * strength)
        in round (fromIntegral riftDepth * t')
    else
        -- Ocean side of a rift: gentle slope down
        let depth = -200 - round (800.0 * strength)
        in round (fromIntegral depth * t')

-- | Transform boundary: minimal elevation effect.
--   Slight disruption near the fault line.
transformEffect :: Float -> Int
transformEffect boundaryDist =
    let fadeRange = 50.0
        t = max 0.0 (1.0 - abs boundaryDist / fadeRange)
        t' = t * t * (3.0 - 2.0 * t)
        -- Minor ridges/valleys: +/- 100m
    in round (100.0 * t' * (if boundaryDist > 0 then 1.0 else -1.0))

-----------------------------------------------------------
-- Local Noise
-----------------------------------------------------------

-- | Local elevation noise, wrapping in X.
elevationNoise :: Word64 -> Int -> Int -> Int -> Int
elevationNoise seed worldSize gx gy =
    let e1 = wrappedValueNoise2D (seed + 10) worldSize gx gy 12
        e2 = wrappedValueNoise2D (seed + 11) worldSize gx gy 5
        raw = e1 * 0.7 + e2 * 0.3
        mapped = (raw - 0.5) * 3.0
    in clampInt (-2) 2 (round mapped)

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x

-----------------------------------------------------------
-- Jitter
-----------------------------------------------------------

jitterAmount :: Word64 -> Int -> Int -> Int -> Float
jitterAmount seed worldSize gx gy =
    let n1 = wrappedValueNoise2D seed worldSize gx gy 20
        n2 = wrappedValueNoise2D (seed + 99) worldSize gx gy 8
        combined = n1 * 0.7 + n2 * 0.3
    in (combined - 0.5) * 80.0

-----------------------------------------------------------
-- Noise & Hash
-----------------------------------------------------------

valueNoise2D :: Word64 -> Int -> Int -> Int -> Float
valueNoise2D seed x y scale =
    let fx = fromIntegral x / fromIntegral scale :: Float
        fy = fromIntegral y / fromIntegral scale :: Float
        ix = floor fx :: Int
        iy = floor fy :: Int
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

plateHash :: Word64 -> Int -> Int -> Word32
plateHash seed plateIdx propIdx =
    hashCoord (seed + fromIntegral propIdx * 7919) plateIdx propIdx

hashCoord :: Word64 -> Int -> Int -> Word32
hashCoord seed x y =
    let h0 = fromIntegral seed :: Word64
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
        h6 = h5 * 0xc4ceb9fe1a85ec53
        h7 = h6 `xor` (h6 `shiftR` 33)
    in fromIntegral (h7 .&. 0xFFFFFFFF)

hashToFloat' :: Word32 -> Float
hashToFloat' h = fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF :: Word32)

hashToRange :: Word32 -> Int -> Int -> Int
hashToRange h lo hi =
    let f = hashToFloat' h
        span' = hi - lo + 1
    in lo + floor (f * fromIntegral span')

smoothstep :: Float -> Float
smoothstep t = t * t * (3.0 - 2.0 * t)

lerp :: Float -> Float -> Float -> Float
lerp t a b = a + t * (b - a)
