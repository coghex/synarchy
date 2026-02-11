{-# LANGUAGE Strict #-}
module World.Plate
    ( -- * Plate data
      TectonicPlate(..)
    , generatePlates
      -- * Queries
    , plateAt
    , elevationAtGlobal
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)
import Data.List (minimumBy)
import Data.Ord (comparing)
import World.Material (MaterialId(..), matGranite, matDiorite, matGabbro)

-----------------------------------------------------------
-- Tectonic Plate
-----------------------------------------------------------

data TectonicPlate = TectonicPlate
    { plateCenterX  :: !Int
    , plateCenterY  :: !Int
    , plateIsLand   :: !Bool
    , plateBaseElev :: !Int
    , plateMaterial :: !MaterialId
    , plateDriftX   :: !Float
    , plateDriftY   :: !Float
    } deriving (Show)

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

        cx = hashToRange h1 (-halfTiles) halfTiles
        cy = hashToRange h2 (-halfTiles) halfTiles

        isLand = hashToFloat' h3 > 0.4

        baseElev = if isLand
                   then hashToRange h4 0 2
                   else hashToRange h4 (-3) (-1)

        matChoice = hashToRange h5 0 2
        material = case matChoice of
            0 -> matGranite
            1 -> matDiorite
            _ -> matGabbro

        driftX = hashToFloat' h6 * 2.0 - 1.0
        driftY = hashToFloat' (plateHash seed plateIndex 7) * 2.0 - 1.0

    in TectonicPlate
        { plateCenterX  = cx
        , plateCenterY  = cy
        , plateIsLand   = isLand
        , plateBaseElev = baseElev
        , plateMaterial = material
        , plateDriftX   = driftX
        , plateDriftY   = driftY
        }

-----------------------------------------------------------
-- Plate Query
-----------------------------------------------------------

plateAt :: Word64 -> [TectonicPlate] -> Int -> Int -> (TectonicPlate, Float)
plateAt seed plates gx gy =
    let jitter = jitterAmount seed gx gy
        distTo plate =
            let dx = fromIntegral (gx - plateCenterX plate) :: Float
                dy = fromIntegral (gy - plateCenterY plate) :: Float
            in sqrt (dx * dx + dy * dy) + jitter
    in minimumBy (comparing snd) [(plate, distTo plate) | plate <- plates]

-----------------------------------------------------------
-- Global Elevation Query
-----------------------------------------------------------

-- | Pure elevation query for any global tile position.
--   Returns (surfaceZ, materialId).
--   Works across chunk boundaries — used for neighbor lookups.
elevationAtGlobal :: Word64 -> [TectonicPlate] -> Int -> Int -> (Int, MaterialId)
elevationAtGlobal seed plates gx gy =
    let (plate, _dist) = plateAt seed plates gx gy
        baseElev = plateBaseElev plate
        material = plateMaterial plate
        localNoise = elevationNoise seed gx gy
    in (baseElev + localNoise, material)

-- | Local elevation noise: adds small variation to plate surfaces.
elevationNoise :: Word64 -> Int -> Int -> Int
elevationNoise seed gx gy =
    let e1 = valueNoise2D (seed + 10) gx gy 12
        e2 = valueNoise2D (seed + 11) gx gy 5
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

jitterAmount :: Word64 -> Int -> Int -> Float
jitterAmount seed gx gy =
    let n1 = valueNoise2D seed gx gy 20
        n2 = valueNoise2D (seed + 99) gx gy 8
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
