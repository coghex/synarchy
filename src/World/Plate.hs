{-# LANGUAGE Strict #-}
module World.Plate
    ( -- * Plate data
      TectonicPlate(..)
    , generatePlates
      -- * Queries
    , plateAt
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

-- | A tectonic plate with a center position and properties.
--   All fields are determined purely from seed + plate index.
data TectonicPlate = TectonicPlate
    { plateCenterX  :: !Int       -- ^ Center X in global tile coords
    , plateCenterY  :: !Int       -- ^ Center Y in global tile coords
    , plateIsLand   :: !Bool      -- ^ True = continental, False = oceanic
    , plateBaseElev :: !Int       -- ^ Base elevation (-3 to +3)
    , plateMaterial :: !MaterialId -- ^ Primary rock type
    , plateDriftX   :: !Float     -- ^ Drift direction X (-1 to 1), for future boundary calc
    , plateDriftY   :: !Float     -- ^ Drift direction Y (-1 to 1), for future boundary calc
    } deriving (Show)

-----------------------------------------------------------
-- Plate Generation
-----------------------------------------------------------

-- | Generate N tectonic plates from seed and world size.
--   World size is in chunks, each chunk is 16 tiles,
--   so total tile span = worldSize * 16, centered on origin.
generatePlates :: Word64 -> Int -> Int -> [TectonicPlate]
generatePlates seed worldSize plateCount =
    map (generateOnePlate seed worldSize) [0 .. plateCount - 1]

generateOnePlate :: Word64 -> Int -> Int -> TectonicPlate
generateOnePlate seed worldSize plateIndex =
    let halfTiles = (worldSize * 16) `div` 2
        -- Deterministic "random" values from seed + plate index
        -- Each property uses a different sub-seed so they don't correlate
        h1 = plateHash seed plateIndex 1
        h2 = plateHash seed plateIndex 2
        h3 = plateHash seed plateIndex 3
        h4 = plateHash seed plateIndex 4
        h5 = plateHash seed plateIndex 5
        h6 = plateHash seed plateIndex 6

        -- Place center within world bounds
        -- Use the hash to get a position in [-halfTiles, halfTiles)
        cx = hashToRange h1 (-halfTiles) halfTiles
        cy = hashToRange h2 (-halfTiles) halfTiles

        -- ~40% of plates are oceanic, 60% continental
        isLand = hashToFloat' h3 > 0.4

        -- Base elevation: land = 0 to 2, ocean = -3 to -1
        baseElev = if isLand
                   then hashToRange h4 0 2
                   else hashToRange h4 (-3) (-1)

        -- Material: pick from the three igneous types
        matChoice = hashToRange h5 0 2
        material = case matChoice of
            0 -> matGranite
            1 -> matDiorite
            _ -> matGabbro

        -- Drift direction (unit-ish vector, for future plate boundaries)
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

-- | Find which plate a global tile position belongs to.
--   Uses jittered Voronoi: distance to each plate center is
--   perturbed by noise so boundaries are organic, not straight.
--
--   Returns (plate, distance-to-center) for use in boundary effects.
plateAt :: Word64 -> [TectonicPlate] -> Int -> Int -> (TectonicPlate, Float)
plateAt seed plates gx gy =
    let jitter = jitterAmount seed gx gy
        distTo plate =
            let dx = fromIntegral (gx - plateCenterX plate) :: Float
                dy = fromIntegral (gy - plateCenterY plate) :: Float
            in sqrt (dx * dx + dy * dy) + jitter
    in minimumBy (comparing snd) [(plate, distTo plate) | plate <- plates]

-- | Noise-based jitter so plate boundaries aren't straight lines.
--   Returns a value in [-40, 40] tile units of boundary wobble.
jitterAmount :: Word64 -> Int -> Int -> Float
jitterAmount seed gx gy =
    let -- Two octaves of noise for organic-looking boundaries
        n1 = valueNoise2D seed gx gy 20    -- broad wobble
        n2 = valueNoise2D (seed + 99) gx gy 8  -- fine wobble
        combined = n1 * 0.7 + n2 * 0.3      -- [0, 1]
    in (combined - 0.5) * 80.0              -- [-40, 40]

-----------------------------------------------------------
-- Noise & Hash (same as Generate.hs, duplicated for
-- module independence — could be factored to World.Noise)
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

-- | Hash for plate property generation.
--   Combines seed, plate index, and property index.
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

-- | Map a hash to an Int in [lo, hi] inclusive
hashToRange :: Word32 -> Int -> Int -> Int
hashToRange h lo hi =
    let f = hashToFloat' h   -- [0, 1)
        span' = hi - lo + 1
    in lo + floor (f * fromIntegral span')

smoothstep :: Float -> Float
smoothstep t = t * t * (3.0 - 2.0 * t)

lerp :: Float -> Float -> Float -> Float
lerp t a b = a + t * (b - a)
