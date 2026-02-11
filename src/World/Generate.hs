{-# LANGUAGE Strict #-}
module World.Generate
    ( -- * Generation
      generateChunk
      -- * Coordinate helpers
    , chunkSize
    , globalToChunk
    , chunkToGlobal
    , chunkWorldBounds
    , chunkLoadRadius
    , cameraChunkCoord
      -- * Types re-export
    , ChunkCoord(..)
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)
import qualified Data.HashMap.Strict as HM
import World.Types (Tile(..), ChunkCoord(..), Chunk, WorldGenParams(..))
import World.Grid (worldToGrid)

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

chunkSize :: Int
chunkSize = 16

chunkLoadRadius :: Int
chunkLoadRadius = 2

-----------------------------------------------------------
-- Coordinate Helpers
-----------------------------------------------------------

globalToChunk :: Int -> Int -> (ChunkCoord, (Int, Int))
globalToChunk gx gy =
    let cx = floorDiv gx chunkSize
        cy = floorDiv gy chunkSize
        lx = floorMod gx chunkSize
        ly = floorMod gy chunkSize
    in (ChunkCoord cx cy, (lx, ly))

chunkToGlobal :: ChunkCoord -> Int -> Int -> (Int, Int)
chunkToGlobal (ChunkCoord cx cy) lx ly =
    (cx * chunkSize + lx, cy * chunkSize + ly)

chunkWorldBounds :: ChunkCoord -> ((Int, Int), (Int, Int))
chunkWorldBounds (ChunkCoord cx cy) =
    let minX = cx * chunkSize
        minY = cy * chunkSize
        maxX = minX + chunkSize - 1
        maxY = minY + chunkSize - 1
    in ((minX, minY), (maxX, maxY))

cameraChunkCoord :: Float -> Float -> ChunkCoord
cameraChunkCoord camX camY =
    let (gx, gy) = worldToGrid camX camY
        (coord, _) = globalToChunk gx gy
    in coord

floorDiv :: Int -> Int -> Int
floorDiv a b = floor (fromIntegral a / fromIntegral b :: Double)

floorMod :: Int -> Int -> Int
floorMod a b = a - floorDiv a b * b

-----------------------------------------------------------
-- Chunk Generation
-----------------------------------------------------------

-- | Generate a single chunk. Pure and deterministic.
--   Samples multi-octave value noise per column to get elevation
--   in the range [-3, +3]. Each column is fully filled from
--   min(0, surface) to max(0, surface) so cliff faces are
--   visible from any camera rotation.
generateChunk :: WorldGenParams -> ChunkCoord -> Chunk
generateChunk params coord =
    let seed = wgpSeed params
        tiles = [ tile
                | lx <- [0 .. chunkSize - 1]
                , ly <- [0 .. chunkSize - 1]
                , let (gx, gy) = chunkToGlobal coord lx ly
                      surfaceZ = elevationAt seed gx gy
                , tile <- generateColumn lx ly surfaceZ
                ]
    in HM.fromList tiles

-- | Generate tiles for a single column at local (lx, ly).
--   The surface tile is at surfaceZ. Below the surface we fill
--   down far enough that cliff faces are backed by geometry
--   from any camera angle. We use a fixed fill depth rather
--   than bridging to z=0.
generateColumn :: Int -> Int -> Int -> [((Int, Int, Int), Tile)]
generateColumn lx ly surfaceZ =
    let fillDepth = 3  -- how many tiles below surface to fill for cliff faces
        lo = surfaceZ - fillDepth
    in [ ((lx, ly, z), Tile 1 0)
       | z <- [lo .. surfaceZ]
       ]

-----------------------------------------------------------
-- Elevation
-----------------------------------------------------------

-- | Multi-octave elevation at a global grid position.
--   Returns an integer z-level in [-3, +3].
--   Uses 3 octaves with different sub-seeds so they don't correlate.
elevationAt :: Word64 -> Int -> Int -> Int
elevationAt seed gx gy =
    let -- Octave 1: broad hills (scale 12 — spans ~1 chunk)
        e1 = valueNoise2D (seed + 0) gx gy 12
        -- Octave 2: medium bumps (scale 6 — a few per chunk)
        e2 = valueNoise2D (seed + 1) gx gy 6
        -- Octave 3: fine detail (scale 3 — many per chunk)
        e3 = valueNoise2D (seed + 2) gx gy 3

        -- Weighted sum, raw is in [0, 1]
        raw = e1 * 0.6 + e2 * 0.25 + e3 * 0.15

        -- Map [0, 1] → [-3, +3]
        -- 0.0 → -3, 0.5 → 0, 1.0 → +3
        mapped = (raw - 0.5) * 6.0

    in clampInt (-3) 3 (round mapped)

-- | Clamp an Int to [lo, hi]
clampInt :: Int -> Int -> Int -> Int
clampInt lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x

-----------------------------------------------------------
-- Value Noise (pure, hash-based)
-----------------------------------------------------------

-- | 2D value noise with bilinear interpolation.
--   Returns a value in [0, 1]. Pure and deterministic.
valueNoise2D :: Word64 -> Int -> Int -> Int -> Float
valueNoise2D seed x y scale =
    let -- Grid cell coordinates
        fx = fromIntegral x / fromIntegral scale :: Float
        fy = fromIntegral y / fromIntegral scale :: Float
        ix = floor fx :: Int
        iy = floor fy :: Int

        -- Fractional position within cell
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy

        -- Smooth interpolation (Hermite smoothstep)
        sx = smoothstep tx
        sy = smoothstep ty

        -- Hash at four corners of the grid cell
        v00 = hashToFloat seed ix       iy
        v10 = hashToFloat seed (ix + 1) iy
        v01 = hashToFloat seed ix       (iy + 1)
        v11 = hashToFloat seed (ix + 1) (iy + 1)

        -- Bilinear interpolation
        top    = lerp sx v00 v10
        bottom = lerp sx v01 v11

    in lerp sy top bottom

-----------------------------------------------------------
-- Hash & Math Utilities
-----------------------------------------------------------

-- | Hash two ints and a seed to a float in [0, 1].
hashToFloat :: Word64 -> Int -> Int -> Float
hashToFloat seed x y =
    let h = hashCoord seed x y
    in fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF :: Word32)

-- | Integer hash combining seed with coordinates.
--   Deterministic, good distribution (murmur3-style finalizer).
hashCoord :: Word64 -> Int -> Int -> Word32
hashCoord seed x y =
    let h0 = fromIntegral seed :: Word64
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        -- Murmur3 finalizer
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
        h6 = h5 * 0xc4ceb9fe1a85ec53
        h7 = h6 `xor` (h6 `shiftR` 33)
    in fromIntegral (h7 .&. 0xFFFFFFFF)

-- | Hermite smoothstep: 3t² - 2t³
smoothstep :: Float -> Float
smoothstep t = t * t * (3.0 - 2.0 * t)

-- | Linear interpolation
lerp :: Float -> Float -> Float -> Float
lerp t a b = a + t * (b - a)
