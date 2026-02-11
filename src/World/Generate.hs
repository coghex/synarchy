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
import World.Material (MaterialId(..), matGranite)
import World.Plate (TectonicPlate(..), generatePlates, plateAt)
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
--   Uses tectonic plates to determine base elevation and material,
--   then adds local noise for surface texture.
generateChunk :: WorldGenParams -> ChunkCoord -> Chunk
generateChunk params coord =
    let seed = wgpSeed params
        plates = generatePlates seed (wgpWorldSize params) (wgpPlateCount params)
        tiles = [ tile
                | lx <- [0 .. chunkSize - 1]
                , ly <- [0 .. chunkSize - 1]
                , let (gx, gy) = chunkToGlobal coord lx ly
                      (surfaceZ, material) = columnInfo seed plates gx gy
                , tile <- generateColumn lx ly surfaceZ (unMaterialId material)
                ]
    in HM.fromList tiles

-- | Determine surface elevation and material for a column.
--   Combines plate base elevation with local noise.
columnInfo :: Word64 -> [TectonicPlate] -> Int -> Int -> (Int, MaterialId)
columnInfo seed plates gx gy =
    let (plate, _dist) = plateAt seed plates gx gy
        baseElev = plateBaseElev plate
        material = plateMaterial plate

        -- Local noise: small variation on top of plate elevation
        -- This gives texture to the surface — not perfectly flat continents
        localNoise = elevationNoise seed gx gy
        surfaceZ = clampInt (-5) 5 (baseElev + localNoise)

    in (surfaceZ, material)

-- | Local elevation noise: adds small bumps and dips to plate surfaces.
--   Returns -1, 0, or +1 typically.
elevationNoise :: Word64 -> Int -> Int -> Int
elevationNoise seed gx gy =
    let -- Two octaves: gentle rolling + fine detail
        e1 = valueNoise2D (seed + 10) gx gy 12  -- broad
        e2 = valueNoise2D (seed + 11) gx gy 5   -- fine
        raw = e1 * 0.7 + e2 * 0.3               -- [0, 1]
        -- Map to [-1.5, +1.5] so most values round to -1, 0, or +1
        mapped = (raw - 0.5) * 3.0
    in clampInt (-2) 2 (round mapped)

-- | Generate tiles for a single column at local (lx, ly).
--   The surface tile is at surfaceZ with the given material.
--   Below the surface we fill down for cliff face visibility.
generateColumn :: Int -> Int -> Int -> Word8 -> [((Int, Int, Int), Tile)]
generateColumn lx ly surfaceZ material =
    let fillDepth = 3
        lo = surfaceZ - fillDepth
    in [ ((lx, ly, z), Tile material 0)  -- slopeId=0 (flat) for now
       | z <- [lo .. surfaceZ]
       ]

-- | Clamp an Int to [lo, hi]
clampInt :: Int -> Int -> Int -> Int
clampInt lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x

-----------------------------------------------------------
-- Value Noise (pure, hash-based)
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
        v00 = hashToFloat seed ix       iy
        v10 = hashToFloat seed (ix + 1) iy
        v01 = hashToFloat seed ix       (iy + 1)
        v11 = hashToFloat seed (ix + 1) (iy + 1)
        top    = lerp sx v00 v10
        bottom = lerp sx v01 v11
    in lerp sy top bottom

-----------------------------------------------------------
-- Hash & Math Utilities
-----------------------------------------------------------

hashToFloat :: Word64 -> Int -> Int -> Float
hashToFloat seed x y =
    let h = hashCoord seed x y
    in fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF :: Word32)

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

smoothstep :: Float -> Float
smoothstep t = t * t * (3.0 - 2.0 * t)

lerp :: Float -> Float -> Float -> Float
lerp t a b = a + t * (b - a)
