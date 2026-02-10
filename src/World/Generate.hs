{-# LANGUAGE Strict #-}
module World.Generate
    ( -- * Generation
      generateChunk
      -- * Configuration
    , WorldGenParams(..)
    , defaultWorldGenParams
      -- * Coordinate helpers
    , chunkSize
    , globalToChunk
    , chunkToGlobal
    , chunkWorldBounds
      -- * Types re-export
    , ChunkCoord(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Types (Tile(..), ChunkCoord(..), Chunk)

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

-- | Tiles per chunk side
chunkSize :: Int
chunkSize = 16

-----------------------------------------------------------
-- Generation Parameters
-----------------------------------------------------------

-- | Pure, serializable world generation parameters.
--   Same params + same ChunkCoord = same Chunk, always.
data WorldGenParams = WorldGenParams
    { wgpSeed       :: !Word64
    , wgpWorldSize  :: !Int     -- ^ World size in chunks (e.g. 64 → 64×64 chunks)
    } deriving (Show, Eq)

defaultWorldGenParams :: WorldGenParams
defaultWorldGenParams = WorldGenParams
    { wgpSeed      = 42
    , wgpWorldSize = 64
    }

-----------------------------------------------------------
-- Coordinate Helpers
-----------------------------------------------------------

-- | Convert global tile (x, y) to (ChunkCoord, local (lx, ly)).
--   Handles negative coordinates correctly.
globalToChunk :: Int -> Int -> (ChunkCoord, (Int, Int))
globalToChunk gx gy =
    let cx = floorDiv gx chunkSize
        cy = floorDiv gy chunkSize
        lx = floorMod gx chunkSize
        ly = floorMod gy chunkSize
    in (ChunkCoord cx cy, (lx, ly))

-- | Convert (ChunkCoord, local) back to global tile coords.
chunkToGlobal :: ChunkCoord -> Int -> Int -> (Int, Int)
chunkToGlobal (ChunkCoord cx cy) lx ly =
    (cx * chunkSize + lx, cy * chunkSize + ly)

-- | Get the global tile coordinate range for a chunk.
--   Returns ((minX, minY), (maxX, maxY)) inclusive.
chunkWorldBounds :: ChunkCoord -> ((Int, Int), (Int, Int))
chunkWorldBounds (ChunkCoord cx cy) =
    let minX = cx * chunkSize
        minY = cy * chunkSize
        maxX = minX + chunkSize - 1
        maxY = minY + chunkSize - 1
    in ((minX, minY), (maxX, maxY))

-- | Floor division that works correctly for negative numbers.
--   (-1) `floorDiv` 16 = -1, not 0.
floorDiv :: Int -> Int -> Int
floorDiv a b = floor (fromIntegral a / fromIntegral b :: Double)

-- | Floor modulo that always returns [0, b).
--   (-1) `floorMod` 16 = 15, not -1.
floorMod :: Int -> Int -> Int
floorMod a b = a - floorDiv a b * b

-----------------------------------------------------------
-- Chunk Generation
-----------------------------------------------------------

-- | Generate a single chunk. Pure and deterministic.
--   For now: flat grass at z=0 for every column.
generateChunk :: WorldGenParams -> ChunkCoord -> Chunk
generateChunk _params coord =
    let ((minX, minY), (maxX, maxY)) = chunkWorldBounds coord
        tiles = [ ((lx, ly, 0), Tile 1)  -- grass at z=0
                | lx <- [0 .. chunkSize - 1]
                , ly <- [0 .. chunkSize - 1]
                ]
    in HM.fromList tiles
