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
import qualified Data.HashMap.Strict as HM
import World.Types (Tile(..), ChunkCoord(..), Chunk, WorldGenParams(..))
import World.Grid (worldToGrid, tileHalfWidth, tileHalfDiamondHeight)

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

chunkSize :: Int
chunkSize = 16

-- | How many chunks around the camera chunk to keep loaded.
--   2 means a 5×5 grid (the camera chunk ± 2 in each direction).
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

-- | Determine which chunk the camera is currently in.
--   Converts Camera2D world-space position → grid coord → chunk coord.
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

generateChunk :: WorldGenParams -> ChunkCoord -> Chunk
generateChunk _params coord =
    let tiles = [ ((lx, ly, 0), Tile 1)
                | lx <- [0 .. chunkSize - 1]
                , ly <- [0 .. chunkSize - 1]
                ]
    in HM.fromList tiles
