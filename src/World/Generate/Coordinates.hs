{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Coordinates
    ( globalToChunk
    , chunkToGlobal
    , chunkWorldBounds
    , cameraChunkCoord
    ) where

import UPrelude
import World.Types
import World.Grid (worldToGrid)
import Engine.Graphics.Camera (CameraFacing(..))

-----------------------------------------------------------
-- Coordinate Helpers
-----------------------------------------------------------

globalToChunk ∷ Int → Int → (ChunkCoord, (Int, Int))
globalToChunk gx gy =
    let cx = div gx chunkSize
        cy = div gy chunkSize
        lx = floorMod gx chunkSize
        ly = floorMod gy chunkSize
    in (ChunkCoord cx cy, (lx, ly))

chunkToGlobal ∷ ChunkCoord → Int → Int → (Int, Int)
chunkToGlobal (ChunkCoord cx cy) lx ly =
    (cx * chunkSize + lx, cy * chunkSize + ly)

chunkWorldBounds ∷ ChunkCoord → ((Int, Int), (Int, Int))
chunkWorldBounds (ChunkCoord cx cy) =
    let minX = cx * chunkSize
        minY = cy * chunkSize
        maxX = minX + chunkSize - 1
        maxY = minY + chunkSize - 1
    in ((minX, minY), (maxX, maxY))

cameraChunkCoord ∷ CameraFacing → Float → Float → ChunkCoord
cameraChunkCoord facing camX camY =
    let (gx, gy) = worldToGrid facing camX camY
        (coord, _) = globalToChunk gx gy
    in coord

floorMod ∷ Int → Int → Int
floorMod a b = a - div a b * b
