{-# LANGUAGE Strict #-}
module World.Generate.Coordinates
    ( globalToChunk
    , chunkToGlobal
    , chunkWorldBounds
    , cameraChunkCoord
    , canonicalTileFrame
    ) where

import UPrelude
import World.Types
import World.Grid (worldToGrid)
import Engine.Graphics.Camera (CameraFacing(..))

-- * Coordinate Helpers

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

-- * Canonical (u-wrapped) tile frame — #1135
--
--   Chunks are STORED under u-wrapped coords ('wrapChunkCoordU', applied
--   at insert time by "World.Thread.ChunkLoading"). A tile coord that
--   was not itself read out of stored world data can therefore name an
--   ALIAS of a loaded chunk: same physical tile, coord one whole world
--   away along u. 'canonicalTileFrame' moves such a coord into the
--   stored frame, and is the identity for a coord already in it (and for
--   arena / zero-size worlds, where 'wrapChunkCoordU' never wraps).

-- | Resolve a global tile coord to the canonical chunk that STORES it,
--   its local index within that chunk, and the whole-chunk @(dgx, dgy)@
--   shift carrying any coord from the raw frame into the stored one.
--
--   The shift is the u-wrap expressed in tiles: it moves whole chunks,
--   so the local index is the same on both sides, and it preserves
--   @v = gx + gy@ exactly as 'wrapChunkCoordU' preserves @v = cx + cy@.
canonicalTileFrame ∷ Int                -- ^ world size in chunks
                   → Int → Int          -- ^ global tile (gx, gy), raw frame
                   → (ChunkCoord, (Int, Int), (Int, Int))
                      -- ^ (stored chunk, local (lx, ly), tile shift (dgx, dgy))
canonicalTileFrame worldSize gx gy =
    let (ccRaw@(ChunkCoord rcx rcy), local) = globalToChunk gx gy
        ChunkCoord ccx ccy = wrapChunkCoordU worldSize ccRaw
    in ( ChunkCoord ccx ccy
       , local
       , ((ccx - rcx) * chunkSize, (ccy - rcy) * chunkSize) )

floorMod ∷ Int → Int → Int
floorMod a b = a - div a b * b
