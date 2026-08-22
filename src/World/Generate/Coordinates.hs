{-# LANGUAGE Strict #-}
module World.Generate.Coordinates
    ( globalToChunk
    , chunkToGlobal
    , chunkWorldBounds
    , cameraChunkCoord
    , canonicalTileFrame
    , canonicalTile
    , tileAliasStep
    , localizeTileToAnchor
    , seamTileDist2
    , chunkInSeamRegion
    ) where

import UPrelude
-- Imports here are deliberately NARROW (#1113): the "World.Types"
-- umbrella re-exports "World.Generate.Types", which reaches
-- "World.Magma.Init" and through it Field/Lookup, so importing it
-- would make this module unreachable from the very consumers
-- 'globalToChunk' exists to serve. Keep this at "World.Chunk.Types",
-- which owns 'chunkSize' and the seam wrap, and keep "World.Grid"
-- narrow for the same reason.
import World.Chunk.Types (ChunkCoord(..), chunkSize, wrapChunkCoordU)
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

-- | The canonical (stored-frame) image of a global tile coord: the coord
--   naming the same physical tile inside the u range chunks are stored
--   under. Identity for a coord already there, and for arena / zero-size
--   worlds.
canonicalTile ∷ Int → Int → Int → (Int, Int)
canonicalTile worldSize gx gy =
    let (_, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
    in (gx + dgx, gy + dgy)

-- * Anchor-local alias frame — #1175
--
--   Canonical coords are the STORAGE frame, not a usable frame for
--   GEOMETRY. Two physically adjacent tiles straddling the seam sit at
--   opposite ends of the canonical u range, so a @min@/@max@ over them
--   spans the whole world. Rectangle formation therefore happens in the
--   alias frame LOCAL to the drag's anchor ('localizeTileToAnchor'), and
--   each enumerated tile is canonicalised only at lookup / storage.

-- | One u-alias step expressed in TILES. Shifting u by the wrap period
--   @w@ moves a chunk by @(±w/2, ∓w/2)@; this is that half-period in
--   tiles. Zero for a non-wrapping (arena / zero-size) world.
tileAliasStep ∷ Int → Int
tileAliasStep worldSize = (worldSize `div` 2) * chunkSize

-- | Express a tile coord in the u-alias frame nearest an anchor: the
--   alias with the minimum tile-space Chebyshev distance to the anchor,
--   an exact tie keeping the coord as supplied.
--
--   This is the 'wrapChunkCoordU' topology at tile granularity — the
--   same three-image search 'World.Chunk.Types.chunkSeamChebyshev' does
--   over chunks, since @v = gx + gy@ is glacier-bounded and never wraps.
--   Identity away from the seam and for non-wrapping worlds.
localizeTileToAnchor ∷ Int              -- ^ world size in chunks
                     → (Int, Int)       -- ^ anchor, defines the local frame
                     → (Int, Int)       -- ^ coord to re-express
                     → (Int, Int)
localizeTileToAnchor worldSize (ax, ay) b@(bx, by)
    | step ≤ 0  = b
    | otherwise = snd (foldl' better (cheb b, b) [ alias k | k ← [-1, 1] ])
  where
    step = tileAliasStep worldSize
    alias k = (bx + k * step, by - k * step)
    cheb (cx, cy) = max (abs (cx - ax)) (abs (cy - ay))
    -- Strict improvement only, so an exact tie keeps the earlier
    -- candidate — and the supplied alias is seeded first.
    better acc@(bestD, _) c
        | cheb c < bestD = (cheb c, c)
        | otherwise      = acc

-- | Squared distance from a fractional query point to a tile, minimised
--   over the tile's u-aliases — the seam-aware form of the plain
--   Euclidean compare the @nearest*Designation@ scans use. Equal to the
--   raw distance away from the seam and for non-wrapping worlds.
seamTileDist2 ∷ Int → (Float, Float) → (Int, Int) → Float
seamTileDist2 worldSize (qx, qy) (gx, gy) =
    let (lx, ly) = localizeTileToAnchor worldSize
                       (round qx, round qy) (gx, gy)
        dx = fromIntegral lx - qx
        dy = fromIntegral ly - qy
    in dx * dx + dy * dy

-- | Does a chunk fall inside an inclusive chunk-coord region, counting
--   its u-aliases? A region built by stepping outward from a coord
--   (a worker's scan radius) is expressed in that coord's raw frame, so
--   at the seam it names aliases of the canonical keys a designation map
--   holds. Identity away from the seam and for non-wrapping worlds.
chunkInSeamRegion ∷ Int → (Int, Int) → (Int, Int) → ChunkCoord → Bool
chunkInSeamRegion worldSize (cx1, cy1) (cx2, cy2) (ChunkCoord cx cy) =
    or [ inside (cx + k * halfW) (cy - k * halfW) | k ← ks ]
  where
    halfW = worldSize `div` 2
    ks | halfW ≤ 0 = [0]
       | otherwise = [-1, 0, 1]
    inside x y = x ≥ min cx1 cx2 ∧ x ≤ max cx1 cx2
               ∧ y ≥ min cy1 cy2 ∧ y ≤ max cy1 cy2

floorMod ∷ Int → Int → Int
floorMod a b = a - div a b * b
