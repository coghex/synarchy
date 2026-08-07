{-# LANGUAGE Strict #-}
-- | Canonicalising chunk resolution for the render path (#1135).
--
--   Loaded chunks are STORED under u-wrapped (canonical) coords:
--   'World.Thread.ChunkLoading' wraps every requested coord with
--   'wrapChunkCoordU' before inserting, and states that insert-time and
--   lookup-time wrapping must not diverge. A render-side lookup that
--   hands 'HM.lookup' whatever coord it happened to compute therefore
--   misses right at the cylindrical U seam — the chunk IS loaded, the
--   key is simply an alias of the one it is stored under.
--
--   This module is that single lookup-time boundary. Away from the seam
--   every function here is the identity on its input (and in arena /
--   zero-size worlds 'wrapChunkCoordU' never wraps at all), so routing a
--   lookup through it can only turn a seam miss into a hit.
--
--   Two shapes, because render sites need two different things:
--
--   * 'canonicalChunkLookup' — for a lookup whose result is read at a
--     LOCAL index. The wrap shifts the chunk coord by whole worlds along
--     u, so the local @(lx, ly)@ index into the resolved chunk's maps is
--     unchanged and the key is all that needs fixing.
--
--   * 'canonicalTileFrame' — for a lookup whose caller also keeps
--     GLOBAL tile coords. Those coords are in the raw frame and must be
--     shifted by the same whole-chunk delta, or they name a tile the
--     stored chunk does not hold (and that downstream world commands,
--     which only speak the canonical frame, would reject).
module World.Render.ChunkLookup
    ( canonicalChunkLookup
    , canonicalTileFrame
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..), chunkSize, wrapChunkCoordU)
import World.Generate.Coordinates (globalToChunk)

-- | Look a chunk coord up in a chunk-keyed map, canonicalising the key
--   first so it matches the frame chunks are stored under.
--
--   Use this wherever a render site steps outward from a chunk it
--   already holds (a cardinal neighbour, a probed adjacent chunk): the
--   step is computed in the home chunk's raw frame and lands outside the
--   canonical u range whenever the home chunk sits against the seam.
canonicalChunkLookup ∷ Int                        -- ^ world size in chunks
                     → HM.HashMap ChunkCoord v    -- ^ loaded-chunk-keyed map
                     → ChunkCoord                 -- ^ possibly-aliased coord
                     → Maybe v
canonicalChunkLookup worldSize m cc =
    HM.lookup (wrapChunkCoordU worldSize cc) m

-- | Resolve a global tile coord to the canonical chunk that STORES it,
--   its local index within that chunk, and the whole-chunk @(dgx, dgy)@
--   shift carrying any coord from the raw frame into the stored one.
--
--   The shift is the u-wrap expressed in tiles: it moves whole chunks, so
--   the local index is the same on both sides, and it preserves
--   @v = gx + gy@ exactly as 'wrapChunkCoordU' preserves @v = cx + cy@.
--   Both are @(0, 0)@ / the identity away from the seam.
canonicalTileFrame ∷ Int                -- ^ world size in chunks
                   → Int → Int          -- ^ global tile (gx, gy), raw frame
                   → (ChunkCoord, (Int, Int), (Int, Int))
                      -- ^ (stored chunk, local (lx, ly), tile shift (dgx, dgy))
canonicalTileFrame worldSize gx gy =
    let (ccRaw@(ChunkCoord rcx rcy), local) = globalToChunk gx gy
        cc@(ChunkCoord ccx ccy) = wrapChunkCoordU worldSize ccRaw
    in (cc, local, ((ccx - rcx) * chunkSize, (ccy - rcy) * chunkSize))
