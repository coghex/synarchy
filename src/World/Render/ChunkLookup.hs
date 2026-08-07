{-# LANGUAGE Strict #-}
-- | Canonicalising chunk lookup for the render path (#1135).
--
--   Loaded chunks are STORED under u-wrapped (canonical) coords:
--   'World.Thread.ChunkLoading' wraps every requested coord with
--   'wrapChunkCoordU' before inserting, and states that insert-time and
--   lookup-time wrapping must not diverge. A render-side lookup that
--   hands 'HM.lookup' whatever coord it happened to compute therefore
--   misses right at the cylindrical U seam — the chunk IS loaded, the
--   key is simply an alias of the one it is stored under.
--
--   This module is that lookup-time boundary for callers that step
--   outward from a chunk they already hold. Callers that also carry
--   GLOBAL TILE coords need 'World.Generate.Coordinates.canonicalTileFrame'
--   instead (or as well): those coords are in the raw frame and must be
--   shifted by the same whole-chunk delta, or they name a tile the
--   stored chunk does not hold.
module World.Render.ChunkLookup
    ( canonicalChunkLookup
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)

-- | Look a chunk coord up in a chunk-keyed map, canonicalising the key
--   first so it matches the frame chunks are stored under.
--
--   Use this wherever a render site steps outward from a chunk it
--   already holds (a cardinal neighbour, a probed adjacent chunk): the
--   step is computed in the home chunk's raw frame and lands outside the
--   canonical u range whenever the home chunk sits against the seam. The
--   wrap moves whole chunks, so a local @(lx, ly)@ index into the
--   resolved chunk is unchanged and the key is all that needs fixing.
--   Identity away from the seam.
canonicalChunkLookup ∷ Int                        -- ^ world size in chunks
                     → HM.HashMap ChunkCoord v    -- ^ loaded-chunk-keyed map
                     → ChunkCoord                 -- ^ possibly-aliased coord
                     → Maybe v
canonicalChunkLookup worldSize m cc =
    HM.lookup (wrapChunkCoordU worldSize cc) m
