{-# LANGUAGE Strict #-}

-- | The seam topology of one world page, as the fluid simulation sees it.
--
--   Chunks are STORED u-wrapped, so on a cylindrical page the chunk
--   physically east of @(cx, cy)@ is not always the raw @(cx+1, cy)@
--   key: at the seam the canonical neighbour has BOTH components changed
--   ('World.Chunk.Types.wrapChunkCoordU'). Nothing under @src\/Sim@ knew
--   any world's topology, so both of the simulation's neighbour probes —
--   the cardinal activation behind 'Sim.Command.Types.SimChunkEdited'
--   and 'Sim.Fluid.Active.reconcileSeams' — looked up RAW keys, missed
--   the far-side chunk, and walled edited fluid in against an artificial
--   boundary (#2044).
--
--   Topology is page-scoped and travels with every command that can seed
--   or activate one world's sim state, so a page cannot reach a normal
--   tick or a synchronous fast settle without it.
--
--   The two non-wrapping cases are selected SEPARATELY, exactly as
--   'World.Chunk.Queue.chunkQueueCanon' selects them (#2001). A world
--   size of zero or less has no seam at all. An arena's 'wgpWorldSize'
--   is a sentinel @100000@ rather than a real extent
--   ('World.Thread.Command.Init.handleWorldInitArenaCommand'), so it is
--   recognised by 'isArenaParams' and never handed to
--   'wrapChunkCoordU' — passing that sentinel through would silently
--   wrap an arena coord past @u = ±50000@.
module Sim.Topology
    ( SimTopology(..)
    , simTopologyForParams
    , simCanonChunk
    , simSeamNeighbor
    , simCardinalNeighbors
    ) where

import UPrelude
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)
import World.Generate.Types (WorldGenParams(..), isArenaParams)

-- | How one page's chunk keys behave along the u axis.
--
--   'SimCylindricalU' carries an already-validated WRAPPING world size:
--   'simTopologyForParams' is the only thing that builds it, and it
--   rejects both non-wrapping cases first. Nothing downstream re-checks
--   for the arena sentinel, so nothing downstream may build this
--   constructor from a raw 'wgpWorldSize'.
data SimTopology
    = SimFlatTopology
      -- ^ No seam: an arena, or a page with no (or a non-positive) world
      --   size. Every coordinate operation below is the identity.
    | SimCylindricalU !Int
      -- ^ The page wraps on u with this world size (in chunks).
    deriving (Show, Eq)

-- | The simulation's view of a page's gen params. Observationally
--   identical to 'World.Chunk.Queue.chunkQueueCanon' for every coord,
--   which is what keeps the sim's neighbour keys and the page's chunk
--   STORAGE keys the same spelling.
simTopologyForParams ∷ WorldGenParams → SimTopology
simTopologyForParams params
    | isArenaParams params = SimFlatTopology
    | worldSize ≤ 0        = SimFlatTopology
    | otherwise            = SimCylindricalU worldSize
  where worldSize = wgpWorldSize params

-- | The key a chunk coord is stored under on a page with this topology.
simCanonChunk ∷ SimTopology → ChunkCoord → ChunkCoord
simCanonChunk SimFlatTopology     = id
simCanonChunk (SimCylindricalU w) = wrapChunkCoordU w

-- | The STORED key of the chunk physically offset by @(dx, dy)@ chunks
--   from @coord@ — the identity offset away from the seam, and the
--   far-side canonical key at it.
simSeamNeighbor ∷ SimTopology → Int → Int → ChunkCoord → ChunkCoord
simSeamNeighbor topo dx dy (ChunkCoord cx cy) =
    simCanonChunk topo (ChunkCoord (cx + dx) (cy + dy))

-- | The stored keys of the four chunks physically cardinal-adjacent to
--   @coord@, in +X, -X, +Y, -Y order.
simCardinalNeighbors ∷ SimTopology → ChunkCoord → [ChunkCoord]
simCardinalNeighbors topo coord =
    [ simSeamNeighbor topo dx dy coord
    | (dx, dy) ← [(1, 0), (-1, 0), (0, 1), (0, -1)] ]
