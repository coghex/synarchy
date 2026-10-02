-- | Placing a fixture onto a page, and normalizing what a solver reports
--   back into the fixture's own frame (#2719).
--
--   A placement translates every local chunk by a whole-chunk offset
--   and then canonicalizes it through the page's own seam topology
--   ('Sim.Topology.simCanonChunk'), exactly the key the simulation
--   stores that chunk under. On a cylindrical page a chunk translated
--   past the u seam therefore changes BOTH coordinates; the cell index
--   inside the chunk never changes.
--
--   Solver adapters speak in stored cells. Every report they make is
--   normalized back to fixture tiles through the inverse of this one
--   map, so two runs of the same fixture at different placements —
--   ordinary or across the wrapped seam — are compared tile for tile in
--   the same frame, and a stored cell the placement never produced is
--   detected rather than silently dropped.
module RiverRuntime.Harness.Placement
    ( Placement(..)
    , StoredCell(..)
    , PlacementMap(..)
    , storedChunk
    , placeTile
    , placementMap
    , normalizeCell
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import World.Chunk.Types (ChunkCoord(..))
import Sim.Topology (SimTopology(..), simCanonChunk, simSeamNeighbor)
import RiverRuntime.Harness.Fixture

data Placement = Placement
    { plName     ∷ Text
    , plTopology ∷ SimTopology
    , plOffset   ∷ (Int, Int)  -- ^ whole-chunk translation, before wrapping
    } deriving (Show, Eq)

-- | A cell as a solver stores it: the canonical chunk key and the
--   row-major index inside that chunk.
data StoredCell = StoredCell
    { scChunk ∷ ChunkCoord
    , scIndex ∷ Int
    } deriving (Show, Eq, Ord)

data PlacementMap = PlacementMap
    { pmPlacement ∷ Placement
    , pmForward   ∷ M.Map Tile StoredCell
    , pmInverse   ∷ M.Map StoredCell Tile
    , pmChunks    ∷ M.Map LocalChunk ChunkCoord
    } deriving (Show, Eq)

-- | The stored key of a local chunk under a placement.
storedChunk ∷ Placement → LocalChunk → ChunkCoord
storedChunk pl (LocalChunk cx cy) =
    let (ox, oy) = plOffset pl
    in simCanonChunk (plTopology pl) (ChunkCoord (cx + ox) (cy + oy))

placeTile ∷ Placement → Tile → StoredCell
placeTile pl tile = StoredCell (storedChunk pl (tileChunk tile)) (tileIndex tile)

-- | The complete placement of a fixture, refusing one that would merge
--   two local chunks into one stored key (a fixture wider than the
--   page) or break the physical adjacency of two neighbouring local
--   chunks — the seam probe must reach the stored neighbour the local
--   frame says is there.
placementMap ∷ Placement → Fixture → Either Text PlacementMap
placementMap pl fx
    | M.size inverseChunks ≢ M.size chunks =
        Left ("placement " <> plName pl <> " maps two fixture chunks onto one stored chunk")
    | not (null broken) =
        Left ("placement " <> plName pl <> " breaks chunk adjacency: " <> tshow (take 1 broken))
    | otherwise = Right PlacementMap
        { pmPlacement = pl
        , pmForward   = forward
        , pmInverse   = M.fromList [ (sc, t) | (t, sc) ← M.toList forward ]
        , pmChunks    = chunks
        }
  where
    chunks = M.fromList [ (lc, storedChunk pl lc) | (lc, _) ← fxChunks fx ]
    inverseChunks = M.fromList [ (sc, lc) | (lc, sc) ← M.toList chunks ]
    forward = M.fromList [ (t, placeTile pl t) | t ← fixtureTiles fx ]
    broken =
        [ (a, b)
        | (a@(LocalChunk ax ay), sa) ← M.toList chunks
        , (dx, dy) ← [(1, 0), (0, 1)]
        , let b = LocalChunk (ax + dx) (ay + dy)
        , Just sb ← [M.lookup b chunks]
        , simSeamNeighbor (plTopology pl) dx dy sa ≢ sb ]

-- | The fixture tile a stored cell stands for, if the placement
--   produced it at all.
normalizeCell ∷ PlacementMap → StoredCell → Maybe Tile
normalizeCell pm sc = M.lookup sc (pmInverse pm)
