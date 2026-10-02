-- | Placing a fixture onto a page, and normalizing what a solver reports
--   back into the fixture's own frame (#2719).
--
--   A placement translates every fixture tile by a TILE offset, then
--   stores it the way the simulation does: the physical chunk containing
--   the translated tile, canonicalized through the page's own seam
--   topology ('Sim.Topology.simCanonChunk'), and the row-major index of
--   the translated tile inside it. A tile offset that is not a whole
--   number of chunks changes the partition: the same physical flow face
--   can sit inside a chunk at one placement, on an ordinary chunk seam at
--   another, and on the wrapped u seam of a cylindrical page at a third.
--   That partition dependence is exactly what translation comparisons
--   measure.
--
--   The stored chunks a translated fixture touches can hold cells the
--   fixture does not: PADDING. Every padding cell is seeded as a dry
--   cell at the fixture's base terrain — a wall — and the harness checks
--   that it stays exactly that ("RiverRuntime.Harness.Run").
--
--   A stored chunk takes the residency of the fixture chunks it overlaps.
--   A placement that would put an active and an inactive fixture chunk
--   into one stored chunk is refused, as is one that maps two tiles onto
--   one stored cell or breaks the physical adjacency of two neighbouring
--   tiles.
--
--   Solver adapters speak in stored cells. Every report is normalized
--   back to fixture tiles through the inverse of this one map, so runs at
--   different placements compare tile for tile in the same frame.
module RiverRuntime.Harness.Placement
    ( Placement(..)
    , StoredCell(..)
    , PlacementMap(..)
    , placeTile
    , placementMap
    , normalizeCell
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Map.Strict as M
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import Sim.Topology (SimTopology, simCanonChunk, simSeamNeighbor)
import RiverRuntime.Harness.Fixture

data Placement = Placement
    { plName     ∷ Text
    , plTopology ∷ SimTopology
    , plOffset   ∷ (Int, Int)  -- ^ translation in TILES, before wrapping
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
    , pmChunks    ∷ M.Map ChunkCoord Residency
      -- ^ every stored chunk the placement touches, with its residency
    , pmPadding   ∷ M.Map StoredCell Elevation
      -- ^ stored cells outside the fixture, seeded as dry walls
    } deriving (Show, Eq)

-- | The physical tile a fixture tile lands on, before wrapping.
physical ∷ Placement → Tile → Tile
physical pl (Tile x y) = let (ox, oy) = plOffset pl in Tile (x + ox) (y + oy)

physicalChunk ∷ Tile → ChunkCoord
physicalChunk (Tile x y) = ChunkCoord (x `div` chunkSize) (y `div` chunkSize)

placeTile ∷ Placement → Tile → StoredCell
placeTile pl tile =
    let p = physical pl tile
    in StoredCell (simCanonChunk (plTopology pl) (physicalChunk p)) (tileIndex p)

-- | The complete placement of a fixture, or why it cannot be placed.
placementMap ∷ Placement → Fixture → Either Text PlacementMap
placementMap pl fx
    | M.size inverse ≢ M.size forward =
        Left ("placement " <> plName pl <> " maps two fixture tiles onto one stored cell")
    | not (null mixed) =
        Left ("placement " <> plName pl <> " puts active and inactive fixture chunks into "
              <> "one stored chunk: " <> tshow (take 1 mixed))
    | not (null broken) =
        Left ("placement " <> plName pl <> " breaks tile adjacency: " <> tshow (take 1 broken))
    | otherwise = Right PlacementMap
        { pmPlacement = pl
        , pmForward   = forward
        , pmInverse   = inverse
        , pmChunks    = M.map head' residencies
        , pmPadding   = M.fromList
            [ (sc, fxBaseTerrain fx)
            | cc ← M.keys residencies, i ← [0 .. chunkSize * chunkSize - 1]
            , let sc = StoredCell cc i, not (M.member sc inverse) ]
        }
  where
    residency = M.fromList (fxChunks fx)
    tiles = fixtureTiles fx
    forward = M.fromList [ (t, placeTile pl t) | t ← tiles ]
    inverse = M.fromList [ (sc, t) | (t, sc) ← M.toList forward ]
    residencies = M.map L.nub $ M.fromListWith (flip (<>))
        [ (scChunk sc, [residency M.! tileChunk t]) | (t, sc) ← M.toList forward ]
    mixed = [ cc | (cc, rs) ← M.toList residencies, length rs > 1 ]
    head' rs = case rs of
        (r : _) → r
        []      → ResidentActive  -- unreachable: every key has a tile
    -- Two face-adjacent fixture tiles in different physical chunks must
    -- be stored in chunks the simulation's own seam probe connects.
    broken =
        [ (a, b)
        | a ← tiles, b ← [ Tile (tileX a + 1) (tileY a), Tile (tileX a) (tileY a + 1) ]
        , M.member b forward
        , let ChunkCoord ax ay = physicalChunk (physical pl a)
              ChunkCoord bx by = physicalChunk (physical pl b)
        , (ax, ay) ≢ (bx, by)
        , simSeamNeighbor (plTopology pl) (bx - ax) (by - ay) (scChunk (forward M.! a))
            ≢ scChunk (forward M.! b) ]

-- | The fixture tile a stored cell stands for, if the placement put a
--   fixture tile there at all.
normalizeCell ∷ PlacementMap → StoredCell → Maybe Tile
normalizeCell pm sc = M.lookup sc (pmInverse pm)
