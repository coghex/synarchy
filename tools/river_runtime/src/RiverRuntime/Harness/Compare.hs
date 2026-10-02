-- | Reference comparisons between two runs of the same fixture (#2719,
--   requirement 6): original against translated, two candidates, or a
--   halved step against a full one. These are the measurements of
--   Appendix B of @docs/designs/river_runtime_design.md@. The harness
--   REPORTS them; it sets no pass/fail threshold.
--
--   == Definitions (pinned by the @Sim.Fluid.Harness@ group)
--
--   * __Frame.__ Both runs are compared in the fixture's own tile frame.
--     Each run's cells were normalized back from whatever stored keys
--     its placement produced ("RiverRuntime.Harness.Placement"), so a
--     translation across the cylindrical seam compares tile for tile.
--     Distances are therefore plain local-frame distances; nothing here
--     wraps.
--   * __Matched times.__ A run samples logical time @k * interval@ for
--     each step @k@. Two runs are compared only at the times BOTH
--     sampled, so a halved-step run is read at every second step.
--   * __Equivalent surface.__ A wet cell's surface is its terrain
--     elevation plus its quantity; a dry cell's is its terrain
--     elevation (zero depth). The error at a cell is the absolute
--     difference between the two runs, in eighths of a z-level.
--   * __Domain.__ The union of both runs' wet extents. When neither run
--     has a wet cell the surface error is 'SurfaceBothDry', not zero.
--   * __Percentile.__ Nearest rank: of @n@ errors sorted ascending, the
--     95th percentile is the one at 1-based rank @ceil(0.95 n)@; the
--     maximum is the last.
--   * __Wet/dry disagreement.__ Cells wet in exactly one run, reported
--     separately from the surface error.
--   * __Wet boundary.__ A run's wet cells that have at least one face
--     neighbour, inside the fixture, that is dry. A fixture tile's edge
--     is not a shoreline.
--   * __Boundary distance.__ The symmetric Hausdorff distance between
--     the two boundaries under the Chebyshev metric (a diagonal
--     neighbour is one tile away). Both empty is 'BoundaryBothEmpty';
--     one empty is 'BoundaryOnlyIn' that run, with no number.
--   * __Milestones.__ A milestone is first reached at the earliest
--     sample satisfying it, scanning each run's OWN samples (a finer run
--     can find it sooner). A milestone already satisfied at step 0 is
--     reached at time 0 and flagged initial. One never reached is
--     unreached; a pair where only one run reached it, or neither did,
--     is reported as such rather than as a difference.
module RiverRuntime.Harness.Compare
    ( Series(..)
    , RunSide(..)
    , SurfaceError(..)
    , BoundaryDistance(..)
    , TimeComparison(..)
    , MilestoneKind(..)
    , Milestone(..)
    , MilestoneHit(..)
    , MilestoneDelta(..)
    , MilestoneComparison(..)
    , Comparison(..)
    , compareSeries
    , nearestRankP95
    , equivalentSurface
    , wetBoundary
    , boundaryDistance
    , milestoneHit
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import RiverRuntime.Harness.Adapter (CellState(..), cellQuantity)
import RiverRuntime.Harness.Fixture

-- | One run as a comparison sees it.
data Series = Series
    { seInterval ∷ LogicalTime
    , seTiles    ∷ S.Set Tile
    , seSamples  ∷ [(Int, M.Map Tile CellState)]  -- ^ (step, cells), ascending
    } deriving (Show, Eq)

data RunSide = RunA | RunB
    deriving (Show, Eq)

data SurfaceError
    = SurfaceBothDry
    | SurfaceError
        { seCells ∷ Int  -- ^ size of the union of wet extents
        , seP95   ∷ Int  -- ^ eighths of a z-level
        , seMax   ∷ Int
        }
    deriving (Show, Eq)

data BoundaryDistance
    = BoundaryBothEmpty
    | BoundaryOnlyIn RunSide
    | BoundaryTiles Int
    deriving (Show, Eq)

data TimeComparison = TimeComparison
    { tcTime         ∷ LogicalTime
    , tcStepA        ∷ Int
    , tcStepB        ∷ Int
    , tcSurface      ∷ SurfaceError
    , tcWetDryCells  ∷ Int
    , tcBoundary     ∷ BoundaryDistance
    } deriving (Show, Eq)

data MilestoneKind
    = ArrivalAtLeast Quantity  -- ^ the tile holds at least this much
    | DrainageAtMost Quantity  -- ^ the tile holds at most this much
    deriving (Show, Eq)

data Milestone = Milestone
    { msName ∷ Text
    , msTile ∷ Tile
    , msKind ∷ MilestoneKind
    } deriving (Show, Eq)

data MilestoneHit = MilestoneHit
    { mhStep    ∷ Int
    , mhTime    ∷ LogicalTime
    , mhInitial ∷ Bool  -- ^ already satisfied at step 0
    } deriving (Show, Eq)

data MilestoneDelta
    = MilestoneNeitherReached
    | MilestoneOnlyReachedBy RunSide
    | MilestoneDelta
        { mdTime  ∷ Int        -- ^ B's time minus A's, in logical microseconds
        , mdSteps ∷ Maybe Int  -- ^ B's step minus A's, when the intervals match
        }
    deriving (Show, Eq)

data MilestoneComparison = MilestoneComparison
    { mcMilestone ∷ Milestone
    , mcA         ∷ Maybe MilestoneHit
    , mcB         ∷ Maybe MilestoneHit
    , mcDelta     ∷ MilestoneDelta
    } deriving (Show, Eq)

data Comparison = Comparison
    { cmpTimes      ∷ [TimeComparison]
    , cmpMilestones ∷ [MilestoneComparison]
    } deriving (Show, Eq)

-- | Compare two runs of the same fixture. Refuses runs over different
--   tile sets, which are not the same fixture.
compareSeries ∷ [Milestone] → Series → Series → Either Text Comparison
compareSeries milestones a b
    | seTiles a ≢ seTiles b = Left "the two runs cover different fixture tiles"
    | otherwise = Right Comparison
        { cmpTimes = [ compareAt t sa sb | (t, (sa, sb)) ← M.toList matched ]
        , cmpMilestones = map compareMilestone milestones
        }
  where
    timed s = M.fromList [ (k * unLogicalTime (seInterval s), (k, cells))
                         | (k, cells) ← seSamples s ]
    matched = M.intersectionWith (,) (timed a) (timed b)
    tiles = seTiles a
    compareAt t (ka, ca) (kb, cb) = TimeComparison
        { tcTime        = LogicalTime t
        , tcStepA       = ka
        , tcStepB       = kb
        , tcSurface     = surfaceError ca cb
        , tcWetDryCells = S.size (wet ca `symDiff` wet cb)
        , tcBoundary    = boundaryDistance (wetBoundary tiles ca) (wetBoundary tiles cb)
        }
    wet cells = S.fromList [ t | (t, c) ← M.toList cells, cellQuantity c > 0 ]
    symDiff x y = (x `S.difference` y) `S.union` (y `S.difference` x)
    surfaceError ca cb =
        let domain = S.toAscList (wet ca `S.union` wet cb)
            errs = [ abs (surf ca t - surf cb t) | t ← domain ]
        in if null errs then SurfaceBothDry
           else SurfaceError (length errs) (nearestRankP95 errs) (maximum errs)
    surf cells t = maybe 0 (unSurface . equivalentSurface) (M.lookup t cells)
    compareMilestone m =
        let ha = milestoneHit a m
            hb = milestoneHit b m
            sameInterval = seInterval a ≡ seInterval b
            delta = case (ha, hb) of
                (Nothing, Nothing) → MilestoneNeitherReached
                (Just _, Nothing)  → MilestoneOnlyReachedBy RunA
                (Nothing, Just _)  → MilestoneOnlyReachedBy RunB
                (Just x, Just y)   → MilestoneDelta
                    { mdTime  = unLogicalTime (mhTime y) - unLogicalTime (mhTime x)
                    , mdSteps = if sameInterval then Just (mhStep y - mhStep x) else Nothing }
        in MilestoneComparison m ha hb delta

-- | Nearest-rank 95th percentile of a non-empty list.
nearestRankP95 ∷ [Int] → Int
nearestRankP95 xs =
    let sorted = L.sort xs
        n = length sorted
        rank = (95 * n + 99) `div` 100  -- ceil (0.95 n), at least 1
    in sorted !! (max 1 rank - 1)

-- | A cell's equivalent surface: terrain plus quantity, terrain alone
--   when dry.
equivalentSurface ∷ CellState → Surface
equivalentSurface c = surfaceOf (cellTerrain c) (Quantity (cellQuantity c))

-- | Wet cells with a dry face neighbour inside the fixture.
wetBoundary ∷ S.Set Tile → M.Map Tile CellState → S.Set Tile
wetBoundary tiles cells = S.fromList
    [ t | (t, c) ← M.toList cells, cellQuantity c > 0
        , any dryNeighbour (cardinalTiles t) ]
  where
    dryNeighbour n = S.member n tiles
                   ∧ maybe True (\c → cellQuantity c ≡ 0) (M.lookup n cells)

boundaryDistance ∷ S.Set Tile → S.Set Tile → BoundaryDistance
boundaryDistance a b
    | S.null a ∧ S.null b = BoundaryBothEmpty
    | S.null b = BoundaryOnlyIn RunA
    | S.null a = BoundaryOnlyIn RunB
    | otherwise = BoundaryTiles (max (directed a b) (directed b a))
  where
    directed xs ys = maximum [ minimum [ chebyshev x y | y ← S.toList ys ]
                             | x ← S.toList xs ]
    chebyshev (Tile ax ay) (Tile bx by) = max (abs (ax - bx)) (abs (ay - by))

-- | The first sample of a run satisfying a milestone.
milestoneHit ∷ Series → Milestone → Maybe MilestoneHit
milestoneHit s m = listToMaybe
    [ MilestoneHit k (LogicalTime (k * unLogicalTime (seInterval s))) (k ≡ 0)
    | (k, cells) ← seSamples s
    , satisfied (maybe 0 cellQuantity (M.lookup (msTile m) cells)) ]
  where
    satisfied q = case msKind m of
        ArrivalAtLeast (Quantity x) → q ≥ x
        DrainageAtMost (Quantity x) → q ≤ x
