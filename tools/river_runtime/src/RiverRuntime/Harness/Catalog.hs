-- | The authored hydraulic fixtures, their placements and milestones,
--   and the eight archived legacy characterization cases (#2719,
--   requirements 2 and 7).
--
--   Every fixture is surrounded by the base terrain, a whole z-level 0
--   that stands above every authored surface, so the fixture's water is
--   held by geometry rather than by the edge of the loaded area. Terrain
--   is authored in whole z-levels here only because the legacy adapter
--   can represent nothing else; the schema itself is exact.
module RiverRuntime.Harness.Catalog
    ( -- * Fixtures
      HarnessExperiment(..)
    , experiments
    , findExperiment
    , channelReservoir
    , damDiversion
    , raisedSill
    , lakeAtRest
    , dryBank
    , withWalls
      -- * Placements
    , originPlacement
    , ordinaryPlacement
    , shiftedPlacement
    , wrappedPlacement
    , wrappedShiftedPlacement
    , wrappedWorldSize
    , standardPlacements
      -- * Archived legacy characterization
    , CharacterizationCase(..)
    , characterizationCases
    , characterizationFixture
    , characterizationSamples
      -- * Units
    , zLevel
    , referenceInterval
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import World.Fluid.Types (FluidType(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import Sim.Topology (SimTopology(..), simTopologyForParams)
import RiverRuntime.Harness.Adapter (cellQuantity)
import RiverRuntime.Harness.Compare (Milestone(..), MilestoneKind(..))
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Placement (Placement(..))
import RiverRuntime.Harness.Run (Trajectory(..), StepRecord(..))

-- | A whole z-level as an exact elevation.
zLevel ∷ Int → Elevation
zLevel z = Elevation (z * 8)

-- | The reference step interval the catalog is authored at: 100 ms of
--   logical time, one production tick at the default rate.
referenceInterval ∷ LogicalTime
referenceInterval = LogicalTime 100000

seconds ∷ Int → LogicalTime
seconds s = LogicalTime (s * 1000000)

-- | A fixture with the comparisons the archive runner makes for it.
data HarnessExperiment = HarnessExperiment
    { heFixture    ∷ Fixture
    , heMilestones ∷ [Milestone]
    } deriving (Show, Eq)

experiments ∷ [HarnessExperiment]
experiments =
    [ HarnessExperiment channelReservoir
        [ arrival "seam-crossing" (Tile 16 8)
        , arrival "channel-front" (Tile 24 8)
        , arrival "end-basin" (Tile 43 8)
        , Milestone "reservoir-drawdown" (Tile 5 8) (DrainageAtMost (Quantity 16)) ]
    , HarnessExperiment damDiversion
        [ arrival "diversion-channel" (Tile 6 12)
        , arrival "diversion-basin" (Tile 6 21)
        , arrival "dam-cell" (Tile 12 8)
        , arrival "downstream" (Tile 13 8) ]
    , HarnessExperiment raisedSill
        [ arrival "beyond-sill" (Tile 17 8) ]
    , HarnessExperiment lakeAtRest
        [ arrival "gate-cell" (Tile 28 8)
        , arrival "basin" (Tile 30 8)
        , Milestone "lake-drawdown" (Tile 27 8) (DrainageAtMost (Quantity 15)) ]
    , HarnessExperiment dryBank
        [ arrival "bank-front" (Tile 10 8)
        , arrival "bank-far" (Tile 12 8) ]
    ]
  where
    arrival name tile = Milestone name tile (ArrivalAtLeast (Quantity 1))

findExperiment ∷ Text → Maybe HarnessExperiment
findExperiment name = listToMaybe [ e | e ← experiments, fxName (heFixture e) ≡ name ]

-- | Cells of one kind over an inclusive rectangle.
rect ∷ (Int, Int) → (Int, Int) → CellSpec → [(Tile, CellSpec)]
rect (x0, y0) (x1, y1) spec = [ (Tile x y, spec) | y ← [y0 .. y1], x ← [x0 .. x1] ]

dry ∷ Int → CellSpec
dry z = CellSpec (zLevel z) Nothing

wet ∷ FluidType → Int → Int → CellSpec
wet ty z q = CellSpec (zLevel z) (Just (FluidSpec ty (Quantity q)))

-- | Later rectangles override earlier ones.
cells ∷ [[(Tile, CellSpec)]] → M.Map Tile CellSpec
cells = M.fromList . concat

active ∷ [(Int, Int)] → [(LocalChunk, Residency)]
active cs = [ (LocalChunk x y, ResidentActive) | (x, y) ← cs ]

-- | Every fixture tile standing at the base terrain, declared as one
--   barrier closed at that crest: the walls that hold the fixture's
--   water. Without them the region check would be vacuous, since the
--   fixture's open cells are all face-connected through its walls.
--   A gate or dam is simply a wall tile that a scheduled edit lowers.
withWalls ∷ Fixture → Fixture
withWalls fx = fx { fxBarriers = Barrier "walls" walls (fxBaseTerrain fx) : fxBarriers fx }
  where
    walls = [ t | t ← fixtureTiles fx, csTerrain (initialCell fx t) ≥ fxBaseTerrain fx ]

-- | A reservoir drains through a three-chunk channel into a basin,
--   crossing two ordinary chunk seams.
channelReservoir ∷ Fixture
channelReservoir = withWalls Fixture
    { fxName        = "channel-reservoir"
    , fxDescription = "A 24-unit reservoir at z-4 drains east through a dry z-5 "
                   <> "channel across two chunk seams into a z-6 basin."
    , fxChunks      = active [(0, 0), (1, 0), (2, 0)]
    , fxBaseTerrain = zLevel 0
    , fxCells       = cells
        [ rect (2, 4) (7, 11) (wet River (-4) 24)
        , rect (8, 8) (41, 8) (dry (-5))
        , rect (42, 6) (45, 10) (dry (-6)) ]
    , fxBarriers    = []
    , fxSchedule    = []
    , fxDuration    = seconds 40
    }

-- | A dammed channel with a closed side diversion: the diversion opens,
--   then the dam opens, then the dam closes again over whatever stands
--   on it, which is displaced and accounted as a sink.
damDiversion ∷ Fixture
damDiversion = withWalls Fixture
    { fxName        = "dam-diversion"
    , fxDescription = "A two-level channel behind a dam with a closed side diversion; "
                   <> "the diversion opens at 5 s, the dam opens at 10 s and closes "
                   <> "again at 20 s, displacing its cell's water to a declared sink."
    , fxChunks      = active [(0, 0), (1, 0), (2, 0), (0, 1)]
    , fxBaseTerrain = zLevel 0
    , fxCells       = cells
        [ rect (1, 8) (11, 8) (wet River (-4) 16)
        , rect (13, 8) (40, 8) (dry (-4))
        , rect (6, 10) (6, 19) (dry (-5))
        , rect (3, 20) (9, 23) (dry (-6)) ]
    , fxBarriers    =
        [ Barrier "dam" [Tile 12 8] (zLevel 0)
        , Barrier "diversion-gate" [Tile 6 9] (zLevel 0) ]
    , fxSchedule    =
        [ Scheduled (seconds 5) (SetTerrain (Tile 6 9) (zLevel (-5)) KeepQuantity)
        , Scheduled (seconds 10) (SetTerrain (Tile 12 8) (zLevel (-4)) KeepQuantity)
        , Scheduled (seconds 20) (SetTerrain (Tile 12 8) (zLevel 0) DisplaceToSink) ]
    , fxDuration    = seconds 30
    }

-- | Water standing above a sill one z higher than its bed, the sill on
--   a chunk seam, with lower dry ground beyond it.
raisedSill ∷ Fixture
raisedSill = withWalls Fixture
    { fxName        = "raised-sill"
    , fxDescription = "A 24-unit pool at z-4 (surface one z above its bed's sill) "
                   <> "against a z-3 sill on the chunk seam, with dry z-5 ground beyond."
    , fxChunks      = active [(0, 0), (1, 0)]
    , fxBaseTerrain = zLevel 0
    , fxCells       = cells
        [ rect (2, 6) (15, 10) (wet River (-4) 24)
        , rect (16, 6) (16, 10) (dry (-3))
        , rect (17, 6) (28, 10) (dry (-5)) ]
    , fxBarriers    = []
    , fxSchedule    = []
    , fxDuration    = seconds 10
    }

-- | A closed lake whose bed steps up twice under one flat surface. With
--   nothing to do it reaches equilibrium deactivation; a gate opens
--   afterwards and drains it into a basin.
lakeAtRest ∷ Fixture
lakeAtRest = withWalls Fixture
    { fxName        = "lake-at-rest"
    , fxDescription = "A flat-surfaced lake (surface z-2) over a bed stepping z-6/-5/-4 "
                   <> "across a chunk seam; a gate to a dry z-7 basin opens at 23 s, "
                   <> "after the 20 s of rest that deactivates every legacy chunk."
    , fxChunks      = active [(0, 0), (1, 0), (2, 0)]
    , fxBaseTerrain = zLevel 0
    , fxCells       = cells
        [ rect (2, 4) (9, 11) (wet Lake (-6) 32)
        , rect (10, 4) (19, 11) (wet Lake (-5) 24)
        , rect (20, 4) (27, 11) (wet Lake (-4) 16)
        , rect (29, 6) (40, 10) (dry (-7)) ]
    , fxBarriers    = [ Barrier "gate" [Tile 28 8] (zLevel 0) ]
    , fxSchedule    =
        [ Scheduled (seconds 23) (SetTerrain (Tile 28 8) (zLevel (-4)) KeepQuantity) ]
    , fxDuration    = seconds 30
    }

-- | One and a half levels of water beside dry cells on the same bed,
--   inside a chunk and across a seam, under a raised dry bank.
dryBank ∷ Fixture
dryBank = withWalls Fixture
    { fxName        = "dry-bank"
    , fxDescription = "A 12-unit pool at z-4 beside dry z-4 cells that continue "
                   <> "across the chunk seam, under a raised dry z-3 bank."
    , fxChunks      = active [(0, 0), (1, 0)]
    , fxBaseTerrain = zLevel 0
    , fxCells       = cells
        [ rect (3, 5) (24, 10) (dry (-4))
        , rect (3, 5) (9, 10) (wet River (-4) 12)
        , rect (3, 11) (24, 11) (dry (-3)) ]
    , fxBarriers    = []
    , fxSchedule    = []
    , fxDuration    = seconds 10
    }

-- * Placements
--
-- Offsets are in tiles. A whole-chunk offset keeps every flow face where
-- it was relative to the chunk grid; any other offset re-partitions the
-- fixture, so a face that was inside a chunk can land on a seam and the
-- reverse. Comparing the origin run with each translation measures how
-- much a solver's answer depends on where the chunk boundaries fall.

originPlacement ∷ Placement
originPlacement = Placement "origin" SimFlatTopology (0, 0)

-- | The same flat page, translated by whole chunks: the partition is
--   unchanged.
ordinaryPlacement ∷ Placement
ordinaryPlacement = Placement "ordinary" SimFlatTopology (48, -32)

-- | The same flat page, translated by half a chunk in x and an odd
--   number of tiles in y: every fixture face moves relative to the chunk
--   grid, so local x = 7|8 (interior at the origin) lands on the
--   x = 15|16 seam.
shiftedPlacement ∷ Placement
shiftedPlacement = Placement "shifted" SimFlatTopology (8, -5)

cylindrical ∷ SimTopology
cylindrical = simTopologyForParams defaultWorldGenParams { wgpWorldSize = wrappedWorldSize }

-- | A cylindrical page where the u seam falls between local chunks
--   x = 0 and x = 1: local chunk (0,0) lands at u = 31, the last column
--   before the seam, so local chunk (1,0) is stored on the far side with
--   both coordinates changed. The partition is unchanged.
wrappedPlacement ∷ Placement
wrappedPlacement = Placement "wrapped" cylindrical (16 * (wrappedWorldSize `div` 2 - 1), 0)

-- | The wrapped page, shifted like 'shiftedPlacement': local x = 7|8
--   lands on the wrapped u seam itself.
wrappedShiftedPlacement ∷ Placement
wrappedShiftedPlacement =
    Placement "wrapped-shifted" cylindrical (16 * (wrappedWorldSize `div` 2 - 1) + 8, 3)

wrappedWorldSize ∷ Int
wrappedWorldSize = 64

standardPlacements ∷ [Placement]
standardPlacements =
    [ originPlacement, ordinaryPlacement, shiftedPlacement
    , wrappedPlacement, wrappedShiftedPlacement ]

-- * The archived legacy characterization

-- | One case of @tools/river_runtime/Characterize.hs@, whose output is
--   archived as @docs/evidence/river-runtime/baseline-solver.json@.
data CharacterizationCase = CharacterizationCase
    { ccName          ∷ Text
    , ccSource        ∷ Tile
    , ccTarget        ∷ Tile
    , ccSourceBed     ∷ Int  -- ^ whole z
    , ccTargetBed     ∷ Int  -- ^ whole z
    , ccUnits         ∷ Int
    , ccTargetPresent ∷ Bool
    , ccTargetActive  ∷ Bool
    } deriving (Show, Eq)

characterizationCases ∷ [CharacterizationCase]
characterizationCases =
    [ c "raised-sill-interior"    (7, 8)  (8, 8)  (-4) (-3) 24 True True
    , c "raised-sill-seam"        (15, 8) (16, 8) (-4) (-3) 24 True True
    , c "downhill-control"        (7, 8)  (8, 8)  (-4) (-5) 24 True True
    , c "one-level-interior"      (7, 8)  (8, 8)  (-4) (-4) 8  True True
    , c "one-level-seam"          (15, 8) (16, 8) (-4) (-4) 8  True True
    , c "inactive-neighbor"       (15, 8) (16, 8) (-4) (-4) 24 True False
    , c "absent-neighbor"         (15, 8) (16, 8) (-4) (-4) 24 False False
    , c "active-neighbor-control" (15, 8) (16, 8) (-4) (-4) 24 True True
    ]
  where
    c name (sx, sy) (tx, ty) = CharacterizationCase name (Tile sx sy) (Tile tx ty)

-- | The case as a fixture: the source chunk active; the target chunk,
--   when it is a different one, present only if the case says so and
--   active only if the case says so. Ten ticks of the reference interval.
characterizationFixture ∷ CharacterizationCase → Fixture
characterizationFixture cc = withWalls Fixture
    { fxName        = "characterization/" <> ccName cc
    , fxDescription = "Archived legacy characterization case " <> ccName cc
    , fxChunks      = (sourceChunk, ResidentActive)
                    : [ (targetChunk, if ccTargetActive cc then ResidentActive
                                                         else ResidentInactive)
                      | targetChunk ≢ sourceChunk, ccTargetPresent cc ]
    , fxBaseTerrain = zLevel 0
    , fxCells       = M.fromList $
        (ccSource cc, wet River (ccSourceBed cc) (ccUnits cc))
        : [ (ccTarget cc, dry (ccTargetBed cc)) | targetDeclared ]
    , fxBarriers    = []
    , fxSchedule    = []
    , fxDuration    = LogicalTime (10 * unLogicalTime referenceInterval)
    }
  where
    sourceChunk = tileChunk (ccSource cc)
    targetChunk = tileChunk (ccTarget cc)
    -- An absent chunk has no cells to declare: the archived case's
    -- target there is the 0 'characterizationSamples' reads.
    targetDeclared = targetChunk ≡ sourceChunk ∨ ccTargetPresent cc

-- | Per step: (source units, target units, total units), the three
--   numbers the archive records. A target in an absent chunk holds 0.
characterizationSamples ∷ CharacterizationCase → Trajectory → [(Int, Int, Int)]
characterizationSamples cc tr =
    [ (at (ccSource cc), at (ccTarget cc), sum (map cellQuantity (M.elems s)))
    | r ← trRecords tr, let s = stSolved r
                            at t = maybe 0 cellQuantity (M.lookup t s) ]
