-- | The interface a solver implements to run in the harness (#2719).
--
--   An adapter is handed a placed fixture and the step interval the run
--   selected, advances it one logical step of that interval at a time,
--   accepts cell-level edits the harness has already resolved, and
--   reports EXACT per-cell state in its own stored frame.
--   It knows nothing about the checks the harness makes: conservation,
--   barriers, storage and edit accounting are all judged by the harness
--   against the fixture's own declarations ("RiverRuntime.Harness.Run").
--
--   Face records are optional and explicitly so. An adapter that can
--   name every exchange it accepted reports 'FaceRecordsObserved', and
--   the harness then checks each record against the fixture geometry
--   and reconciles the records with the per-cell changes. An adapter
--   that cannot — the legacy solver, which records only cell volumes —
--   reports 'FaceRecordsUnavailable'. The two are never conflated: an
--   empty observed list is a claim that nothing crossed any face.
--   Candidate kernels (RVR-02) must supply face records.
module RiverRuntime.Harness.Adapter
    ( CellState(..)
    , cellQuantity
    , Observation
    , Exchange(..)
    , FaceRecords(..)
    , CellEdit(..)
    , SolverAdapter(..)
    , Adapter(..)
    , adapterInfo
    , AdapterInfo(..)
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import World.Fluid.Types (FluidType)
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Placement

-- | One cell's exact state. A dry cell has no fluid; a wet one names its
--   fluid and the units standing over its terrain.
data CellState = CellState
    { cellTerrain ∷ Elevation
    , cellFluid   ∷ Maybe (FluidType, Quantity)
    } deriving (Show, Eq)

cellQuantity ∷ CellState → Int
cellQuantity = maybe 0 (unQuantity . snd) . cellFluid

-- | Everything an adapter reports after a step or an edit, keyed by the
--   cells it stores.
type Observation = M.Map StoredCell CellState

-- | One accepted exchange during a step: units moved from one cell to
--   another.
data Exchange = Exchange
    { exFrom  ∷ StoredCell
    , exTo    ∷ StoredCell
    , exUnits ∷ Int
    } deriving (Show, Eq)

data FaceRecords
    = FaceRecordsUnavailable
      -- ^ This adapter cannot report exchanges; face-level checks are
      --   reported as unavailable, never as passed.
    | FaceRecordsObserved [Exchange]
      -- ^ Every exchange the step accepted.
    deriving (Show, Eq)

-- | A cell's required state after an edit. The harness computes it from
--   the fixture operation and the cell's current state; the adapter
--   makes the cell exactly that or refuses.
data CellEdit = CellEdit
    { ceCell   ∷ StoredCell
    , ceTarget ∷ CellState
    } deriving (Show, Eq)

data SolverAdapter s = SolverAdapter
    { saName      ∷ Text
    , saConfig    ∷ [(Text, Text)]
      -- ^ identity and configuration recorded in every archive
    , saIntervals ∷ [LogicalTime]
      -- ^ the step intervals this adapter genuinely implements; the
      --   harness refuses any other rather than relabel steps
    , saInit      ∷ LogicalTime → PlacementMap → Fixture → Either Text s
      -- ^ seeds a run at the chosen step interval, one of 'saIntervals';
      --   an adapter whose transition depends on the interval binds it
      --   here, so every later 'saStep' advances by exactly that much
    , saStep      ∷ s → (s, FaceRecords)
    , saEdit      ∷ [CellEdit] → s → Either Text s
    , saObserve   ∷ s → Observation
    }

data Adapter = ∀ s. Adapter (SolverAdapter s)

data AdapterInfo = AdapterInfo
    { aiName      ∷ Text
    , aiConfig    ∷ [(Text, Text)]
    , aiIntervals ∷ [LogicalTime]
    } deriving (Show, Eq)

adapterInfo ∷ Adapter → AdapterInfo
adapterInfo (Adapter sa) = AdapterInfo (saName sa) (saConfig sa) (saIntervals sa)
