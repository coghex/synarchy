-- | Explicit-duration stepping and the solver-independent checks
--   (#2719, requirements 3 and 5).
--
--   A run advances a placed fixture through an adapter for an explicit
--   number of logical steps. Nothing here reads a clock, a thread or a
--   scheduler: step @k@ IS logical time @k * interval@.
--
--   == Step order
--
--   At each step @k@, from @0@ to the final step @n@:
--
--   1. The state the solver produced (the initial seeding, at @k = 0@)
--      is observed: 'stSolved'.
--   2. Every operation due at time @k * interval@ is resolved, in
--      declaration order, against the harness's own expected state and
--      handed to the adapter as one batch of cell edits; the result is
--      observed again: 'stAfterOps'. With nothing due the two are the
--      same observation.
--   3. Unless @k = n@, the adapter advances one step.
--
--   Samples for comparison are 'stAfterOps': the state at a logical time
--   is the state the solver advances FROM.
--
--   == What is checked, and against what
--
--   Every check reads the fixture's declarations and the harness's own
--   bookkeeping — never anything the adapter says about itself:
--
--   * __Initial quantities.__ The first observation equals the declared
--     cells exactly, terrain and fluid, cell for cell.
--   * __Storage.__ Every placed cell, and nothing else, is reported, with
--     a quantity in @0 .. 65535@ — the 'Word16' an active cell carries.
--   * __Terrain.__ A solver step never changes terrain; an edit sets
--     exactly the terrain the schedule says.
--   * __Edit accounting.__ An operation batch changes exactly the cells
--     it names, to exactly the resolved target, so the total moves by
--     exactly the declared sources, sinks and displacements.
--   * __Solver accounting.__ A step conserves the total exactly.
--   * __Barriers.__ A closed barrier cell holds nothing after a step, and
--     no quantity crosses one: every connected region the closed cells
--     separate conserves its own total across the step. Per-cell
--     snapshots cannot see two compensating transfers through the same
--     closed face within one step, so for an adapter without face
--     records that face-level property is reported UNAVAILABLE, not
--     passed ('trFaceChecks').
--   * __Face records__, when the adapter supplies them: each exchange is
--     between two face-adjacent fixture cells, neither a closed barrier,
--     with a positive amount, and the records reconcile exactly with
--     every cell's change over the step.
--
--   A violation is recorded, not thrown, so a run reports every
--   diagnostic it found. An authoring error the harness cannot account
--   for — an interval the adapter does not implement, an operation that
--   does not fall on a step, a sink larger than what the cell holds —
--   refuses the run instead.
module RiverRuntime.Harness.Run
    ( RunConfig(..)
    , StepRecord(..)
    , AppliedOp(..)
    , Violation(..)
    , ViolationKind(..)
    , FaceCheckStatus(..)
    , Trajectory(..)
    , storageCapacity
    , runFixture
    , runFixtureWith
    , trajectorySeries
    , regionsAt
    , closedTiles
    , totalQuantity
    ) where

import UPrelude
import Control.Applicative ((<|>))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import RiverRuntime.Harness.Adapter
import RiverRuntime.Harness.Compare (Series(..))
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Placement

data RunConfig = RunConfig
    { rcPlacement ∷ Placement
    , rcInterval  ∷ LogicalTime
    } deriving (Show, Eq)

-- | One applied operation and the signed quantity it was declared to
--   account for: a source adds, a sink or a displacement removes, a
--   quantity-preserving terrain edit accounts for nothing.
data AppliedOp = AppliedOp
    { aoOp    ∷ Operation
    , aoDelta ∷ Int
    } deriving (Show, Eq)

data StepRecord = StepRecord
    { stStep      ∷ Int
    , stTime      ∷ LogicalTime
    , stSolved    ∷ M.Map Tile CellState
    , stOps       ∷ [AppliedOp]
    , stAfterOps  ∷ M.Map Tile CellState
    , stExchanges ∷ Maybe FaceRecords
      -- ^ the records of the step that produced 'stSolved' (none at
      --   step 0, which no step produced)
    } deriving (Show, Eq)

data ViolationKind
    = InitialMismatch
    | MissingCells
    | UnexpectedCells
    | NegativeStorage
    | StorageOverflow
    | EmptyWetCell
    | TerrainChangedBySolver
    | EditAccountingMismatch
    | UnexplainedTotalChange
    | BarrierOccupied
    | RegionLeak
    | ExchangeOutsideFixture
    | ExchangeNotAFace
    | ExchangeThroughBarrier
    | ExchangeNonPositive
    | ExchangeReconcileMismatch
    deriving (Show, Eq, Ord, Enum, Bounded)

data Violation = Violation
    { vStep   ∷ Int
    , vKind   ∷ ViolationKind
    , vDetail ∷ Text
    } deriving (Show, Eq)

data FaceCheckStatus
    = FaceChecksUnavailable
      -- ^ at least one step reported no face records
    | FaceChecksApplied
      -- ^ every step reported face records and they were checked
    deriving (Show, Eq)

data Trajectory = Trajectory
    { trFixture    ∷ Fixture
    , trConfig     ∷ RunConfig
    , trAdapter    ∷ AdapterInfo
    , trSteps      ∷ Int
    , trRecords    ∷ [StepRecord]
    , trViolations ∷ [Violation]
    , trFaceChecks ∷ FaceCheckStatus
    } deriving (Show, Eq)

-- | The largest quantity one cell may hold: the 'Word16' of an active
--   cell ('Sim.Fluid.Types.afcVolume').
storageCapacity ∷ Int
storageCapacity = fromIntegral (maxBound ∷ Word16)

runFixture ∷ Adapter → RunConfig → Fixture → Either Text Trajectory
runFixture (Adapter sa) cfg fx = fmap fst (runFixtureWith sa cfg fx)

-- | 'runFixture' for a concrete adapter, also returning its final
--   state so a caller can inspect adapter-specific detail.
runFixtureWith ∷ SolverAdapter s → RunConfig → Fixture
               → Either Text (Trajectory, s)
runFixtureWith sa cfg fx = do
    case validateFixture fx of
        []       → pure ()
        problems → Left ("invalid fixture " <> fxName fx <> ": " <> tshow problems)
    let LogicalTime interval = rcInterval cfg
        LogicalTime duration = fxDuration fx
    when (interval ≤ 0) $ Left "the step interval must be positive"
    unless (rcInterval cfg `elem` saIntervals sa) $
        Left ("adapter " <> saName sa <> " does not implement interval "
              <> tshow interval <> "; refusing to relabel its steps")
    unless (duration `mod` interval ≡ 0) $
        Left ("duration " <> tshow duration <> " is not a whole number of "
              <> tshow interval <> " steps")
    forM_ (fxSchedule fx) $ \(Scheduled (LogicalTime at) op) →
        unless (at `mod` interval ≡ 0) $
            Left ("operation " <> tshow op <> " at " <> tshow at
                  <> " does not fall on a step of " <> tshow interval)
    pm ← placementMap (rcPlacement cfg) fx
    s0 ← saInit sa pm fx
    let steps = duration `div` interval
        initialTerrain = M.fromList
            [ (t, csTerrain (initialCell fx t)) | t ← fixtureTiles fx ]
        (initialObs, initialProblems) = normalize pm 0 (saObserve sa s0)
        initialViolations = initialProblems <> initialMismatches fx initialObs
    (records, violations, faceAvail, final) ←
        loop pm steps 0 s0 initialObs Nothing initialTerrain
             [] initialViolations True
    pure ( Trajectory
            { trFixture    = fx
            , trConfig     = cfg
            , trAdapter    = AdapterInfo (saName sa) (saConfig sa) (saIntervals sa)
            , trSteps      = steps
            , trRecords    = records
            , trViolations = violations
            , trFaceChecks = if faceAvail then FaceChecksApplied
                                          else FaceChecksUnavailable
            }
         , final )
  where
    interval = unLogicalTime (rcInterval cfg)
    loop pm steps k s solved exchanges terrain accRecords accViolations faceAvail = do
        let due = [ op | Scheduled (LogicalTime at) op ← fxSchedule fx
                       , at ≡ k * interval ]
        (applied, targets, terrain') ← resolveOps fx terrain solved due
        (s', afterOps, editViolations) ←
            if null due then pure (s, solved, []) else do
                let edits = [ CellEdit (pmForward pm M.! t) c | (t, c) ← M.toList targets ]
                s' ← either (\e → Left ("adapter refused edits at step " <> tshow k
                                        <> ": " <> e)) Right (saEdit sa edits s)
                let (obs, problems) = normalize pm k (saObserve sa s')
                pure ( s', obs
                     , problems <> editAccounting k solved targets obs
                       <> totalAccounting k solved applied obs )
        let record = StepRecord
                { stStep = k, stTime = LogicalTime (k * interval)
                , stSolved = solved, stOps = applied, stAfterOps = afterOps
                , stExchanges = exchanges }
            accRecords' = record : accRecords
            accViolations' = accViolations <> editViolations
        if k ≡ steps
        then pure (reverse accRecords', accViolations', faceAvail, s')
        else do
            let (s'', faces) = saStep sa s'
                (next, problems) = normalize pm (k + 1) (saObserve sa s'')
                closed = closedTiles fx terrain'
                stepViolations = problems
                    <> solverChecks fx (k + 1) closed terrain' afterOps next
                    <> faceChecks fx pm (k + 1) closed afterOps next faces
                faceAvail' = faceAvail ∧ faces ≢ FaceRecordsUnavailable
            loop pm steps (k + 1) s'' next (Just faces) terrain'
                 accRecords' (accViolations' <> stepViolations) faceAvail'

-- | Normalize an observation into fixture tiles, reporting cells the
--   placement never produced, missing cells, and invalid storage.
normalize ∷ PlacementMap → Int → Observation
          → (M.Map Tile CellState, [Violation])
normalize pm k obs = (cells, unexpected <> missing <> storage)
  where
    cells = M.fromList [ (t, c) | (sc, c) ← M.toList obs
                                , Just t ← [normalizeCell pm sc] ]
    unexpected =
        [ Violation k UnexpectedCells (tshow (take 4 extra) <> " (" <> tshow (length extra) <> ")")
        | let extra = [ sc | sc ← M.keys obs, isNothing (normalizeCell pm sc) ]
        , not (null extra) ]
    missing =
        [ Violation k MissingCells (tshow (take 4 gone) <> " (" <> tshow (length gone) <> ")")
        | let gone = M.keys (pmForward pm `M.difference` cells)
        , not (null gone) ]
    storage = concat
        [ [ Violation k NegativeStorage (tshow t <> " holds " <> tshow q) | q < 0 ]
          <> [ Violation k EmptyWetCell (tshow t <> " is wet with 0 units") | q ≡ 0 ]
          <> [ Violation k StorageOverflow (tshow t <> " holds " <> tshow q)
             | q > storageCapacity ]
        | (t, CellState _ (Just (_, Quantity q))) ← M.toList cells ]

initialMismatches ∷ Fixture → M.Map Tile CellState → [Violation]
initialMismatches fx obs =
    [ Violation 0 InitialMismatch
        (tshow t <> ": declared " <> tshow declared <> ", observed " <> tshow seen)
    | t ← fixtureTiles fx
    , let CellSpec e fl = initialCell fx t
          declared = CellState e (fmap (\(FluidSpec ty q) → (ty, q)) fl)
          seen = M.lookup t obs
    , isJust seen, seen ≢ Just declared ]

-- | Resolve the operations due now against the expected state, in
--   declaration order. Returns what each accounted for, the final target
--   of every touched cell, and the scheduled terrain afterwards.
resolveOps ∷ Fixture → M.Map Tile Elevation → M.Map Tile CellState → [Operation]
           → Either Text ([AppliedOp], M.Map Tile CellState, M.Map Tile Elevation)
resolveOps fx terrain current ops = go ops [] M.empty terrain
  where
    go [] applied targets terr = pure (reverse applied, targets, terr)
    go (op : rest) applied targets terr = do
        let t = opTile op
        CellState e fluid ← maybe
            (Left ("operation on " <> tshow t <> ", which the adapter did not report"))
            Right (M.lookup t targets <|> M.lookup t current)
        (delta, target) ← case op of
            SetTerrain _ e' KeepQuantity → do
                when (closesBarrier t e' ∧ isJust fluid) $
                    Left ("closing barrier cell " <> tshow t
                          <> " over standing fluid; declare DisplaceToSink")
                pure (0, CellState e' fluid)
            SetTerrain _ e' DisplaceToSink →
                pure (negate (maybe 0 (unQuantity . snd) fluid), CellState e' Nothing)
            AddFluid _ ty (Quantity q) → case fluid of
                Nothing → pure (q, CellState e (Just (ty, Quantity q)))
                Just (ty', Quantity q')
                    | ty' ≡ ty  → pure (q, CellState e (Just (ty, Quantity (q' + q))))
                    | otherwise → Left ("source of " <> tshow ty <> " into "
                                        <> tshow ty' <> " at " <> tshow t)
            RemoveFluid _ (Quantity q) → case fluid of
                Just (ty, Quantity q')
                    | q' > q  → pure (negate q, CellState e (Just (ty, Quantity (q' - q))))
                    | q' ≡ q  → pure (negate q, CellState e Nothing)
                _ → Left ("sink of " <> tshow q <> " at " <> tshow t
                          <> " exceeds what the cell holds")
        go rest (AppliedOp op delta : applied) (M.insert t target targets)
           (M.insert t (cellTerrain target) terr)
    opTile (SetTerrain t _ _) = t
    opTile (AddFluid t _ _)   = t
    opTile (RemoveFluid t _)  = t
    closesBarrier t e' = or [ e' ≥ barrierCrest b
                            | b ← fxBarriers fx, t `elem` barrierCells b ]

-- | An edit batch must change exactly the cells it names, to exactly
--   their targets. Totals then move by exactly the declared deltas.
editAccounting ∷ Int → M.Map Tile CellState → M.Map Tile CellState
               → M.Map Tile CellState → [Violation]
editAccounting k before targets after =
    [ Violation k EditAccountingMismatch
        (tshow t <> ": expected " <> tshow want <> ", observed " <> tshow got)
    | (t, got) ← M.toList after
    , let want = M.findWithDefault (M.findWithDefault got t before) t targets
    , got ≢ want ]

-- | The total after an edit batch differs from the total before it by
--   exactly the declared sources, sinks and displacements.
totalAccounting ∷ Int → M.Map Tile CellState → [AppliedOp] → M.Map Tile CellState
                → [Violation]
totalAccounting k before applied after =
    [ Violation k EditAccountingMismatch
        ("total " <> tshow (totalQuantity before) <> " plus declared "
         <> tshow declared <> " became " <> tshow (totalQuantity after))
    | totalQuantity after ≢ totalQuantity before + declared ]
  where declared = sum (map aoDelta applied)

-- | The barrier cells closed under a scheduled terrain.
closedTiles ∷ Fixture → M.Map Tile Elevation → S.Set Tile
closedTiles fx terrain = S.fromList
    [ t | b ← fxBarriers fx, t ← barrierCells b
        , M.findWithDefault (fxBaseTerrain fx) t terrain ≥ barrierCrest b ]

-- | The connected regions closed barriers separate: face-connected
--   components of the fixture's open tiles, each in ascending order.
regionsAt ∷ Fixture → S.Set Tile → [[Tile]]
regionsAt fx closed = go (S.fromList (fixtureTiles fx) `S.difference` closed) []
  where
    go open acc = case S.lookupMin open of
        Nothing → reverse acc
        Just seed →
            let region = flood open (S.singleton seed) [seed]
            in go (open `S.difference` region) (S.toAscList region : acc)
    flood _ seen [] = seen
    flood open seen (t : frontier) =
        let fresh = [ n | n ← cardinalTiles t, S.member n open, not (S.member n seen) ]
        in flood open (foldr S.insert seen fresh) (fresh <> frontier)

totalQuantity ∷ M.Map Tile CellState → Int
totalQuantity = sum . map cellQuantity . M.elems

solverChecks ∷ Fixture → Int → S.Set Tile → M.Map Tile Elevation
             → M.Map Tile CellState → M.Map Tile CellState → [Violation]
solverChecks fx k closed terrain before after = concat
    [ [ Violation k TerrainChangedBySolver
          (tshow t <> ": scheduled " <> tshow want <> ", observed " <> tshow (cellTerrain c))
      | (t, c) ← M.toList after
      , let want = M.findWithDefault (fxBaseTerrain fx) t terrain
      , cellTerrain c ≢ want ]
    , [ Violation k UnexplainedTotalChange
          ("total " <> tshow (totalQuantity before) <> " became " <> tshow (totalQuantity after))
      | totalQuantity before ≢ totalQuantity after ]
    , [ Violation k BarrierOccupied (tshow t <> " holds " <> tshow (cellQuantity c))
      | t ← S.toAscList closed, Just c ← [M.lookup t after], cellQuantity c ≢ 0 ]
    , [ Violation k RegionLeak
          ("region starting " <> tshow (take 1 region) <> ": "
           <> tshow was <> " became " <> tshow now)
      | not (S.null closed)
      , region ← regionsAt fx closed
      , let was = sumOver before region
            now = sumOver after region
      , was ≢ now ]
    ]
  where
    sumOver m = sum . map (\t → maybe 0 cellQuantity (M.lookup t m))

faceChecks ∷ Fixture → PlacementMap → Int → S.Set Tile
           → M.Map Tile CellState → M.Map Tile CellState → FaceRecords → [Violation]
faceChecks _ _ _ _ _ _ FaceRecordsUnavailable = []
faceChecks _ pm k closed before after (FaceRecordsObserved exchanges) =
    concatMap check exchanges <> reconcile
  where
    local sc = normalizeCell pm sc
    check ex@(Exchange from to units) = case (local from, local to) of
        (Just a, Just b) → concat
            [ [ Violation k ExchangeNonPositive (tshow ex) | units ≤ 0 ]
            , [ Violation k ExchangeNotAFace (tshow (a, b)) | b `notElem` cardinalTiles a ]
            , [ Violation k ExchangeThroughBarrier (tshow (a, b))
              | S.member a closed ∨ S.member b closed ]
            ]
        _ → [Violation k ExchangeOutsideFixture (tshow ex)]
    net = M.fromListWith (+) $ concat
        [ [(a, negate u), (b, u)]
        | Exchange from to u ← exchanges, Just a ← [local from], Just b ← [local to] ]
    reconcile =
        [ Violation k ExchangeReconcileMismatch
            (tshow t <> ": records net " <> tshow recorded <> ", cell changed by " <> tshow changed)
        | t ← L.nub (M.keys after <> M.keys net)
        , let recorded = M.findWithDefault 0 t net
              changed = maybe 0 cellQuantity (M.lookup t after)
                      - maybe 0 cellQuantity (M.lookup t before)
        , recorded ≢ changed ]

-- | The comparison view of a run: one sample per step, in the
--   fixture's own frame, at that step's logical time.
trajectorySeries ∷ Trajectory → Series
trajectorySeries tr = Series
    { seInterval = rcInterval (trConfig tr)
    , seTiles    = S.fromList (fixtureTiles (trFixture tr))
    , seSamples  = [ (stStep r, stAfterOps r) | r ← trRecords tr ]
    }
