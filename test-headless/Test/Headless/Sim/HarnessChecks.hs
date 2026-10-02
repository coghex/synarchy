-- | The harness's solver-independent checks, proven against
--   deliberately invalid adapters (#2719, requirement 5).
--
--   A scripted adapter seeds the fixture exactly (unless told to corrupt
--   it) and then does whatever its script says each step. A valid
--   script must pass with no violation; each invalid one must produce
--   its own diagnostic. Nothing here passes merely because a valid
--   legacy example does.
--
--   Fixture: a 4-cell, 16-unit pool along row 4 ending at A = (5,4),
--   a dry cell B = (5,5) in the same basin, the closed gate G = (6,4)
--   (a wall tile), and a dry channel E = (7,4) .. (9,4) beyond it.
module Test.Headless.Sim.HarnessChecks (spec) where

import UPrelude
import Test.Hspec
import qualified Data.List as L
import qualified Data.Map.Strict as M
import World.Chunk.Types (ChunkCoord(..))
import World.Fluid.Types (FluidType(..))
import RiverRuntime.Harness.Adapter
import RiverRuntime.Harness.Catalog (originPlacement, referenceInterval, withWalls, zLevel)
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Placement
import RiverRuntime.Harness.Run

tileA, tileB, gate, tileE ∷ Tile
tileA = Tile 5 4
tileB = Tile 5 5
gate  = Tile 6 4
tileE = Tile 7 4

checkFixture ∷ Fixture
checkFixture = withWalls Fixture
    { fxName        = "checks"
    , fxDescription = "pool, same-basin dry cell, closed gate, dry channel"
    , fxChunks      = [(LocalChunk 0 0, ResidentActive)]
    , fxBaseTerrain = zLevel 0
    , fxCells       = M.fromList $
        [ (Tile x 4, CellSpec (zLevel (-4)) (Just (FluidSpec River (Quantity 16)))) | x ← [2 .. 5] ]
        <> [ (tileB, CellSpec (zLevel (-4)) Nothing) ]
        <> [ (Tile x 4, CellSpec (zLevel (-4)) Nothing) | x ← [7 .. 9] ]
    , fxBarriers    = []
    , fxSchedule    = []
    , fxDuration    = LogicalTime 300000
    }

data Scripted = Scripted
    { spPlacement ∷ PlacementMap
    , spCells     ∷ Observation
    }

-- | A step script, written in fixture tiles through the placement.
type Script = PlacementMap → Observation → (Observation, FaceRecords)

scripted ∷ (PlacementMap → Observation → Observation) → Script
         → ([CellEdit] → Observation → Observation) → Adapter
scripted corrupt script edit = Adapter SolverAdapter
    { saName      = "scripted"
    , saConfig    = []
    , saIntervals = [referenceInterval]
    , saInit      = \pm fx → Right (Scripted pm (corrupt pm (M.fromList
        [ (sc, seeded (initialCell fx t)) | (t, sc) ← M.toList (pmForward pm) ])))
    , saStep      = \s → let (cells, faces) = script (spPlacement s) (spCells s)
                         in (s { spCells = cells }, faces)
    , saEdit      = \es s → Right s { spCells = edit es (spCells s) }
    , saObserve   = spCells
    }
  where
    seeded (CellSpec e fl) = CellState e (fmap (\(FluidSpec ty q) → (ty, q)) fl)

applyEdits ∷ [CellEdit] → Observation → Observation
applyEdits es o = foldl' (\m (CellEdit c t) → M.insert c t m) o es

at ∷ PlacementMap → Tile → StoredCell
at pm t = pmForward pm M.! t

-- | Add (or, negative, remove) units at a tile. Zero is dry; a negative
--   result is kept, so a script CAN report invalid storage.
addAt ∷ PlacementMap → Tile → Int → Observation → Observation
addAt pm t n = M.adjust bump (at pm t)
  where
    bump (CellState e fl) =
        let q = maybe 0 (unQuantity . snd) fl + n
        in CellState e (if q ≡ 0 then Nothing else Just (River, Quantity q))

setAt ∷ PlacementMap → Tile → CellState → Observation → Observation
setAt pm t c = M.insert (at pm t) c

move ∷ Int → Tile → Tile → PlacementMap → Observation → Observation
move n from to pm = addAt pm to n . addAt pm from (negate n)

unavailable ∷ (PlacementMap → Observation → Observation) → Script
unavailable f pm o = (f pm o, FaceRecordsUnavailable)

recorded ∷ (PlacementMap → Observation → Observation)
         → (PlacementMap → [Exchange]) → Script
recorded f ex pm o = (f pm o, FaceRecordsObserved (ex pm))

check ∷ Fixture → Adapter → IO Trajectory
check fx adapter = either (\e → expectationFailure (show e) ≫ error "unreachable") pure
    (runFixture adapter (RunConfig originPlacement referenceInterval) fx)

kinds ∷ Trajectory → [ViolationKind]
kinds = L.sort . L.nub . map vKind . trViolations

honest ∷ Script → Adapter
honest s = scripted (\_ o → o) s applyEdits

spec ∷ Spec
spec = do
    describe "valid adapters pass" $ do
        it "a conserving move with matching face records" $ do
            tr ← check checkFixture $ honest $
                recorded (move 1 tileA tileB) (\pm → [Exchange (at pm tileA) (at pm tileB) 1])
            trViolations tr `shouldBe` []
            trFaceChecks tr `shouldBe` FaceChecksApplied

        it "an observed empty exchange list over an unchanged state" $ do
            tr ← check checkFixture $ honest $ recorded (\_ o → o) (const [])
            trViolations tr `shouldBe` []
            trFaceChecks tr `shouldBe` FaceChecksApplied

        it "a conserving move with face records unavailable, reported as such" $ do
            tr ← check checkFixture $ honest $ unavailable (move 1 tileA tileB)
            trViolations tr `shouldBe` []
            trFaceChecks tr `shouldBe` FaceChecksUnavailable

    describe "invalid adapters are diagnosed" $ do
        it "incorrect initial quantities" $ do
            tr ← check checkFixture $
                scripted (\pm → addAt pm tileA (-1)) (unavailable (\_ o → o)) applyEdits
            kinds tr `shouldBe` [InitialMismatch]
            map vStep (trViolations tr) `shouldBe` [0]

        it "unexplained loss and gain" $ do
            loss ← check checkFixture $ honest $ unavailable (\pm → addAt pm tileA (-1))
            kinds loss `shouldSatisfy` elem UnexplainedTotalChange
            [ vStep v | v ← trViolations loss, vKind v ≡ UnexplainedTotalChange ]
                `shouldBe` [1, 2, 3]
            gain ← check checkFixture $ honest $ unavailable (\pm → addAt pm tileB 1)
            kinds gain `shouldSatisfy` elem UnexplainedTotalChange

        it "barrier leakage that conserves the total" $ do
            tr ← check checkFixture $ honest $ unavailable (move 1 tileA tileE)
            kinds tr `shouldBe` [RegionLeak]
            withRecords ← check checkFixture $ honest $
                recorded (move 1 tileA tileE) (\pm → [Exchange (at pm tileA) (at pm tileE) 1])
            kinds withRecords `shouldBe` [RegionLeak, ExchangeNotAFace]

        it "water standing in, or passing through, a closed barrier cell" $ do
            tr ← check checkFixture $ honest $
                recorded (move 1 tileA gate) (\pm → [Exchange (at pm tileA) (at pm gate) 1])
            kinds tr `shouldBe` [BarrierOccupied, RegionLeak, ExchangeThroughBarrier]

        it "invalid storage" $ do
            negative ← check checkFixture $ honest $
                unavailable (\pm → addAt pm tileA 1 . addAt pm tileB (-1))
            kinds negative `shouldSatisfy` elem NegativeStorage
            overflow ← check checkFixture $ honest $
                unavailable (\pm → setAt pm tileA (CellState (zLevel (-4)) (Just (River, Quantity 70000))))
            kinds overflow `shouldSatisfy` elem StorageOverflow
            emptyWet ← check checkFixture $ honest $
                unavailable (\pm → setAt pm tileB (CellState (zLevel (-4)) (Just (River, Quantity 0))))
            kinds emptyWet `shouldBe` [EmptyWetCell]
            missing ← check checkFixture $ honest $ unavailable (\pm → M.delete (at pm tileB))
            kinds missing `shouldBe` [MissingCells]
            extra ← check checkFixture $ honest $
                unavailable (\_ → M.insert (StoredCell (ChunkCoord 9 9) 0) (CellState (zLevel 0) Nothing))
            kinds extra `shouldBe` [UnexpectedCells]

        it "terrain changed by a solver step" $ do
            tr ← check checkFixture $ honest $
                unavailable (\pm → setAt pm tileB (CellState (zLevel (-5)) Nothing))
            kinds tr `shouldBe` [TerrainChangedBySolver]

        it "face records inconsistent with the cell changes" $ do
            short ← check checkFixture $ honest $
                recorded (move 2 tileA tileB) (\pm → [Exchange (at pm tileA) (at pm tileB) 1])
            kinds short `shouldBe` [ExchangeReconcileMismatch]
            zero ← check checkFixture $ honest $
                recorded (\_ o → o) (\pm → [Exchange (at pm tileA) (at pm tileB) 0])
            kinds zero `shouldBe` [ExchangeNonPositive]
            outside ← check checkFixture $ honest $
                recorded (\_ o → o) (\pm → [Exchange (StoredCell (ChunkCoord 9 9) 0) (at pm tileB) 1])
            kinds outside `shouldSatisfy` elem ExchangeOutsideFixture

    describe "edit accounting" $ do
        let sourced = checkFixture
                { fxSchedule = [Scheduled (LogicalTime 100000) (AddFluid tileB River (Quantity 5))] }

        it "accounts a declared source across the edit boundary" $ do
            tr ← check sourced $ honest $ unavailable (\_ o → o)
            trViolations tr `shouldBe` []
            let r = trRecords tr !! 1
            map aoDelta (stOps r) `shouldBe` [5]
            totalQuantity (stAfterOps r) `shouldBe` totalQuantity (stSolved r) + 5

        it "diagnoses an adapter that ignores the edit" $ do
            tr ← check sourced $ scripted (\_ o → o) (unavailable (\_ o → o)) (\_ o → o)
            kinds tr `shouldBe` [EditAccountingMismatch]
            map vStep (trViolations tr) `shouldBe` [1, 1]
