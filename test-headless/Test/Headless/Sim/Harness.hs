-- | The controlled hydraulic experiment harness (#2719, RVR-01):
--   authored fixtures, explicit-step runs, the legacy adapter over the
--   real 'Sim.Fluid.Active.simulateActiveTick', and the exact
--   reproduction of the eight archived characterization cases
--   (@docs/evidence/river-runtime/baseline-solver.json@).
--
--   The solver-independent checks are proven against deliberately
--   invalid adapters in "Test.Headless.Sim.HarnessChecks", and the
--   comparison metrics on hand-computable inputs in
--   "Test.Headless.Sim.HarnessCompare". These pins RECORD current
--   behaviour; they do not endorse it (RVR-07 is expected to replace
--   the legacy expectations).
module Test.Headless.Sim.Harness (spec) where

import UPrelude
import Test.Hspec
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import World.Chunk.Types (ChunkCoord(..))
import World.Fluid.Types (FluidType(..))
import RiverRuntime.Harness.Adapter
import RiverRuntime.Harness.Catalog
import RiverRuntime.Harness.Compare
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Legacy
import RiverRuntime.Harness.Placement
import RiverRuntime.Harness.Run
import qualified Test.Headless.Sim.HarnessChecks as Checks
import qualified Test.Headless.Sim.HarnessCompare as Compare

spec ∷ Spec
spec = do
    describe "fixtures" fixtureSpec
    describe "explicit-duration stepping" steppingSpec
    describe "legacy adapter" legacySpec
    describe "archived legacy characterization" characterizationSpec
    describe "solver-independent checks" Checks.spec
    describe "comparison metrics" Compare.spec

runLegacy ∷ Placement → Fixture → Either Text Trajectory
runLegacy pl = runFixture legacyAdapter (RunConfig pl referenceInterval)

expectRun ∷ Either Text α → IO α
expectRun = either (\e → expectationFailure (T.unpack e) ≫ error "unreachable") pure

-- | The declared initial state of every fixture tile.
declared ∷ Fixture → M.Map Tile CellState
declared fx = M.fromList
    [ (t, CellState e (fmap (\(FluidSpec ty q) → (ty, q)) fl))
    | t ← fixtureTiles fx, let CellSpec e fl = initialCell fx t ]

catalog ∷ [Fixture]
catalog = map heFixture experiments

fixtureSpec ∷ Spec
fixtureSpec = do
    it "names five distinct authored fixtures" $
        L.nub (map fxName catalog) `shouldBe`
            ["channel-reservoir", "dam-diversion", "raised-sill", "lake-at-rest", "dry-bank"]

    it "covers multi-chunk channels, a closable dam, a side diversion, a sill, a lake and dry banks" $ do
        map (length . fxChunks) catalog `shouldBe` [3, 4, 2, 3, 2]
        map barrierName (fxBarriers damDiversion) `shouldBe` ["walls", "dam", "diversion-gate"]
        [ (at, op) | Scheduled at op ← fxSchedule damDiversion ] `shouldBe`
            [ (LogicalTime 5000000, SetTerrain (Tile 6 9) (zLevel (-5)) KeepQuantity)
            , (LogicalTime 10000000, SetTerrain (Tile 12 8) (zLevel (-4)) KeepQuantity)
            , (LogicalTime 20000000, SetTerrain (Tile 12 8) (zLevel 0) DisplaceToSink) ]

    it "validates every catalog fixture and every characterization fixture" $
        map validateFixture (catalog <> map characterizationFixture characterizationCases)
            `shouldBe` replicate (length catalog + length characterizationCases) []

    it "rejects authoring errors instead of guessing" $ do
        let base = raisedSill
            outside = Tile 40 40
        validateFixture base { fxCells = M.insert (Tile 3 7) (CellSpec (zLevel (-4))
                (Just (FluidSpec River (Quantity 0)))) (fxCells base) }
            `shouldSatisfy` any ("non-positive quantity" `T.isInfixOf`)
        validateFixture base { fxCells = M.insert outside (CellSpec (zLevel (-4)) Nothing) (fxCells base) }
            `shouldSatisfy` any ("outside every declared chunk" `T.isInfixOf`)
        validateFixture base { fxChunks = fxChunks base <> take 1 (fxChunks base) }
            `shouldSatisfy` any ("declared twice" `T.isInfixOf`)
        validateFixture base { fxBarriers = [Barrier "low" [Tile 0 0] (Elevation (-8))] }
            `shouldSatisfy` any ("not above the highest initial surface" `T.isInfixOf`)
        validateFixture base { fxSchedule = [Scheduled (fxDuration base)
                                               (RemoveFluid (Tile 3 7) (Quantity 1))] }
            `shouldSatisfy` any ("at or after the duration" `T.isInfixOf`)

    it "places the wrapped translation across the cylindrical u seam" $ do
        pm ← expectRun (placementMap wrappedPlacement damDiversion)
        -- Local chunk (0,0) lands at u = 31, the last column before the
        -- seam of a worldSize-64 page; local (1,0) is physically (32,0),
        -- u = 32, stored on the far side as (0,32) with BOTH coordinates
        -- changed. Local (0,1) stays inside the seam at (31,1).
        M.toList (pmChunks pm) `shouldBe`
            [ (LocalChunk 0 0, ChunkCoord 31 0), (LocalChunk 0 1, ChunkCoord 31 1)
            , (LocalChunk 1 0, ChunkCoord 0 32), (LocalChunk 2 0, ChunkCoord 1 32) ]
        normalizeCell pm (StoredCell (ChunkCoord 0 32) 0) `shouldBe` Just (Tile 16 0)
        normalizeCell pm (StoredCell (ChunkCoord 32 0) 0) `shouldBe` Nothing

    it "starts every fixture at every placement exactly as declared" $
        forM_ catalog $ \fx → forM_ standardPlacements $ \pl → do
            tr ← expectRun (runLegacy pl fx)
            fmap stSolved (listToMaybe (trRecords tr)) `shouldBe` Just (declared fx)

    it "runs every fixture at every placement with no check violation" $
        forM_ catalog $ \fx → forM_ standardPlacements $ \pl → do
            tr ← expectRun (runLegacy pl fx)
            (fxName fx, plName pl, trViolations tr) `shouldBe` (fxName fx, plName pl, [])

    it "accounts the closing dam's displaced water as a declared sink" $ do
        tr ← expectRun (runLegacy originPlacement damDiversion)
        let at k = trRecords tr !! k
            standing = maybe 0 cellQuantity (M.lookup (Tile 12 8) (stSolved (at 200)))
        standing `shouldSatisfy` (> 0)
        map aoDelta (stOps (at 200)) `shouldBe` [negate standing]
        totalQuantity (stAfterOps (at 200)) `shouldBe` totalQuantity (stSolved (at 200)) - standing

steppingSpec ∷ Spec
steppingSpec = do
    it "takes duration / interval steps and samples each logical time once" $ do
        tr ← expectRun (runLegacy originPlacement raisedSill)
        trSteps tr `shouldBe` 100
        map stStep (trRecords tr) `shouldBe` [0 .. 100]
        map (unLogicalTime . stTime) (trRecords tr) `shouldBe` map (* 100000) [0 .. 100]

    it "is deterministic: a rerun on identical inputs is identical" $
        forM_ catalog $ \fx → do
            a ← expectRun (runLegacy wrappedPlacement fx)
            b ← expectRun (runLegacy wrappedPlacement fx)
            a `shouldBe` b

    it "refuses an interval the adapter does not implement instead of relabelling" $
        runFixture legacyAdapter (RunConfig originPlacement (LogicalTime 50000)) raisedSill
            `shouldSatisfy` either ("does not implement interval" `T.isInfixOf`) (const False)

    it "refuses an operation that does not fall on a step" $
        runLegacy originPlacement raisedSill
            { fxSchedule = [Scheduled (LogicalTime 150000) (RemoveFluid (Tile 3 7) (Quantity 1))] }
            `shouldSatisfy` either ("does not fall on a step" `T.isInfixOf`) (const False)

    it "refuses a sink larger than the cell holds and a closing edit over water" $ do
        runLegacy originPlacement raisedSill
            { fxSchedule = [Scheduled (LogicalTime 0) (RemoveFluid (Tile 3 7) (Quantity 25))] }
            `shouldSatisfy` either ("exceeds what the cell holds" `T.isInfixOf`) (const False)
        runLegacy originPlacement raisedSill
            { fxBarriers = Barrier "gate" [Tile 3 7] (zLevel 0) : []
            , fxSchedule = [Scheduled (LogicalTime 0) (SetTerrain (Tile 3 7) (zLevel 0) KeepQuantity)] }
            `shouldSatisfy` either ("over standing fluid" `T.isInfixOf`) (const False)

legacySpec ∷ Spec
legacySpec = do
    it "reports face records as unavailable on every step, never as an empty list" $ do
        tr ← expectRun (runLegacy originPlacement channelReservoir)
        catMaybes (map stExchanges (trRecords tr)) `shouldBe` replicate 400 FaceRecordsUnavailable
        trFaceChecks tr `shouldBe` FaceChecksUnavailable

    it "rejects fractional terrain and out-of-range quantities rather than rounding" $ do
        runLegacy originPlacement raisedSill
            { fxCells = M.insert (Tile 3 7) (CellSpec (Elevation (-28)) Nothing) (fxCells raisedSill) }
            `shouldSatisfy` either ("not a whole z-level" `T.isInfixOf`) (const False)
        runLegacy originPlacement raisedSill
            -- No walls: a column that deep overtops them, which the
            -- schema would refuse before the adapter is asked.
            { fxBarriers = []
            , fxCells = M.insert (Tile 3 7) (CellSpec (zLevel (-4))
                (Just (FluidSpec River (Quantity 65536)))) (fxCells raisedSill) }
            `shouldSatisfy` either ("exceeds Word16 capacity" `T.isInfixOf`) (const False)
        runLegacy originPlacement raisedSill
            { fxSchedule = [Scheduled (LogicalTime 0) (SetTerrain (Tile 20 8) (Elevation (-36)) KeepQuantity)] }
            `shouldSatisfy` either ("not a whole z-level" `T.isInfixOf`) (const False)

    it "reads the passive plane after equilibrium deactivation, without double counting" $ do
        let resting = lakeAtRest { fxSchedule = [], fxDuration = LogicalTime 22000000 }
        (tr, final) ← expectRun (runFixtureWith legacySolver
                                    (RunConfig originPlacement referenceInterval) resting)
        -- 200 unchanged ticks deactivate every chunk...
        map snd (legacyActiveChunks final) `shouldBe` [False, False, False]
        -- ...and the quantities read from the baked passive plane are the
        -- active ones, cell for cell, before and after.
        trViolations tr `shouldBe` []
        [ stSolved r ≡ declared resting | r ← trRecords tr ] `shouldBe` replicate 221 True

    it "wakes declared-active chunks on a scheduled gate edit after deactivation" $ do
        (tr, final) ← expectRun (runFixtureWith legacySolver
                                    (RunConfig originPlacement referenceInterval) lakeAtRest)
        trViolations tr `shouldBe` []
        map snd (legacyActiveChunks final) `shouldBe` [True, True, True]
        let basin = Milestone "basin" (Tile 30 8) (ArrivalAtLeast (Quantity 1))
        fmap mhStep (milestoneHit (trajectorySeries tr) basin) `shouldSatisfy`
            maybe False (> 230)

    it "never wakes a chunk the fixture declared inactive" $ do
        let inactive = CharacterizationCase "inactive-neighbor" (Tile 15 8) (Tile 16 8)
                                                (-4) (-4) 24 True False
            fx = (characterizationFixture inactive)
                    { fxSchedule = [Scheduled (LogicalTime 200000)
                                      (SetTerrain (Tile 14 8) (zLevel (-4)) KeepQuantity)] }
        (tr, final) ← expectRun (runFixtureWith legacySolver
                                    (RunConfig originPlacement referenceInterval) fx)
        trViolations tr `shouldBe` []
        legacyActiveChunks final `shouldBe` [(ChunkCoord 0 0, True), (ChunkCoord 1 0, False)]

-- | The archived samples of baseline-solver.json, per case: ticks 0..10
--   of (source units, target units, total units).
archivedBaseline ∷ [(Text, [(Int, Int, Int)])]
archivedBaseline =
    [ ("raised-sill-interior", replicate 11 (24, 0, 24))
    , ("raised-sill-seam", replicate 11 (24, 0, 24))
    , ("downhill-control", map (\(s, t) → (s, t, 24))
        [(24, 0), (16, 8), (12, 12), (10, 14), (9, 15), (8, 16), (8, 16), (8, 16), (8, 16), (8, 16), (8, 16)])
    , ("one-level-interior", replicate 11 (8, 0, 8))
    , ("one-level-seam", map (\(s, t) → (s, t, 8))
        [(8, 0), (6, 2), (5, 3), (4, 4), (4, 4), (4, 4), (4, 4), (4, 4), (4, 4), (4, 4), (4, 4)])
    , ("inactive-neighbor", replicate 11 (24, 0, 24))
    , ("absent-neighbor", replicate 11 (24, 0, 24))
    , ("active-neighbor-control", map (\(s, t) → (s, t, 24))
        [(24, 0), (18, 6), (15, 9), (14, 10), (13, 11), (12, 12), (12, 12), (12, 12), (12, 12), (12, 12), (12, 12)])
    ]

characterizationSpec ∷ Spec
characterizationSpec = do
    it "names the eight archived cases in archive order" $
        map ccName characterizationCases `shouldBe` map fst archivedBaseline

    forM_ (zip characterizationCases archivedBaseline) $ \(cc, (_, expected)) →
        it ("reproduces " <> T.unpack (ccName cc) <> " exactly") $ do
            tr ← expectRun (runLegacy originPlacement (characterizationFixture cc))
            characterizationSamples cc tr `shouldBe` expected
            trViolations tr `shouldBe` []
