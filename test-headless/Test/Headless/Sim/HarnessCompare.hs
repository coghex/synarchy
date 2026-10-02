-- | The harness's comparison metrics on hand-computable inputs (#2719,
--   requirement 6). Every expected number here can be checked by hand
--   from the definitions in "RiverRuntime.Harness.Compare".
module Test.Headless.Sim.HarnessCompare (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import World.Fluid.Types (FluidType(..))
import RiverRuntime.Harness.Adapter
import RiverRuntime.Harness.Catalog
import RiverRuntime.Harness.Compare
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Legacy (legacyAdapter)
import RiverRuntime.Harness.Run

-- | A 20-tile row at y = 0 on flat z0 terrain.
rowTiles ∷ S.Set Tile
rowTiles = S.fromList [ Tile x 0 | x ← [0 .. 19] ]

-- | The row with the first @n@ tiles holding @q@ units each.
wetPrefix ∷ Int → Int → M.Map Tile CellState
wetPrefix n q = M.fromList
    [ (Tile x 0, CellState (Elevation 0) (if x < n then Just (River, Quantity q) else Nothing))
    | x ← [0 .. 19] ]

series ∷ Int → [M.Map Tile CellState] → Series
series interval samples = Series (LogicalTime interval) rowTiles (zip [0 ..] samples)

compared ∷ [Milestone] → Series → Series → IO Comparison
compared ms a b = either (\e → expectationFailure (show e) ≫ error "unreachable") pure
                         (compareSeries ms a b)

spec ∷ Spec
spec = do
    describe "percentile convention" $
        it "is nearest rank: the ceil(0.95 n)-th smallest" $ do
            nearestRankP95 [0 .. 19] `shouldBe` 18
            nearestRankP95 [1 .. 10] `shouldBe` 10
            nearestRankP95 [1 .. 100] `shouldBe` 95
            nearestRankP95 [7] `shouldBe` 7
            nearestRankP95 [5, 0, 9, 1] `shouldBe` 9

    describe "equivalent surface" $ do
        it "is terrain plus quantity when wet, terrain alone when dry" $ do
            equivalentSurface (CellState (Elevation (-32)) (Just (River, Quantity 12)))
                `shouldBe` Surface (-20)
            equivalentSurface (CellState (Elevation 16) Nothing) `shouldBe` Surface 16

        it "is compared over the union of both wet extents, dry sides at terrain" $ do
            cmp ← compared [] (series 100000 [wetPrefix 10 8]) (series 100000 [wetPrefix 5 8])
            map tcSurface (cmpTimes cmp) `shouldBe` [SurfaceError 10 8 8]
            map tcWetDryCells (cmpTimes cmp) `shouldBe` [5]

        it "reports both-dry rather than a zero error" $ do
            cmp ← compared [] (series 100000 [wetPrefix 0 0]) (series 100000 [wetPrefix 0 0])
            map tcSurface (cmpTimes cmp) `shouldBe` [SurfaceBothDry]
            map tcWetDryCells (cmpTimes cmp) `shouldBe` [0]
            map tcBoundary (cmpTimes cmp) `shouldBe` [BoundaryBothEmpty]

        it "measures depth differences on a shared wet extent" $ do
            cmp ← compared [] (series 100000 [wetPrefix 4 8]) (series 100000 [wetPrefix 4 11])
            map tcSurface (cmpTimes cmp) `shouldBe` [SurfaceError 4 3 3]
            map tcWetDryCells (cmpTimes cmp) `shouldBe` [0]

    describe "wet boundary" $ do
        it "is the wet cells with a dry neighbour inside the fixture" $ do
            -- x = 9 borders the dry x = 10; x = 0 borders only the edge
            -- of the fixture, which is not a shoreline.
            wetBoundary rowTiles (wetPrefix 10 8) `shouldBe` S.fromList [Tile 9 0]
            wetBoundary rowTiles (wetPrefix 20 8) `shouldBe` S.empty

        it "measures the symmetric Chebyshev Hausdorff distance" $ do
            cmp ← compared [] (series 100000 [wetPrefix 10 8]) (series 100000 [wetPrefix 5 8])
            map tcBoundary (cmpTimes cmp) `shouldBe` [BoundaryTiles 5]
            boundaryDistance (S.fromList [Tile 3 3]) (S.fromList [Tile 4 4]) `shouldBe` BoundaryTiles 1
            boundaryDistance (S.fromList [Tile 0 0, Tile 6 0]) (S.fromList [Tile 1 0])
                `shouldBe` BoundaryTiles 5

        it "reports a boundary present in only one run without a number" $ do
            cmp ← compared [] (series 100000 [wetPrefix 0 0]) (series 100000 [wetPrefix 3 8])
            map tcBoundary (cmpTimes cmp) `shouldBe` [BoundaryOnlyIn RunB]
            map tcSurface (cmpTimes cmp) `shouldBe` [SurfaceError 3 8 8]

    describe "matched logical times" $
        it "reads a halved-step run at every second step" $ do
            let coarse = series 100000 (map (\k → wetPrefix k 8) [1 .. 4])
                fine   = series 50000 (map (\k → wetPrefix ((k + 3) `div` 2) 8) [0 .. 6])
            cmp ← compared [] coarse fine
            map (\tc → (unLogicalTime (tcTime tc), tcStepA tc, tcStepB tc)) (cmpTimes cmp)
                `shouldBe` [(0, 0, 0), (100000, 1, 2), (200000, 2, 4), (300000, 3, 6)]
            map tcWetDryCells (cmpTimes cmp) `shouldBe` [0, 0, 0, 0]

    describe "milestones" $ do
        let arrival t = Milestone "arrival" t (ArrivalAtLeast (Quantity 1))
            growing = series 100000 (map (\k → wetPrefix k 8) [1 .. 6])

        it "reports step-count differences at equal intervals" $ do
            let later = series 100000 (map (\k → wetPrefix (max 1 (k - 2)) 8) [1 .. 6])
            cmp ← compared [arrival (Tile 3 0)] growing later
            map mcDelta (cmpMilestones cmp) `shouldBe` [MilestoneDelta 200000 (Just 2)]
            map (fmap mhStep . mcA) (cmpMilestones cmp) `shouldBe` [Just 3]

        it "flags a milestone already satisfied at step 0" $ do
            cmp ← compared [arrival (Tile 0 0)] growing growing
            map mcA (cmpMilestones cmp) `shouldBe` [Just (MilestoneHit 0 (LogicalTime 0) True)]
            map mcDelta (cmpMilestones cmp) `shouldBe` [MilestoneDelta 0 (Just 0)]

        it "reports unmatched and never-reached milestones as such" $ do
            let stuck = series 100000 (replicate 6 (wetPrefix 1 8))
            cmp ← compared [arrival (Tile 4 0), arrival (Tile 15 0)] growing stuck
            map mcDelta (cmpMilestones cmp)
                `shouldBe` [MilestoneOnlyReachedBy RunA, MilestoneNeitherReached]
            map mcB (cmpMilestones cmp) `shouldBe` [Nothing, Nothing]

        it "scans each run's own samples, so a finer run can arrive between coarse samples" $ do
            let coarse = series 100000 (map (\k → wetPrefix k 8) [0, 0, 1, 1])
                fine   = series 50000 (map (\k → wetPrefix k 8) [0, 0, 0, 1, 1, 1, 1])
            cmp ← compared [arrival (Tile 0 0)] coarse fine
            map mcDelta (cmpMilestones cmp) `shouldBe` [MilestoneDelta (-50000) Nothing]

    describe "translation normalization" $ do
        it "refuses two runs over different tile sets" $
            compareSeries [] (series 100000 [wetPrefix 1 8])
                (Series (LogicalTime 100000) (S.insert (Tile 0 1) rowTiles) [(0, wetPrefix 1 8)])
                `shouldSatisfy` either (const True) (const False)

        it "compares whole-chunk translations, ordinary and wrapped, tile for tile" $
            forM_ [ordinaryPlacement, wrappedPlacement] $ \pl → do
                base ← legacyRun originPlacement
                moved ← legacyRun pl
                -- With the partition unchanged the legacy solver gives the
                -- same answer: recorded here, not required of a candidate.
                seSamples (trajectorySeries moved) `shouldBe` seSamples (trajectorySeries base)
                cmp ← compared [] (trajectorySeries base) (trajectorySeries moved)
                length (cmpTimes cmp) `shouldBe` 301
                cmpTimes cmp `shouldSatisfy` all (\tc →
                    zeroError (tcSurface tc)
                    ∧ tcWetDryCells tc ≡ 0 ∧ tcBoundary tc ≡ BoundaryTiles 0)

        it "measures the legacy partition dependence under re-partitioning translations" $
            forM_ [shiftedPlacement, wrappedShiftedPlacement] $ \pl → do
                base ← legacyRun originPlacement
                moved ← legacyRun pl
                cmp ← compared [] (trajectorySeries base) (trajectorySeries moved)
                -- Identical at step 0 (same declared state), different later:
                -- the legacy answer depends on where chunk seams fall.
                fmap tcSurface (listToMaybe (cmpTimes cmp)) `shouldSatisfy` maybe False zeroError
                cmpTimes cmp `shouldSatisfy` any (not . zeroError . tcSurface)
  where
    zeroError (SurfaceError _ 0 0) = True
    zeroError _                    = False
    legacyRun p = either (\e → expectationFailure (show e) ≫ error "unreachable") pure
        (runFixture legacyAdapter (RunConfig p referenceInterval) damDiversion)
