{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Tests for the pure bleeding-trail accumulator/mapping math (issue
--   #882): "Blood.Trail"'s 'consumeTrailMarks' (distance+cadence
--   gating, conserved volume, partition invariance) and its volume ->
--   texture-request mapping, plus "Combat.Wounds.Bleed"'s external- vs
--   internal-bleed kind classification the wound tick's conserved
--   external-loss accounting relies on.
module Test.Headless.Blood.Trail (spec) where

import UPrelude
import Test.Hspec
import Data.List (sort, nub)
import Unit.Types.Trail (TrailState(..), emptyTrailState)
import Blood.Trail
import Blood.Types (BloodStyle(..))
import Combat.Wounds.Bleed (isExternallyBleedingKind)

spec ∷ Spec
spec = do
  kindSpec
  gatingSpec
  conservationSpec
  partitionInvarianceSpec
  textureMappingSpec

kindSpec ∷ Spec
kindSpec = describe "Combat.Wounds.Bleed.isExternallyBleedingKind" $ do
    it "excludes internal, fracture, and concussion" $ do
        isExternallyBleedingKind "internal"   `shouldBe` False
        isExternallyBleedingKind "fracture"   `shouldBe` False
        isExternallyBleedingKind "concussion" `shouldBe` False

    it "includes every other wound kind" $ do
        mapM_ (\k → isExternallyBleedingKind k `shouldBe` True)
            ["slash", "stab", "blunt", "arterial", "severed", "frostbite"]

gatingSpec ∷ Spec
gatingSpec = describe "Blood.Trail.consumeTrailMarks — distance/cadence gating" $ do

    it "pops nothing when distance hasn't been covered, even with ample time and volume" $ do
        let ts0 = TrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (ts', marks) = consumeTrailMarks defaultTrailThresholds 0.1 100.0 100.0 ts0
        marks `shouldBe` []
        tsPendingVolume ts' `shouldBe` 5.0   -- untouched — nothing consumed

    it "pops nothing when cadence hasn't elapsed, even with ample distance and volume" $ do
        let ts0 = TrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0, tsLastMarkAt = 100.0 }
            (ts', marks) = consumeTrailMarks defaultTrailThresholds 50.0 0.01 100.01 ts0
        marks `shouldBe` []
        tsPendingVolume ts' `shouldBe` 5.0

    it "pops exactly one mark once BOTH gates are cleared" $ do
        let tp  = defaultTrailThresholds
            ts0 = TrailState { tsPendingVolume = 0.2, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (_, marks) = consumeTrailMarks tp (ttMinDistance tp) (ttMinCadence tp)
                            (ttMinCadence tp) ts0
        length marks `shouldBe` 1

    it "never pops a mark with no real blood behind it, even when both gates clear" $ do
        -- A stale distance/cadence bank from a bleed that has since
        -- stopped (clotted/bandaged to zero) must not manufacture a
        -- mark once nothing is left to spend (issue #882 requirement
        -- 2/3 — never a fixed per-tick/per-gate stamp).
        let tp  = defaultTrailThresholds
            ts0 = TrailState { tsPendingVolume = 0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (ts', marks) = consumeTrailMarks tp (ttMinDistance tp) (ttMinCadence tp)
                            (ttMinCadence tp) ts0
        marks `shouldBe` []
        -- The banked progress itself is preserved (not silently reset),
        -- so real volume arriving later still needs a full fresh gate.
        tsDistSinceMark ts' `shouldBe` ttMinDistance tp

conservationSpec ∷ Spec
conservationSpec = describe "Blood.Trail.consumeTrailMarks — conserved volume" $ do

    it "never invents or drops volume: popped marks sum to the pending total" $ do
        let tp  = defaultTrailThresholds
            ts0 = TrailState { tsPendingVolume = 3.7, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            -- A single big jump clearing 10 multiples of BOTH gates at once
            -- (the "catch-up" case) — every drop of pendingVolume must show
            -- up across the popped marks, never silently dropped.
            (ts', marks) = consumeTrailMarks tp (10 * ttMinDistance tp)
                                              (10 * ttMinCadence tp) (10 * ttMinCadence tp) ts0
        length marks `shouldBe` 10
        sum (map tmoVolume marks) `shouldSatisfy` (\v → abs (v - 3.7) < 1e-4)
        tsPendingVolume ts' `shouldBe` 0

    it "catch-up marks spread across the step (never all stamped at one endpoint)" $ do
        let tp  = defaultTrailThresholds
            ts0 = TrailState { tsPendingVolume = 1.0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (_, marks) = consumeTrailMarks tp (10 * ttMinDistance tp)
                                            (10 * ttMinCadence tp) (10 * ttMinCadence tp) ts0
            fracs = map tmoFraction marks
        length marks `shouldBe` 10
        fracs `shouldBe` sort fracs   -- non-decreasing, so spread in order
        fracs `shouldBe` nub fracs    -- and all distinct (evenly spaced)
        all (\f → f > 0 ∧ f ≤ 1) fracs `shouldBe` True

    it "leftover distance/time banked below a gate carries forward, never lost" $ do
        let tp  = defaultTrailThresholds
            ts0 = TrailState { tsPendingVolume = 0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            -- Half the distance gate, half the cadence gate: no mark yet,
            -- but the progress must still be recorded for next tick.
            (ts', marks) = consumeTrailMarks tp (ttMinDistance tp / 2)
                                              (ttMinCadence tp / 2) (ttMinCadence tp / 2) ts0
        marks `shouldBe` []
        tsDistSinceMark ts' `shouldSatisfy` (\d → abs (d - ttMinDistance tp / 2) < 1e-6)

partitionInvarianceSpec ∷ Spec
partitionInvarianceSpec =
  describe "Blood.Trail.consumeTrailMarks — timestep-partition invariance" $ do

    it "an evenly-divisible path splits into the SAME total marks/volume/positions regardless of step count" $
        checkPartitionInvariance 10 10 2.0 100

    it "an UNEVENLY-divisible path still matches within a small tolerance" $
        checkPartitionInvariance 10.3 10.3 2.37 37

    it "a CADENCE-limited path (distance alone would allow more marks than \
       \cadence does) still matches — round-2 review regression" $
        -- distGates=3 (3 tiles at ttMinDistance=1.0) but cadenceGates=2
        -- (1 second at ttMinCadence=0.5): distance alone would allow 3
        -- marks, cadence caps it to 2 — exactly the asymmetric scenario
        -- where distance-ONLY positioning broke (it placed marks at the
        -- distance multiples 1,2 instead of where cadence actually
        -- cleared, 1.5,3.0, and disagreed across different chunkings).
        checkPartitionInvariance 3.0 2.0 2.0 4

-- | Shared partition-invariance check (issue #882 spec addition, tightened
--   by round-2 review): the same continuous path (@distGates@/
--   @cadenceGates@ multiples of the thresholds) and external-loss budget
--   (@totalVol@), split into a single big update vs @steps@ small ones,
--   must converge to the SAME emitted mark count, total volume, AND
--   absolute positions — within a documented tolerance, since chunking
--   can shift exactly which step a boundary-crossing mark pops on by at
--   most one (Float summation: e.g. @0.05*10@ landing a hair below
--   @0.5@). Volume conservation itself (popped + still-pending =
--   everything ever added) is checked EXACTLY for both the single big
--   call and the many-small-steps run — nothing may be invented or
--   dropped regardless of how the same journey is chunked.
checkPartitionInvariance ∷ Float → Double → Float → Int → Expectation
checkPartitionInvariance distGates cadenceGates totalVol steps = do
    let tp = defaultTrailThresholds
        totalDist = distGates * ttMinDistance tp
        totalTime = cadenceGates * ttMinCadence tp
        (bigFinal, bigMarks) = consumeTrailMarks tp totalDist totalTime totalTime
                                    emptyTrailState { tsPendingVolume = totalVol }
        bigPositions = map (\m → tmoFraction m * totalDist) bigMarks

        stepDist = totalDist / fromIntegral steps
        stepTime = totalTime / fromIntegral steps
        stepVol  = totalVol  / fromIntegral steps
        goSteps _ _ ts acc 0 = (ts, acc)
        goSteps clock cumDist ts acc k =
            let ts1 = ts { tsPendingVolume = tsPendingVolume ts + stepVol }
                clock' = clock + stepTime
                (ts2, popped) = consumeTrailMarks tp stepDist stepTime clock' ts1
                positioned = [ (cumDist + tmoFraction m * stepDist, m) | m ← popped ]
            in goSteps clock' (cumDist + stepDist) ts2 (acc ++ positioned) (k - 1 ∷ Int)
        (smallFinal, smallPositioned) = goSteps (0 ∷ Double) (0 ∷ Float) emptyTrailState [] steps
        smallMarks = map snd smallPositioned
        smallPositions = map fst smallPositioned
        conserved final marks = sum (map tmoVolume marks) + tsPendingVolume final
    -- Exact conservation for EACH chunking, regardless of pop timing.
    conserved bigFinal bigMarks `shouldSatisfy` (\v → abs (v - totalVol) < 1e-4)
    conserved smallFinal smallMarks `shouldSatisfy` (\v → abs (v - totalVol) < 1e-3)
    -- Documented cross-chunking tolerance: a boundary-crossing mark can
    -- land one step early/late, never further.
    abs (length smallMarks - length bigMarks) `shouldSatisfy` (≤ 1)
    -- Positional invariance (round-2 review requirement): when chunking
    -- doesn't shift the mark COUNT, the absolute positions must agree
    -- closely too — not just fall in the same ballpark from re-derived
    -- totals.
    when (length smallMarks ≡ length bigMarks) $
        zipWith (\a b → abs (a - b)) bigPositions smallPositions
            `shouldSatisfy` all (< 0.05)

textureMappingSpec ∷ Spec
textureMappingSpec =
  describe "Blood.Trail volume -> texture-request mapping" $ do

    it "severity bucket never gets lighter as volume grows" $
        monotonic trailSeverityBucket

    it "footprint never shrinks as volume grows" $
        monotonic trailFootprint

    it "opacity never decreases as volume grows" $
        monotonic trailOpacity

    it "style stays within the drops/smear vocabulary (never pool/spatter/streak)" $
        mapM_ (\v → trailStyleFor v `shouldSatisfy` (`elem` [StyleDrops, StyleSmear]))
              [0, 0.001, 0.01, 0.05, 0.1, 0.3, 0.4, 1.0, 5.0]

    it "a heavier mark is never LESS opaque than a lighter one at the same style" $ do
        trailOpacity 0.4 `shouldSatisfy` (> trailOpacity 0.01)

  where
    volumes = [0, 0.001, 0.01, 0.02, 0.05, 0.08, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 1.0, 5.0]
    monotonic ∷ (Ord a, Show a) ⇒ (Float → a) → Expectation
    monotonic f =
        let vals = map f volumes
        in vals `shouldBe` sort vals
