{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Impact blood wound-kind/severity mapping tests (#607): the pure
--   decision behind "does a fresh wound draw blood, and how much" —
--   Blood.Impact.impactBloodForWound. No engine boot needed.
module Test.Headless.Blood.Impact (spec) where

import UPrelude
import Test.Hspec
import Blood.Types
import Blood.Impact

-- | Unwrap an expected 'Just', failing the example (not crashing) on
--   'Nothing' — avoids incomplete `let Just x = ...` pattern bindings,
--   which fail the -Werror=incomplete-uni-patterns CI gate.
expectJust ∷ HasCallStack ⇒ String → Maybe a → IO a
expectJust msg = maybe (expectationFailure msg >> error msg) pure

spec ∷ Spec
spec = do
    describe "impactBloodForWound / stab" $ do
        it "creates pool-style blood" $
            fmap ibStyle (impactBloodForWound "stab" 0.5) `shouldBe` Just StylePool

        it "a high-severity stab produces a stronger request than a \
           \low-severity one" $ do
            lo ← expectJust "low-severity stab should create blood"
                    (impactBloodForWound "stab" 0.1)
            hi ← expectJust "high-severity stab should create blood"
                    (impactBloodForWound "stab" 0.9)
            ibSeverity hi `shouldSatisfy` (> ibSeverity lo)
            ibOpacity hi `shouldSatisfy` (> ibOpacity lo)
            ibFootprint hi `shouldSatisfy` (> ibFootprint lo)

    describe "impactBloodForWound / slash" $
        it "creates streak-style blood" $
            fmap ibStyle (impactBloodForWound "slash" 0.5) `shouldBe` Just StyleStreak

    describe "impactBloodForWound / arterial and severed" $ do
        it "arterial always creates at least a moderate-severity mark" $ do
            ib ← expectJust "arterial should create blood"
                    (impactBloodForWound "arterial" 0.05)
            ibSeverity ib `shouldSatisfy` (≥ SeverityModerate)

        it "severed always creates at least a moderate-severity mark" $ do
            ib ← expectJust "severed should create blood"
                    (impactBloodForWound "severed" 0.05)
            ibSeverity ib `shouldSatisfy` (≥ SeverityModerate)

        it "a high-severity arterial wound still scales stronger than a \
           \barely-there one" $ do
            lo ← expectJust "low-severity arterial should create blood"
                    (impactBloodForWound "arterial" 0.1)
            hi ← expectJust "high-severity arterial should create blood"
                    (impactBloodForWound "arterial" 0.95)
            ibSeverity hi `shouldSatisfy` (> ibSeverity lo)
            ibOpacity hi `shouldSatisfy` (> ibOpacity lo)

    describe "impactBloodForWound / no direct blood" $ do
        it "internal never creates blood, regardless of severity" $ do
            impactBloodForWound "internal" 0.1 `shouldBe` Nothing
            impactBloodForWound "internal" 1.0 `shouldBe` Nothing

        it "ordinary blunt (below the catastrophic tier) creates no blood" $ do
            impactBloodForWound "blunt" 0.1 `shouldBe` Nothing
            impactBloodForWound "blunt" 0.5 `shouldBe` Nothing

        it "REGRESSION: a blunt hit merely bashed/slammed/clubbed \
           \(below the crushing/pulverizing/pulping T4 tier) must not \
           \spawn blood, even though it's a real, narrated hit" $
            -- 0.84 is one hundredth below catastrophicBluntThreshold
            -- (0.85, scripts/injury_log.lua's own T4 boundary) -- this
            -- is exactly the "bashes"/"smashes"-tier hit the issue
            -- calls out, not a graze.
            impactBloodForWound "blunt" 0.84 `shouldBe` Nothing

        it "ordinary fracture (below destroyThreshold) creates no blood" $
            impactBloodForWound "fracture" 0.5 `shouldBe` Nothing

        it "ordinary concussion (below the catastrophic tier) creates \
           \no blood" $
            impactBloodForWound "concussion" 0.5 `shouldBe` Nothing

    describe "impactBloodForWound / catastrophic blunt-family trauma" $ do
        it "blunt trauma at/above the crushing/pulverizing/pulping tier \
           \creates blood" $
            impactBloodForWound "blunt" 0.85 `shouldSatisfy` isJust'

        it "fracture at/above destroyThreshold (a crushed skull/ribcage) \
           \creates blood" $
            impactBloodForWound "fracture" 1.0 `shouldSatisfy` isJust'

        it "concussion at/above the catastrophic tier (a pulverized \
           \brain) creates blood" $
            impactBloodForWound "concussion" 0.85 `shouldSatisfy` isJust'

    describe "pickImpactWound" $ do
        it "returns Nothing when no candidate qualifies for blood" $
            pickImpactWound [("internal", 1.0), ("blunt", 0.1)] `shouldBe` Nothing

        it "picks the higher-severity-bucket wound, e.g. a catastrophic \
           \blunt hit over a merely-moderate stab" $ do
            (kind, _, _) ← expectJust "expected a qualifying candidate"
                (pickImpactWound [("stab", 0.3), ("blunt", 0.9)])
            kind `shouldBe` "blunt"

        it "REGRESSION: ranks by the FLOORED severity bucket, not raw \
           \opacity -- a barely-there arterial nick (floored to \
           \SeverityModerate) must outrank an even-lower-severity \
           \ordinary stab, even though the stab's raw opacity is \
           \numerically higher" $ do
            (kind, _, ib) ← expectJust "expected a qualifying candidate"
                (pickImpactWound [("stab", 0.05), ("arterial", 0.01)])
            -- Sanity: this only tests something if the stab really WOULD
            -- win on raw opacity alone (proving the old ranking was
            -- wrong, not just that both approaches happen to agree).
            stabIb ← expectJust "stab should create blood"
                (impactBloodForWound "stab" 0.05)
            arterialIbRaw ← expectJust "arterial should create blood"
                (impactBloodForWound "arterial" 0.01)
            ibOpacity stabIb `shouldSatisfy` (> ibOpacity arterialIbRaw)
            kind `shouldBe` "arterial"
            ibSeverity ib `shouldBe` SeverityModerate

        it "REGRESSION: a lower-severity wound that QUALIFIES beats a \
           \higher-severity wound that doesn't clear its OWN threshold \
           \(picking by raw severity first can miss the only \
           \qualifying candidate)" $ do
            -- fracture at 0.9 is below its own destroyThreshold (1.0)
            -- and does NOT qualify; concussion at 0.85 is AT its own
            -- catastrophic threshold and DOES -- even though 0.9 > 0.85.
            (kind, sev, _) ← expectJust "expected the concussion to qualify"
                (pickImpactWound [("fracture", 0.9), ("concussion", 0.85)])
            kind `shouldBe` "concussion"
            sev `shouldBe` 0.85

    describe "impactSeverityBucket" $ do
        it "mirrors injury_log.lua's T1..T4 tier boundaries" $ do
            impactSeverityBucket 0.10 `shouldBe` SeverityMinor
            impactSeverityBucket 0.30 `shouldBe` SeverityModerate
            impactSeverityBucket 0.60 `shouldBe` SeveritySevere
            impactSeverityBucket 0.90 `shouldBe` SeverityCatastrophic

    describe "impactFallbackAngle" $
        it "is deterministic for the same seed" $
            impactFallbackAngle 12345 `shouldBe` impactFallbackAngle 12345
  where
    isJust' (Just _) = True
    isJust' Nothing  = False
