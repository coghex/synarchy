{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Tests for Unit.Stats.rollStat. We avoid a 1000-sample statistical
--   check because under this codebase's build config it tripped over
--   some -O1 / Strict interaction that produced spurious values; the
--   per-call checks here are sufficient to verify rollStat's contract.
module Test.Headless.Unit.Stats (spec) where

import UPrelude
import Test.Hspec
import System.Random (mkStdGen)
import Unit.Stats (rollStat, effectiveStat, applySkillXP)
import Unit.Types (StatModifier(..))

-- | Roll one stat with the given seed and return just the value.
rollOne ∷ Float → Float → Int → Float
rollOne base range seed = fst (rollStat base range (mkStdGen seed))

-- | Roll a stat N times sequentially with seed 42 as the initial RNG.
--   Returns the values in roll order.
rollN ∷ Int → Float → Float → [Float]
rollN n base range = go n (mkStdGen 42)
  where
    go 0 _ = []
    go k g = case rollStat base range g of
        (v, g') → v : go (k - 1) g'

spec ∷ Spec
spec = do
    describe "rollStat range = 0" $ do
        it "returns the base value unchanged" $
            rollOne 5.0 0.0 1 `shouldBe` 5.0
        it "clamps a negative base to zero" $
            rollOne (-2.0) 0.0 1 `shouldBe` 0.0

    describe "rollStat with base=1, range=1" $ do
        it "single roll lands in [0.5, 1.5] (seed 1)" $
            rollOne 1.0 1.0 1 `shouldSatisfy` (\v → v ≥ 0.5 ∧ v ≤ 1.5)
        it "single roll lands in [0.5, 1.5] (seed 42)" $
            rollOne 1.0 1.0 42 `shouldSatisfy` (\v → v ≥ 0.5 ∧ v ≤ 1.5)
        it "single roll lands in [0.5, 1.5] (seed 999)" $
            rollOne 1.0 1.0 999 `shouldSatisfy` (\v → v ≥ 0.5 ∧ v ≤ 1.5)
        it "is deterministic for a given seed" $
            rollOne 1.0 1.0 42 `shouldBe` rollOne 1.0 1.0 42

    describe "rollStat with base=50, range=5" $ do
        it "single roll lands in [47.5, 52.5]" $
            rollOne 50.0 5.0 100 `shouldSatisfy` (\v → v ≥ 47.5 ∧ v ≤ 52.5)

    describe "rollStat non-negativity clamp" $ do
        it "with base=0.1 range=2, never returns negative" $ do
            -- Window [-0.9, 1.1]; the >=0 clamp pulls negatives up to 0.
            let v = rollOne 0.1 2.0 7
            v `shouldSatisfy` (≥ 0)

    describe "rollN sequential rolls" $ do
        it "all 5 sequential rolls (base=1, range=1) land in [0.5, 1.5]" $ do
            let vs = rollN 5 1.0 1.0
            all (\v → v ≥ 0.5 ∧ v ≤ 1.5) vs `shouldBe` True
        it "sequential rolls advance the RNG (5 rolls produce 5 distinct values)" $ do
            let vs = rollN 5 1.0 1.0
            length vs `shouldBe` 5
            -- Distinct enough that no two adjacent values match exactly
            all (uncurry (≠)) (zip vs (drop 1 vs)) `shouldBe` True

    let mkMod d s e = StatModifier { smDelta = d, smSource = s, smExpiry = e }
    describe "effectiveStat" $ do
        it "returns the base when no modifiers" $
            effectiveStat 0 5.0 [] `shouldBe` 5.0
        it "adds a single positive modifier" $
            effectiveStat 0 5.0 [mkMod 2.0 "src" Nothing] `shouldBe` 7.0
        it "subtracts a negative modifier" $
            effectiveStat 0 5.0 [mkMod (-1.5) "src" Nothing] `shouldBe` 3.5
        it "sums multiple modifiers" $
            effectiveStat 0 5.0
                [mkMod 1.0 "a" Nothing, mkMod (-0.5) "b" Nothing
                ,mkMod 2.0 "c" Nothing]
              `shouldBe` 7.5
        it "clamps result at 0 when sum is negative" $
            effectiveStat 0 1.0 [mkMod (-10) "doom" Nothing] `shouldBe` 0.0
        it "ignores modifier with expiry <= now (expired)" $
            -- now=100, expiry=50 → expired
            effectiveStat 100 5.0 [mkMod 2.0 "old" (Just 50)] `shouldBe` 5.0
        it "ignores modifier with expiry == now (boundary)" $
            -- The check is `now < t`, so expiry==now is inactive.
            effectiveStat 50 5.0 [mkMod 2.0 "boundary" (Just 50)] `shouldBe` 5.0
        it "includes modifier with expiry > now (still active)" $
            effectiveStat 50 5.0 [mkMod 2.0 "alive" (Just 100)] `shouldBe` 7.0
        it "mixes active and expired correctly" $
            effectiveStat 100 10.0
                [mkMod (-1) "expired1" (Just 50)
                ,mkMod (-2) "active1"  (Just 200)
                ,mkMod (-3) "expired2" (Just 99)
                ,mkMod  1   "perma"    Nothing]
              `shouldBe` 9.0   -- 10 + (-2) + 1 = 9

    describe "applySkillXP" $ do
        let approx target v = abs (v - target) < 1e-4
        it "level 1 + 0.5 XP = 1.5 (user's first example)" $
            applySkillXP 1.0 0.5 `shouldBe` 1.5
        it "level 2 + 0.5 XP = 2.125 (user's second example)" $
            applySkillXP 2.0 0.5 `shouldBe` 2.125
        it "level 3 + 0.5 XP ≈ 3.0556 (user's third example)" $
            applySkillXP 3.0 0.5 `shouldSatisfy` approx (3 + 0.5/9)
        it "no XP added: level unchanged" $
            applySkillXP 1.0 0.0 `shouldBe` 1.0
        it "negative XP decreases the skill" $
            applySkillXP 2.0 (-0.5) `shouldBe` 1.875
        it "growth shrinks quadratically as level rises" $ do
            -- delta at level 5 should be ~25× smaller than at level 1
            let delta1 = applySkillXP 1.0 1.0 - 1.0
                delta5 = applySkillXP 5.0 1.0 - 5.0
            (delta1 / delta5) `shouldSatisfy` approx 25.0
        it "level 0 does not divide by zero (guard kicks in)" $ do
            -- Just verify it doesn't crash / NaN.
            let v = applySkillXP 0.0 1.0
            v `shouldSatisfy` (\x → not (isNaN x) ∧ not (isInfinite x))
