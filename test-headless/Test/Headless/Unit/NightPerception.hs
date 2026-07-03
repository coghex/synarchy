{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'nightPerceptionFactor' (#315): the day/night
--   multiplier on combat awareness. Asserts the ramp is smooth, peaks
--   at noon, troughs (but never zeroes) at midnight, and is symmetric
--   around dawn/dusk — not balance numbers.
module Test.Headless.Unit.NightPerception (spec) where

import UPrelude
import Test.Hspec
import Unit.LineOfSight (nightPerceptionFactor)

spec ∷ Spec
spec = describe "nightPerceptionFactor" $ do
    it "is full (1.0) at noon" $
        nightPerceptionFactor 0.5 `shouldBe` 1.0
    it "is reduced but non-zero at midnight" $ do
        nightPerceptionFactor 0.0 `shouldSatisfy` (> 0.0)
        nightPerceptionFactor 0.0 `shouldSatisfy` (< 1.0)
    it "midnight (0.0) and the wrap-around (1.0) agree" $
        nightPerceptionFactor 0.0 `shouldBe` nightPerceptionFactor 1.0
    it "noon is strictly brighter than any other time of day" $ do
        nightPerceptionFactor 0.5 `shouldSatisfy` (> nightPerceptionFactor 0.25)
        nightPerceptionFactor 0.5 `shouldSatisfy` (> nightPerceptionFactor 0.75)
        nightPerceptionFactor 0.5 `shouldSatisfy` (> nightPerceptionFactor 0.0)
    it "dawn and dusk are symmetric around noon/midnight" $
        nightPerceptionFactor 0.25 `shouldBe` nightPerceptionFactor 0.75
    it "morning ramps monotonically up to noon" $ do
        nightPerceptionFactor 0.1 `shouldSatisfy` (< nightPerceptionFactor 0.3)
        nightPerceptionFactor 0.3 `shouldSatisfy` (< nightPerceptionFactor 0.5)
    it "evening ramps monotonically down from noon" $ do
        nightPerceptionFactor 0.5 `shouldSatisfy` (> nightPerceptionFactor 0.7)
        nightPerceptionFactor 0.7 `shouldSatisfy` (> nightPerceptionFactor 0.9)
