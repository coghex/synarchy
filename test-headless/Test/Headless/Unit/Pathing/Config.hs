{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Tests for Unit.Pathing.Config YAML decoding.
--   The subtle bit is the per-key optional merge: a partial document
--   must override only the keys it names and keep defaults for the
--   rest (the `.:?`/`.!=` gotcha — a `.:` would fail the whole parse on
--   one missing key). These tests pin that behaviour.
module Test.Headless.Unit.Pathing.Config (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import Unit.Pathing.Config

decode ∷ BS.ByteString → Either String PathingConfig
decode = either (Left . show) Right . Yaml.decodeEither'

spec ∷ Spec
spec = do
    describe "full document overrides every field" $
        it "parses all keys" $ do
            let yaml = BS.unlines
                    [ "climb_factor: 1.0"
                    , "ramp_factor: 2.0"
                    , "fall_factor: 3.0"
                    , "fall_trigger_drop: 4"
                    , "river_penalty: 5.0"
                    , "lake_penalty: 6.0"
                    , "replan_cost_threshold: 7.0"
                    ]
            decode yaml `shouldBe` Right PathingConfig
                { pcClimbFactor         = 1.0
                , pcRampFactor          = 2.0
                , pcFallFactor          = 3.0
                , pcFallTriggerDrop     = 4
                , pcRiverPenalty        = 5.0
                , pcLakePenalty         = 6.0
                , pcReplanCostThreshold = 7.0
                }

    describe "partial document keeps defaults for omitted keys" $
        it "overrides only the named key" $ do
            let yaml = "climb_factor: 99.0\n"
            decode yaml `shouldBe` Right defaultPathingConfig { pcClimbFactor = 99.0 }

    describe "empty mapping yields all defaults" $
        it "decodes {} to the default profile" $
            decode "{}\n" `shouldBe` Right defaultPathingConfig

    describe "normalizePathingConfig clamps unsafe values" $ do
        it "raises fall_trigger_drop ≤ 0 to 1 (a 0 makes flat ground a fall)" $ do
            pcFallTriggerDrop (normalizePathingConfig
                defaultPathingConfig { pcFallTriggerDrop = 0 }) `shouldBe` 1
            pcFallTriggerDrop (normalizePathingConfig
                defaultPathingConfig { pcFallTriggerDrop = -3 }) `shouldBe` 1

        it "clamps negative costs/penalties to 0 (A* needs non-negative weights)" $
            normalizePathingConfig PathingConfig
                { pcClimbFactor         = -1.0
                , pcRampFactor          = -2.0
                , pcFallFactor          = -3.0
                , pcFallTriggerDrop     = -1
                , pcRiverPenalty        = -4.0
                , pcLakePenalty         = -5.0
                , pcReplanCostThreshold = -6.0
                } `shouldBe` PathingConfig
                { pcClimbFactor         = 0.0
                , pcRampFactor          = 0.0
                , pcFallFactor          = 0.0
                , pcFallTriggerDrop     = 1
                , pcRiverPenalty        = 0.0
                , pcLakePenalty         = 0.0
                , pcReplanCostThreshold = 0.0
                }

        it "leaves a valid config unchanged" $
            normalizePathingConfig defaultPathingConfig `shouldBe` defaultPathingConfig
