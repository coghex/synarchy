{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology stat modes" gate (#2831, S4 of epic #2827): the
--   acceptance of the activated mode machinery, run through the SHIPPED
--   Haskell and Lua consumers (the engine verbs, @scripts\/physiology.lua@,
--   @scripts\/unit_resources.lua@ and everything it ticks) over the REAL
--   @unit.*@ API, never through the accessor alone.
--
--   One module per contract area, each named for the design's cases:
--
--     * "Test.Headless.Unit.PhysiologyModes.Transitions": I5, T-6 to T-8,
--       T-10 — seeding, atomic refusals, modifiers, the raw-write gate,
--       hooks, derived freshness, bodyless units;
--     * "Test.Headless.Unit.PhysiologyModes.Neutrality": I1, I2, I3 —
--       Nominal neutrality, the six dependency edges against stubs, the
--       healthy steady state of each system alone;
--     * "Test.Headless.Unit.PhysiologyModes.Tissue": T-9 and the SD-18 flag —
--       a Nominal nutrition has no starvation death and recovers depleted
--       tissue, depletion-only;
--     * "Test.Headless.Unit.PhysiologyModes.Persistence": I6, T-10, T-11 —
--       the load gate's normalization table, defaults and pins, and the
--       round trips through the production codec;
--     * "Test.Headless.Unit.PhysiologyModes.Climates": T-1, T-2, T-4, T-5,
--       T-12, T-13 — the small creature, the reference heat, feedback, a
--       default-parameter override and the virtual meal;
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "physiology stat modes"'@.
module Test.Headless.Unit.PhysiologyModes (spec) where

import UPrelude
import Test.Hspec
import qualified Test.Headless.Unit.PhysiologyModes.Climates as Climates
import qualified Test.Headless.Unit.PhysiologyModes.Neutrality as Neutrality
import qualified Test.Headless.Unit.PhysiologyModes.Persistence as Persistence
import qualified Test.Headless.Unit.PhysiologyModes.Tissue as Tissue
import qualified Test.Headless.Unit.PhysiologyModes.Transitions as Transitions

spec ∷ Spec
spec = describe "physiology stat modes" $ do
    Transitions.spec
    Neutrality.spec
    Climates.spec
    Tissue.spec
    Persistence.spec
