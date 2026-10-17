{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology brain and mental atomicity" gate (#2838, S11a of epic
--   #2827): the brain's values are derived on demand by one set of pure
--   rules, and every remaining writer commits an atomic batch.
module Test.Headless.Unit.PhysiologyBrainMental (spec) where

import UPrelude
import Test.Hspec
import qualified Test.Headless.Unit.PhysiologyBrainMental.Atomicity as Atomicity
import qualified Test.Headless.Unit.PhysiologyBrainMental.Machine as Machine
import qualified Test.Headless.Unit.PhysiologyBrainMental.Modes as Modes
import qualified Test.Headless.Unit.PhysiologyBrainMental.Rules as Rules

spec ∷ Spec
spec = describe "physiology brain and mental atomicity" $ do
    Rules.spec
    Atomicity.spec
    Machine.spec
    Modes.spec
