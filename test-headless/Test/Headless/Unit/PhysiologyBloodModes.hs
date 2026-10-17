{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology blood modes" gate (#2837, S10 of epic #2827).
module Test.Headless.Unit.PhysiologyBloodModes (spec) where

import UPrelude
import Test.Hspec
import qualified Test.Headless.Unit.PhysiologyBloodModes.Adapter as Adapter
import qualified Test.Headless.Unit.PhysiologyBloodModes.Modes as Modes
import qualified Test.Headless.Unit.PhysiologyBloodModes.Persistence as Persistence
import qualified Test.Headless.Unit.PhysiologyBloodModes.WoundFold as WoundFold

spec ∷ Spec
spec = describe "physiology blood modes" $ do
    Adapter.spec
    Modes.spec
    Persistence.spec
    WoundFold.spec
