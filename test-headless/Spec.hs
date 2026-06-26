module Main where

import UPrelude
import Test.Hspec
import Test.Headless.Harness (withHeadlessEngine)
import qualified Test.Headless.WorldGen as WorldGen
import qualified Test.Headless.WorldGen.Geology as Geology
import qualified Test.Headless.WorldGen.Parity as Parity
import qualified Test.Headless.WorldGen.Flatness as Flatness
import qualified Test.Headless.WorldGen.SoilGate as SoilGate
import qualified Test.Headless.WorldGen.Exposure as Exposure
import qualified Test.Headless.WorldGen.ZoomParity as ZoomParity
import qualified Test.Headless.WorldGen.BorderProbe as BorderProbe
import qualified Test.Headless.WorldGen.WrapSeam as WrapSeam
import qualified Test.Headless.Unit.Pathing.Cost as PathingCost
import qualified Test.Headless.Unit.Pathing.AStar as PathingAStar
import qualified Test.Headless.Unit.Render.PickFrame as PickFrame
import qualified Test.Headless.Unit.Anim as AnimTest
import qualified Test.Headless.Unit.Injury as InjuryTest
import qualified Test.Headless.Unit.Fall as FallTest
import qualified Test.Headless.Unit.Stats as StatsTest
import qualified Test.Headless.World.Save.Sanitize as SaveSanitize
import qualified Test.Headless.World.Spoil as Spoil
import qualified Test.Headless.Combat.Damage as CombatDamage
import qualified Test.Headless.Combat.Severing as CombatSevering
import qualified Test.Headless.Combat.Wounds as CombatWounds
import qualified Test.Headless.Magma.Shape as MagmaShape
import qualified Test.Headless.Sim.Seam as SimSeam
import qualified Test.Headless.Input.KeyNames as InputKeyNames
import qualified Test.Headless.World.Calendar as Calendar
import qualified Test.Headless.River.Graph as RiverGraph

main ∷ IO ()
main = hspec $ do
    -- ONE engine for all worldgen specs. Worlds are memoized by
    -- (seed, size, plateCount) via Test.Headless.Harness.sharedWorld
    -- — generation is the entire cost of this suite, so specs share
    -- worlds instead of regenerating identical ones per module
    -- (was 16 generations / ~185 s; now ~6 / well under a minute).
    aroundAll withHeadlessEngine $ do
        describe "World Generation" WorldGen.spec
        describe "Geology" Geology.spec
        describe "Chunk/Fast Parity" Parity.spec
        describe "Biome Flatness" Flatness.spec
        describe "Column Exposure" Exposure.spec
        describe "Zoom/Detail Parity" ZoomParity.spec
        describe "Border Probe" BorderProbe.spec
    describe "Wrap Seam" WrapSeam.spec
    describe "Unit.Pathing.Cost" PathingCost.spec
    describe "Unit.Pathing.AStar" PathingAStar.spec
    describe "Unit.Render.pickFrame" PickFrame.spec
    describe "Unit.Anim" AnimTest.spec
    describe "Unit.Injury" InjuryTest.spec
    describe "Unit.Fall" FallTest.spec
    describe "Unit.Stats" StatsTest.spec
    describe "World.Save.Sanitize" SaveSanitize.spec
    describe "World.Spoil" Spoil.spec
    describe "WorldGen.SoilGate" SoilGate.spec
    describe "Combat.Damage" CombatDamage.spec
    describe "Combat.Severing" CombatSevering.spec
    describe "Combat.Wounds" CombatWounds.spec
    describe "World.Magma.Shape" MagmaShape.spec
    describe "Sim.Fluid.Seam" SimSeam.spec
    describe "Input.KeyNames" InputKeyNames.spec
    describe "World.Calendar" Calendar.spec
    describe "River.Graph" RiverGraph.spec
