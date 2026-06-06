module Main where

import UPrelude
import Test.Hspec
import qualified Test.Headless.WorldGen as WorldGen
import qualified Test.Headless.WorldGen.Geology as Geology
import qualified Test.Headless.WorldGen.Parity as Parity
import qualified Test.Headless.WorldGen.Flatness as Flatness
import qualified Test.Headless.WorldGen.Exposure as Exposure
import qualified Test.Headless.WorldGen.ZoomParity as ZoomParity
import qualified Test.Headless.Unit.Pathing.Cost as PathingCost
import qualified Test.Headless.Unit.Pathing.AStar as PathingAStar
import qualified Test.Headless.Unit.Render.PickFrame as PickFrame
import qualified Test.Headless.Unit.Anim as AnimTest
import qualified Test.Headless.Unit.Stats as StatsTest
import qualified Test.Headless.World.Save.Sanitize as SaveSanitize
import qualified Test.Headless.Magma.Shape as MagmaShape

main ∷ IO ()
main = hspec $ do
    describe "World Generation" WorldGen.spec
    describe "Geology" Geology.spec
    describe "Chunk/Fast Parity" Parity.spec
    describe "Biome Flatness" Flatness.spec
    describe "Column Exposure" Exposure.spec
    describe "Zoom/Detail Parity" ZoomParity.spec
    describe "Unit.Pathing.Cost" PathingCost.spec
    describe "Unit.Pathing.AStar" PathingAStar.spec
    describe "Unit.Render.pickFrame" PickFrame.spec
    describe "Unit.Anim" AnimTest.spec
    describe "Unit.Stats" StatsTest.spec
    describe "World.Save.Sanitize" SaveSanitize.spec
    describe "World.Magma.Shape" MagmaShape.spec
