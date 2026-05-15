module Main where

import UPrelude
import Test.Hspec
import qualified Test.Headless.WorldGen as WorldGen
import qualified Test.Headless.WorldGen.Geology as Geology
import qualified Test.Headless.Unit.Pathing.Cost as PathingCost
import qualified Test.Headless.Unit.Pathing.AStar as PathingAStar

main ∷ IO ()
main = hspec $ do
    describe "World Generation" WorldGen.spec
    describe "Geology" Geology.spec
    describe "Unit.Pathing.Cost" PathingCost.spec
    describe "Unit.Pathing.AStar" PathingAStar.spec
