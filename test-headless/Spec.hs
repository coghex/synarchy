module Main where

import UPrelude
import Test.Hspec
import qualified Test.Headless.WorldGen as WorldGen
import qualified Test.Headless.WorldGen.Rivers as Rivers
import qualified Test.Headless.WorldGen.Geology as Geology

main ∷ IO ()
main = hspec $ do
    describe "World Generation" WorldGen.spec
    describe "Rivers" Rivers.spec
    describe "Geology" Geology.spec
