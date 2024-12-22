-- test/Spec.hs
module Main where

import UPrelude
import Test.Hspec
import qualified Engine.GraphicsSpec

main ∷ IO ()
main = hspec $ do
  describe "Engine.Graphics" Engine.GraphicsSpec.spec
