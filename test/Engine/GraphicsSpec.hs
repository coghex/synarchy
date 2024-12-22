-- test/Engine/GraphicsSpec.hs
module Engine.GraphicsSpec (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types

spec ∷ Spec
spec = do
  describe "Graphics Configuration" $ do
    it "should create valid GraphicsConfig" $ do
      let config = GraphicsConfig
            { gcAppName   = "Synarchy"
            , gcWidth     = 800
            , gcHeight    = 600
            , gcDebugMode = True
            }
      gcWidth config `shouldBe` 800
      gcHeight config `shouldBe` 600
      gcAppName config `shouldBe` "Synarchy"
      gcDebugMode config `shouldBe` True
