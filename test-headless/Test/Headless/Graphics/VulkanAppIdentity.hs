-- | Default Vulkan application identity (issue #933). The shipped
--   default `GraphicsConfig` must identify the app to the Vulkan
--   driver/tooling as "Synarchy", not the leftover "Vulkan Device
--   Test" scaffold name. Pure configuration coverage — no Vulkan
--   instance, no GPU, no window.
module Test.Headless.Graphics.VulkanAppIdentity (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.Defaults (defaultGraphicsConfig)
import Engine.Graphics.Base (GraphicsConfig(..))

spec ∷ Spec
spec = do
    describe "default Vulkan application identity" $ do
        it "is exactly \"Synarchy\"" $ do
            gcAppName defaultGraphicsConfig `shouldBe` "Synarchy"
