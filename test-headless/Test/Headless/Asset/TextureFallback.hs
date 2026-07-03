{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | 'resolveTexturePath' (#478): a yaml-declared texture path that
--   resolves to a real file loads unchanged; one that doesn't falls back
--   to the caller's subset placeholder instead of the missing-file path
--   reaching the Vulkan loader (which throws 'TextureLoadFailed').
module Test.Headless.Asset.TextureFallback (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.API.YamlTextures (resolveTexturePath)

-- A real, always-present repo asset — stands in for both "the preferred
-- path" (existing case) and "the fallback path" (missing case) so the
-- test doesn't depend on any of the new #478 unknown-texture assets.
realAsset ∷ FilePath
realAsset = "assets/textures/ui/placeholders/missing_equipment.png"

spec ∷ SpecWith EngineEnv
spec =
    describe "resolveTexturePath" $ do
        it "returns the preferred path when it exists" $ \env → do
            resolved ← resolveTexturePath env "Test" "assets/textures/does/not/exist.png" realAsset
            resolved `shouldBe` realAsset

        it "substitutes the fallback when the preferred path is missing" $ \env → do
            resolved ← resolveTexturePath env "Test" realAsset "assets/textures/does/not/exist.png"
            resolved `shouldBe` realAsset
