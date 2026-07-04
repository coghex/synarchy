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
import Engine.Scripting.Lua.API.Units (unknownUnitTexture, unknownUnitAnimFrame)
import Unit.Direction (Direction(..))

-- A real, always-present repo asset — stands in for both "the preferred
-- path" (existing case) and "the fallback path" (missing case) so the
-- test doesn't depend on any of the new #478 unknown-texture assets.
realAsset ∷ FilePath
realAsset = "assets/textures/ui/placeholders/missing_equipment.png"

spec ∷ SpecWith EngineEnv
spec = do
    describe "resolveTexturePath" $ do
        it "returns the preferred path when it exists" $ \env → do
            resolved ← resolveTexturePath env "Test" "assets/textures/does/not/exist.png" realAsset
            resolved `shouldBe` realAsset

        it "substitutes the fallback when the preferred path is missing" $ \env → do
            resolved ← resolveTexturePath env "Test" realAsset "assets/textures/does/not/exist.png"
            resolved `shouldBe` realAsset

    -- 'unknownUnitAnimFrame' (#485): idle/walk fall back to the
    -- unknown-unit's own authored clip, cycling by frame index; every
    -- other animation still freezes on the single static rotation.
    describe "unknownUnitAnimFrame" $ do
        it "picks the unknown-unit's idle clip for an in-range frame" $ \_env → do
            unknownUnitAnimFrame "idle" DirS 0
                `shouldBe` "assets/textures/units/unknown_unit/animations/idle/south/frame_000.png"

        it "wraps the frame index modulo the authored clip length" $ \_env → do
            unknownUnitAnimFrame "walk" DirE 6
                `shouldBe` "assets/textures/units/unknown_unit/animations/walk/east/frame_000.png"

        it "falls back to the static rotation for animations with no authored clip" $ \_env → do
            unknownUnitAnimFrame "attack_heavy_RH_dagger" DirN 2
                `shouldBe` unknownUnitTexture DirN
