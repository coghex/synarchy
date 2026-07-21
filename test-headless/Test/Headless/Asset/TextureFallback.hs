{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | 'resolveTexturePath' (#478): a yaml-declared texture path that
--   resolves to a real file loads unchanged; one that doesn't falls back
--   to the caller's subset placeholder instead of the missing-file path
--   reaching the Vulkan loader (which throws 'TextureLoadFailed').
--
--   Also the home for the persistence contract's visual-fallback policy
--   (#767, requirement 12, @docs/persistence_contract.md@ SS4): a
--   missing equipment/subset-specific texture substitutes its caller's
--   own placeholder (proven above via 'resolveTexturePath' directly —
--   every YAML-loading call site in @Engine.Scripting.Lua.API.*@ passes
--   its OWN category fallback, e.g. @missing_equipment.png@ for items,
--   @notexture.png@ for materials, @unknown_flora.png@ for flora), and
--   any OTHER missing visual this build has no specialized placeholder
--   for falls through to the magenta/black checkerboard "undefined"
--   texture (below) -- never a load failure, and never something that
--   could invalidate an otherwise-valid save (contract SS4: "missing
--   visual assets never by themselves invalidate an otherwise coherent
--   save"). No terrain-SPECIFIC placeholder asset exists in this build
--   (a separately-tracked, out-of-scope asset-production gap per
--   contract SS4/SS8) -- a missing terrain texture falls through to this
--   SAME magenta-checkerboard policy, not a bespoke one.
module Test.Headless.Asset.TextureFallback (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector.Storable as Vec
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.API.YamlTextures (resolveTexturePath)
import Engine.Scripting.Lua.API.Units (unknownUnitTexture, unknownUnitAnimFrame)
import Engine.Graphics.Vulkan.Texture.Undefined (undefinedTextureData)
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

    -- Contract requirement 12's final fall-through: ANY missing visual
    -- with no specialized placeholder of its own (or a specialized
    -- placeholder this build hasn't produced yet) ends up here, never a
    -- load/render failure.
    describe "undefinedTextureData (magenta/black checkerboard, #767 requirement 12)" $ do
        it "is an 8x8 RGBA texture" $ \_env →
            Vec.length undefinedTextureData `shouldBe` 8 * 8 * 4

        it "is built ONLY from opaque magenta and opaque black texels" $ \_env → do
            let texels = [ Vec.slice (i * 4) 4 undefinedTextureData | i ← [0 .. 8 * 8 - 1] ]
                magenta = Vec.fromList [255, 0, 255, 255]
                black   = Vec.fromList [0, 0, 0, 255]
            all (\t → t ≡ magenta ∨ t ≡ black) texels `shouldBe` True

        it "actually alternates -- both colors are present, not a solid fill" $ \_env → do
            let texels = [ Vec.slice (i * 4) 4 undefinedTextureData | i ← [0 .. 8 * 8 - 1] ]
                magenta = Vec.fromList [255, 0, 255, 255]
                black   = Vec.fromList [0, 0, 0, 255]
            any (≡ magenta) texels `shouldBe` True
            any (≡ black) texels `shouldBe` True
