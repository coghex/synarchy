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
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef)
import System.Directory
    (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv, floraCatalogRef, luaToEngineQueue, luaQueue
    , assetPoolRef, nextObjectIdRef, inputStateRef, loggerRef)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.YamlTextures (resolveTexturePath)
import Engine.Scripting.Lua.API.Units (unknownUnitTexture, unknownUnitAnimFrame)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Undefined (undefinedTextureData)
import Unit.Direction (Direction(..))
import World.Flora.Types (FloraCatalog(..), FloraId(..), FloraSpecies(..)
    , FloraHarvest(..), lookupSpecies)

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

    -- Round-2 review: the checkerboard-pixel tests above prove the
    -- fallback ASSET's own shape, but never exercise a real "missing
    -- specialized placeholder" asset-resolution route reaching it. The
    -- one such route this codebase actually implements (registerFloraSpecies,
    -- Engine.Scripting.Lua.API.YamlTextures) is a flora harvest whose YAML
    -- omits `harvested_texture` entirely: `fyhHarvestedTexture = Nothing`
    -- resolves DIRECTLY to `TextureHandle 0` -- the Undefined/magenta
    -- texture's own reserved bindless slot -- with no file-path resolution
    -- attempt at all (never mind one that could fail). This is the
    -- concrete, headless-reachable proof of contract requirement 12's
    -- "missing specialized placeholders fall through to magenta
    -- checkerboard": no GPU/render pipeline involved, so it holds
    -- regardless of whether this build can even open a window.
    describe "missing specialized-placeholder asset resolution (#767 \
             \requirement 12, round-2 review)" $ do
        it "a flora species with no harvested_texture resolves DIRECTLY \
           \to the Undefined texture's slot 0 -- never a file-path \
           \resolution attempt that could itself fail" $ \env → do
            tmp ← getTemporaryDirectory
            let dir = tmp </> "synarchy-texture-fallback-spec"
                path = dir </> "no_harvest_texture.yaml"
            createDirectoryIfMissing True dir
            writeFile path noHarvestedTextureYaml
            -- engine.loadFloraYaml queues a LuaLoadTextureRequest on the
            -- SHARED luaToEngineQueue for the species' own ground/phase
            -- texture (unrelated to the harvested_texture fallback this
            -- test targets) -- this env is shared across the whole
            -- aroundAll block (round-7 review), so a message left behind
            -- here would still be sitting in the queue when
            -- Test.Headless.Lua.RenderQueue's own discard-count test runs
            -- later and expects to start from empty. Flush it
            -- unconditionally in the finally, alongside the directory
            -- cleanup, so a failed assertion above still restores shared
            -- queue state for later specs.
            (`finally` cleanup dir env) $ do
                idBefore ← fcNextId <$> readIORef (floraCatalogRef env)
                ls ← newBareLuaBackend env
                result ← evalDebug ls
                    ("return engine.loadFloraYaml('" <> pathToLua path <> "')")
                result `shouldSatisfy` (`elem` ["1", "1.0"])
                catalog ← readIORef (floraCatalogRef env)
                case lookupSpecies (FloraId idBefore) catalog of
                    Nothing → expectationFailure
                        "the newly-registered species is missing from the catalog"
                    Just species → case fsHarvest species of
                        Nothing → expectationFailure
                            "the species was registered with no harvest block at all"
                        Just harvest →
                            fhHarvestedTexture harvest `shouldBe` TextureHandle 0

-- | Removes the temp fixture directory and flushes whatever
--   engine.loadFloraYaml queued onto the SHARED luaToEngineQueue (round-7
--   review: a leftover LuaLoadTextureRequest here would still be sitting
--   in the queue when a later spec's own discard-count assertion runs
--   against the same shared 'env'). Runs unconditionally via 'finally'
--   so a failed assertion above still restores shared queue state.
cleanup ∷ FilePath → EngineEnv → IO ()
cleanup dir env = do
    removeDirectoryRecursive dir
    void (Q.flushQueue (luaToEngineQueue env))

pathToLua ∷ FilePath → Text
pathToLua = T.pack

noHarvestedTextureYaml ∷ String
noHarvestedTextureYaml = unlines
    [ "flora:"
    , "  - name: test_fallback_species"
    , "    type: groundcover"
    , "    texDir: assets/textures/flora/unknown_flora_test_dir"
    , "    phases: []"
    , "    harvestable:"
    , "      tags: [leaves]"
    , "      yield:"
    , "        - id: test_item"
    , "          count: [1, 1]"
    , "      regrowth_time: 100"
    , "      # harvested_texture deliberately omitted"
    , "    worldGen:"
    , "      category: groundcover"
    , "      minTemp: 0"
    , "      maxTemp: 40"
    , "      idealTemp: 20"
    , "      minPrecip: 0"
    , "      maxPrecip: 100"
    , "      idealPrecip: 50"
    ]

-- | A bare Lua backend wired to a real engine, no world/window --
--   mirrors the identical pattern several other test-headless modules
--   already establish (e.g. Test.Headless.UI.Clipping's own
--   'newBareLuaBackend').
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses ('executeDebugLua').
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)
