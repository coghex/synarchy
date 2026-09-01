{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The unit loader's REAL registration boundary (#1259, TEX-3).
--
--   Everything here runs 'registerUnitDefs' — the function
--   'Engine.Scripting.Lua.API.Units.Yaml.loadUnitYamlFn' delegates to,
--   with only argument marshalling between them — against a live
--   headless 'EngineEnv', a real asset pool, and a real Lua→engine
--   queue. So these assert on what the loader ACTUALLY did: which
--   texture-upload messages it queued, and which unit definitions it
--   published into the unit manager.
--
--   Most examples INJECT the atlas resolver, because a synthetic
--   compiled tree cannot be created without writing into the repo's own
--   @assets\/textures\/units@ (where the #1257 inventory gate would then
--   find it); the resolver's own filesystem behaviour is covered against
--   a temp tree in "Test.Headless.Unit.Atlas", and what those examples
--   cover is everything downstream of it.
--
--   The LAST examples inject nothing. Every shipped GAMEPLAY unit has real
--   compiled artifacts, so the production resolver runs against each one's
--   shipped index, shipped YAML and shipped source art together. Asset-only
--   declarations are intentionally outside this registration boundary.
module Test.Headless.Unit.Atlas.Loader (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (nub)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlUnits (UnitYamlDef(..), loadUnitYaml)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv, loggerRef)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Scripting.Lua.API.Units.Yaml (AtlasResolver, registerUnitDefs)
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Unit.Atlas.Index (AtlasLoadError(..))
import Unit.Atlas.Types
import Unit.Atlas.Yaml (resolveUnitAtlases)
import Unit.Direction (Direction(..))
import Unit.Types

fixtureUnit ∷ Text
fixtureUnit = "spec_loader_unit"

-- Every shipped gameplay unit's own YAML — the real ones the game loads,
-- not fixtures (#1260 for acolyte, #1261 for the original remainder).
shippedUnits ∷ [Text]
shippedUnits =
    [ "acolyte", "bear_brown", "nomad_primitive"
    , "red_squirrel", "technomule"
    , "tiller", "unknown_unit", "white_tailed_deer" ]

unitYamlPath ∷ Text → FilePath
unitYamlPath name = "data" </> "units" </> T.unpack name ⧺ ".yaml"

-- The whole shipped corpus, pinned so a tree that silently stops
-- registering shows up as a number rather than as nothing at all.
shippedAnimationTotal ∷ Int
shippedAnimationTotal = 131

-- | A minimal but REAL unit YAML, parsed by the engine's own decoder.
--   Its frame paths deliberately do not exist. Nothing loads them since
--   #1261 — an animation's pixels come from its compiled atlas — and
--   the injected resolver supplies the selection, so this spec stays
--   about registration rather than about art being present.
fixtureYamlText ∷ String
fixtureYamlText = unlines
    [ "units:"
    , "  - name: " ⧺ T.unpack fixtureUnit
    , "    sprite: assets/textures/utility/blanktexture.png"
    , "    animations:"
    , "      walk:"
    , "        fps: 8"
    , "        loop: true"
    , "        flip: false"
    , "        frames:"
    , "          south: [\"walk/s/frame_000.png\", \"walk/s/frame_001.png\"]"
    , "      idle:"
    , "        fps: 4"
    , "        loop: true"
    , "        flip: false"
    , "        frames:"
    , "          south: [\"idle/s/frame_000.png\"]"
    ]

withFixtureYaml ∷ (FilePath → IO α) → IO α
withFixtureYaml action = do
    tmp ← getTemporaryDirectory
    let path = tmp </> "synarchy-unit-atlas-loader-spec.yaml"
    writeFile path fixtureYamlText
    action path `finally` removeFile path

-- | A compiled animation, as the resolver would report it.
atlasFor ∷ Text → Int → AtlasAnimation
atlasFor name frames = AtlasAnimation
    { aaName = name, aaFormat = AtlasFormatPng
    , aaPath = "assets/textures/units/" ⧺ T.unpack fixtureUnit
                   ⧺ "/atlas/" ⧺ T.unpack name ⧺ ".png"
    , aaAtlasWidth = frames * 18, aaAtlasHeight = 18
    , aaCellWidth = 16, aaCellHeight = 16, aaCellPadding = 1
    , aaColumns = frames, aaRows = 1
    , aaFlip = False, aaFps = 8, aaLoop = True
    , aaDirections = Map.singleton DirS (AtlasDirectionRow DirS 0 frames)
    , aaSourceDigest = "src", aaAtlasDigest = "atlas"
    }

-- | Run the real registration path with a canned atlas selection.
--   Returns @(defs published, queued messages)@.
runLoader ∷ EngineEnv → AtlasResolver → IO (Int, [LuaToEngineMsg])
runLoader env resolver = withFixtureYaml $ \yamlPath → do
    logger ← readIORef (loggerRef env)
    defs ← loadUnitYaml logger yamlPath
    length defs `seq` pure ()
    poolRef ← newIORef =<< defaultAssetPool
    q ← Q.newQueue
    n ← registerUnitDefs env poolRef q resolver yamlPath defs
    msgs ← Q.flushQueue q
    pure (n, msgs)

atlasRequests ∷ [LuaToEngineMsg] → [(TextureHandle, FilePath)]
atlasRequests msgs = [ (h, p) | LuaLoadAtlasTextureRequest h p ← msgs ]

plainRequests ∷ [LuaToEngineMsg] → [(TextureHandle, FilePath)]
plainRequests msgs = [ (h, p) | LuaLoadTextureRequest h p _ ← msgs ]

-- | The ordinary (non-atlas) uploads with the sampler policy each one
--   declared (#2075).
plainPolicies ∷ [LuaToEngineMsg] → [(FilePath, UploadSampler)]
plainPolicies msgs = [ (p, pol) | LuaLoadTextureRequest _ p pol ← msgs ]

publishedDef ∷ EngineEnv → IO (Maybe UnitDef)
publishedDef env = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    pure (HM.lookup fixtureUnit (umDefs um))

storageOf ∷ Text → UnitDef → Maybe AnimStorage
storageOf anim def = aStorage <$> HM.lookup anim (udAnimations def)

-- | Every direct single-texture reference a unit YAML makes — the
--   families D-8 leaves on ordinary loading.
nonAnimationArt ∷ UnitYamlDef → [Text]
nonAnimationArt d = uydSprite d
    : maybe [] pure (uydPortrait d)
    ⧺ Map.elems (uydDirectionalSprites d)

-- | Register one SHIPPED unit through the production resolver, against
--   its real YAML and its real compiled artifacts. Restores the shared
--   engine's definition table, so these examples stay independent of
--   each other and of everything above.
runShipped
    ∷ EngineEnv → Text
    → IO ([UnitYamlDef], [LuaToEngineMsg], Maybe UnitDef)
runShipped env unitName = do
    let defsRef = ucUnitManagerRef (toUnitCombatCapability env)
    um0 ← readIORef defsRef
    let restore = atomicModifyIORef' defsRef $ \um →
            (um { umDefs = umDefs um0 }, ())
    (`finally` restore) $ do
        logger ← readIORef (loggerRef env)
        defs ← loadUnitYaml logger (unitYamlPath unitName)
        poolRef ← newIORef =≪ defaultAssetPool
        q ← Q.newQueue
        _ ← registerUnitDefs env poolRef q resolveUnitAtlases
                (unitYamlPath unitName) defs
        msgs ← Q.flushQueue q
        um ← readIORef defsRef
        pure (defs, msgs, HM.lookup unitName (umDefs um))

spec ∷ SpecWith EngineEnv
spec = describe "Unit.Atlas.Load — the real unit registration boundary" $ do

    it "queues exactly ONE atlas upload per selected animation" $ \env → do
        let sel = HM.fromList [ ("walk", atlasFor "walk" 2)
                              , ("idle", atlasFor "idle" 1) ]
        (n, msgs) ← runLoader env (\_ _ → pure (Right sel))
        n `shouldBe` 1
        map snd (atlasRequests msgs) `shouldMatchList`
            [ aaPath (atlasFor "walk" 2), aaPath (atlasFor "idle" 1) ]
        -- Distinct handles: one bindless slot each (D-2/D-10), never a
        -- shared or reused one.
        let handles = map fst (atlasRequests msgs)
        length handles `shouldBe` 2
        length (HM.toList (HM.fromList [(h, ()) | h ← handles])) `shouldBe` 2

    it "publishes the definition with each animation on its own atlas handle" $ \env → do
        let sel = HM.fromList [ ("walk", atlasFor "walk" 2)
                              , ("idle", atlasFor "idle" 1) ]
        (_, msgs) ← runLoader env (\_ _ → pure (Right sel))
        mDef ← publishedDef env
        case mDef of
            Nothing → expectationFailure "expected the unit definition to publish"
            Just def → do
                let byPath = Map.fromList [ (p, h) | (h, p) ← atlasRequests msgs ]
                    check anim aa = case storageOf anim def of
                        Just (StorageAtlas res) → do
                            raAnimation res `shouldBe` aa
                            Just (raTexture res) `shouldBe`
                                Map.lookup (aaPath aa) byPath
                        Nothing → expectationFailure
                            (T.unpack anim ⧺ " was not published at all")
                check "walk" (atlasFor "walk" 2)
                check "idle" (atlasFor "idle" 1)

    it "queues no per-frame textures for an atlas-backed animation" $ \env → do
        let sel = HM.fromList [ ("walk", atlasFor "walk" 2)
                              , ("idle", atlasFor "idle" 1) ]
        (_, msgs) ← runLoader env (\_ _ → pure (Right sel))
        -- Only the unit's own sprite remains on the ordinary path; the
        -- three animation frames the YAML declares are NOT loaded, which
        -- is the whole point of one image per animation.
        length (plainRequests msgs) `shouldBe` 1

    -- Before #1261 an animation the selection did not name fell back to
    -- loading one texture per declared frame. There is no such path
    -- now, and the PRODUCTION resolver refuses to produce a partial
    -- selection at all (Test.Headless.Unit.Atlas covers that rule
    -- directly): a unit that declares animations and ships no compiled
    -- artifacts is refused outright, before any handle exists.
    it "refuses a unit whose tree ships no compiled artifacts, through \
       \the production resolver, publishing nothing" $ \env → do
        let defsRef = ucUnitManagerRef (toUnitCombatCapability env)
        atomicModifyIORef' defsRef $ \um →
            (um { umDefs = HM.delete fixtureUnit (umDefs um) }, ())
        (n, msgs) ← runLoader env resolveUnitAtlases
        n `shouldBe` 0
        msgs `shouldBe` []
        publishedDef env ⌦ (`shouldBe` Nothing)

    -- The rejection contract, at the boundary that matters: not one
    -- message queued and not one definition published.
    it "a rejected index queues NOTHING and publishes NOTHING" $ \env → do
        -- The examples above publish this unit into the SHARED engine's
        -- manager, so drop it first: "nothing was published" has to be
        -- measured from a state where it genuinely is not there, and
        -- that also makes this example independent of ordering.
        let defsRef = ucUnitManagerRef (toUnitCombatCapability env)
        atomicModifyIORef' defsRef $ \um →
            (um { umDefs = HM.delete fixtureUnit (umDefs um) }, ())
        um0 ← readIORef defsRef
        let before = HM.keys (umDefs um0)
            reject = AtlasLoadError
                { aleUnit = fixtureUnit, aleAnimation = Just "walk"
                , aleArtifact = "assets/textures/units/spec/atlas/walk.png"
                , aleReason = "spec-injected rejection" }
        (n, msgs) ← runLoader env (\_ _ → pure (Left reject))
        n `shouldBe` 0
        -- Not one upload queued: not the atlas, and not the unit's own
        -- sprite either, because the rejection happens before ANY handle
        -- is allocated.
        msgs `shouldBe` []
        um1 ← readIORef defsRef
        HM.keys (umDefs um1) `shouldMatchList` before
        HM.lookup fixtureUnit (umDefs um1) `shouldBe` Nothing

    -- #1260 (TEX-4)'s acolyte pilot, extended to the whole roster by
    -- #1261 (TEX-6). Everything above injects a canned selection; these
    -- use the PRODUCTION resolver against the artifacts actually checked
    -- in, because a synthetic selection cannot prove that a shipped
    -- index still describes its shipped YAML and its shipped source art.
    -- If any of the three drift, the resolver rejects and the example
    -- for that unit fails — which is the migration gate this installs.
    describe "the shipped roster, through the production resolver" $ do
        forM_ shippedUnits $ \unitName →
            it (T.unpack unitName ⧺ ": registers with every declared \
                \animation atlas-backed on its own handle, and loads no \
                \per-frame animation texture") $ \env → do
                (defs, msgs, mDef) ← runShipped env unitName
                length defs `shouldBe` 1
                case (defs, mDef) of
                    ([yamlDef], Just def) → do
                        let anims  = HM.toList (udAnimations def)
                            atlases = atlasRequests msgs
                            handles = [ raTexture res
                                      | (_, a) ← anims
                                      , StorageAtlas res ← [aStorage a] ]
                        -- Every animation the YAML declares is published,
                        -- and every published one is atlas-backed —
                        -- there is no other constructor for it to be.
                        map fst anims `shouldMatchList`
                            Map.keys (uydAnimations yamlDef)
                        length handles `shouldBe` length anims
                        -- One upload, one handle, one bindless slot per
                        -- animation, no two sharing either (D-2/D-10).
                        length (nub handles) `shouldBe` length anims
                        length atlases `shouldBe` length anims
                        length (nub (map snd atlases)) `shouldBe` length anims

                        -- The ONLY ordinary texture loads left are this
                        -- unit's NON-ANIMATION art. The allowance is
                        -- derived from the YAML rather than from a path
                        -- shape, because several of those fields
                        -- legitimately point AT an animation frame —
                        -- reusing one as a sprite/directional
                        -- sprite/portrait is explicitly legal (#1257)
                        -- and 20 shipped references do it, so "nothing
                        -- under animations/" would be the wrong rule and
                        -- would fail for a correct tree.
                        let allowed = nonAnimationArt yamlDef
                            loaded = map (T.pack ∘ snd) (plainRequests msgs)
                        loaded `shouldSatisfy` all (`elem` allowed)
                        -- and every one of them is actually requested, so
                        -- the allowance is a real upper bound rather than
                        -- a vacuous one.
                        nub loaded `shouldMatchList` nub allowed
                    _ → expectationFailure
                        (T.unpack unitName ⧺ " failed to register")

        -- #2075: the authored portrait is the ONE unit texture whose
        -- only consumer is a UI panel, so it is the one the loader
        -- declares as UI. Everything else it queues is world-drawn and
        -- stays on the player's filter. red_squirrel is the shipped def
        -- that ships a `portrait:`.
        it "declares the authored portrait as UI art and every other \
           \ordinary texture as scene art" $ \env → do
            (defs, msgs, _) ← runShipped env "red_squirrel"
            case defs of
                [yamlDef] → do
                    portraitPath ← case uydPortrait yamlDef of
                        Just p  → pure (T.unpack p)
                        Nothing → do
                            expectationFailure
                                "red_squirrel no longer declares a portrait; \
                                \pick another shipped unit that does"
                            pure ""
                    let queued = plainPolicies msgs
                    lookup portraitPath queued
                        `shouldBe` Just UploadPinnedNearest
                    [ path | (path, UploadPinnedNearest) ← queued ]
                        `shouldBe` [portraitPath]
                    -- The sprite and the directional sprites are the
                    -- world-drawn rest, and none of them slipped over.
                    [ path | (path, UploadGlobalSampler) ← queued ]
                        `shouldSatisfy` (not ∘ null)
                _ → expectationFailure "red_squirrel YAML no longer holds one def"

        it "registers the whole shipped gameplay animation corpus" $ \env → do
            totals ← forM shippedUnits $ \unitName → do
                (_, _, mDef) ← runShipped env unitName
                pure (maybe 0 (HM.size ∘ udAnimations) mDef)
            sum totals `shouldBe` shippedAnimationTotal
