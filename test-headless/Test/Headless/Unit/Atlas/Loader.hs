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
--   The LAST example injects nothing. Since #1260 one unit — @acolyte@ —
--   ships real compiled artifacts, so the production resolver can be run
--   against the shipped index, the shipped YAML and the shipped source
--   art together. That is the one thing a canned selection can never
--   assert: that the three still agree.
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

-- The shipped acolyte's own gameplay YAML — the real one the game
-- loads, not a fixture (#1260).
acolyteYamlPath ∷ FilePath
acolyteYamlPath = "data" </> "units" </> "acolyte.yaml"

-- | A minimal but REAL unit YAML, parsed by the engine's own decoder.
--   Its frame paths deliberately do not exist: the legacy loader
--   substitutes a fallback with a warning, which is exactly what a
--   half-authored unit does in production and keeps this spec about
--   registration rather than about art being present.
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
    , aaAtlasWidth = frames * 16, aaAtlasHeight = 16
    , aaCellWidth = 16, aaCellHeight = 16
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
plainRequests msgs = [ (h, p) | LuaLoadTextureRequest h p ← msgs ]

publishedDef ∷ EngineEnv → IO (Maybe UnitDef)
publishedDef env = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    pure (HM.lookup fixtureUnit (umDefs um))

storageOf ∷ Text → UnitDef → Maybe AnimStorage
storageOf anim def = aStorage <$> HM.lookup anim (udAnimations def)

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
                        other → expectationFailure
                            (T.unpack anim ⧺ " should be atlas-backed, got "
                             ⧺ show (fmap storageIsAtlas other))
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

    it "leaves an unselected animation on the legacy per-frame path" $ \env → do
        let sel = HM.singleton "walk" (atlasFor "walk" 2)
        (n, msgs) ← runLoader env (\_ _ → pure (Right sel))
        n `shouldBe` 1
        map snd (atlasRequests msgs) `shouldBe` [aaPath (atlasFor "walk" 2)]
        -- The unit sprite plus `idle`'s single frame.
        length (plainRequests msgs) `shouldBe` 2
        mDef ← publishedDef env
        case mDef of
            Nothing → expectationFailure "expected the unit definition to publish"
            Just def → do
                fmap storageIsAtlas (storageOf "walk" def) `shouldBe` Just True
                fmap storageIsAtlas (storageOf "idle" def) `shouldBe` Just False

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

    -- #1260 (TEX-4), the acolyte pilot. Everything above injects a
    -- canned selection; this one uses the PRODUCTION resolver against
    -- the artifacts actually checked in, because a synthetic selection
    -- cannot prove that the shipped index still describes the shipped
    -- YAML and the shipped source art. If any of the three drift, the
    -- resolver rejects and this example fails — which is the migration
    -- gate the pilot exists to install.
    it "registers the SHIPPED acolyte through the production resolver \
       \with every animation atlas-backed, and loads no per-frame \
       \animation texture for it" $ \env → do
        let defsRef = ucUnitManagerRef (toUnitCombatCapability env)
        um0 ← readIORef defsRef
        let restore = atomicModifyIORef' defsRef $ \um →
                (um { umDefs = umDefs um0 }, ())
        (`finally` restore) $ do
            logger ← readIORef (loggerRef env)
            defs ← loadUnitYaml logger acolyteYamlPath
            poolRef ← newIORef =≪ defaultAssetPool
            q ← Q.newQueue
            n ← registerUnitDefs env poolRef q resolveUnitAtlases
                    acolyteYamlPath defs
            msgs ← Q.flushQueue q
            n `shouldBe` 1

            um ← readIORef defsRef
            case HM.lookup "acolyte" (umDefs um) of
                Nothing → expectationFailure
                    "the shipped acolyte failed to register"
                Just def → do
                    let anims = HM.toList (udAnimations def)
                        legacy = [ nm | (nm, a) ← anims
                                 , not (storageIsAtlas (aStorage a)) ]
                    length anims `shouldBe` 54
                    legacy `shouldBe` []
                    -- One upload, one handle, one bindless slot per
                    -- animation — and no two animations sharing one.
                    let handles = [ raTexture res
                                  | (_, a) ← anims
                                  , StorageAtlas res ← [aStorage a] ]
                    length (nub handles) `shouldBe` 54

            -- 54 atlases queued, one per animation, each its own path.
            let atlases = atlasRequests msgs
            length atlases `shouldBe` 54
            length (nub (map snd atlases)) `shouldBe` 54
            -- The ONLY ordinary texture loads left are acolyte's
            -- NON-ANIMATION art. The allowance is derived from the YAML
            -- rather than from a path shape, because several of those
            -- fields legitimately point AT an animation frame — reusing
            -- one as a sprite/directional sprite/portrait is explicitly
            -- legal (#1257), and 20 shipped references do it, so
            -- "nothing under animations/" would be the wrong rule and
            -- would fail for a correct tree.
            --
            -- What this does assert is the resident-image reduction the
            -- pilot claims: of 2,250 source frames, the loader opens
            -- only the handful the unit names outside its animations.
            let nonAnimationArt d = uydSprite d
                    : maybe [] pure (uydPortrait d)
                    ⧺ Map.elems (uydDirectionalSprites d)
                allowed = concatMap nonAnimationArt defs
                loaded = map (T.pack ∘ snd) (plainRequests msgs)
            length defs `shouldBe` 1
            loaded `shouldSatisfy` all (`elem` allowed)
            -- and every one of them is actually requested, so the
            -- allowance is a real upper bound rather than a vacuous one.
            nub loaded `shouldMatchList` nub allowed
