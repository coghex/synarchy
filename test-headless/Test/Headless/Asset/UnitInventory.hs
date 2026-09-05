-- | The unit declaration forms (#1257) and what each one reaches.
--
--   #1257 gave three shipped asset trees — @tiller@, @unknown_unit@,
--   @white_tailed_deer@ — inventory-only declarations under a top-level
--   @asset_units:@ key, deliberately outside the gameplay registry.
--   #1261 (TEX-6) PROMOTED all three to ordinary @units:@ entries: with
--   per-frame unit-animation loading retired, a tree renders only
--   through the compiled atlases its declaration drives, and the owner
--   decision of 2026-08-11 kept all three as preview targets. They are
--   now registered, minimal runtime definitions — a name, one direct
--   sprite, and the animation inventory they already carried.
--
--   Both halves of that still have to hold at once, so every membership
--   assertion here is paired with a positive one showing the same
--   file's animations decoded with real content: a file that failed to
--   parse would satisfy a bare exclusion check by accident.
--
--   The @asset_units:@ FORM remains supported for shipped art that is not
--   yet a gameplay unit, and its decoder is also exercised against temp
--   fixtures below.
module Test.Headless.Asset.UnitInventory (spec) where

import UPrelude
import Test.Hspec
import Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (sort)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogEntry(..), LogLevel(..), LoggerState )
import Engine.Asset.YamlUnits
    ( UnitYamlDef(..), UnitYamlAnim(..), UnitYamlAssetDef(..)
    , loadUnitYaml, loadUnitYamlAssets )
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- | The three trees #1261 promoted, with the direction count each
--   declared layout must produce. Five means the canonical authored
--   half (@flip: true@, W/SW/NW mirrored at draw time); eight means a
--   fully authored set (@flip: false@).
--
--   @flip@ is load-bearing and NOT the decoder default: with a YAML
--   entry present the preview takes this value verbatim, so transcribing
--   the decoder's @flip = False@ onto a canonical-five tree would
--   silently drop its mirrored W/SW/NW cells.
promotedUnits ∷ [(Text, Int, Bool, Int)]
promotedUnits =
    --  name                animations  flip   directions
    [ ("tiller",            2,          True,  5)
    , ("unknown_unit",      2,          False, 8)
    , ("white_tailed_deer", 13,         True,  5)
    ]

gameplayUnits ∷ [Text]
gameplayUnits =
    [ "acolyte", "bear_brown", "nomad_primitive"
    , "red_squirrel", "technomule" ]

allGameplayUnits ∷ [Text]
allGameplayUnits = gameplayUnits ⧺ [n | (n, _, _, _) ← promotedUnits]

unitYamlPath ∷ Text → FilePath
unitYamlPath name = "data" </> "units" </> T.unpack name <> ".yaml"

spec ∷ Spec
spec = do
    describe "the promoted declarations parse" $ do
        forM_ promotedUnits $ \(name, animCount, flipV, dirCount) →
            it (T.unpack name <> " declares its shipped animations with \
                \real frame content") $ do
                logger ← silentLogger
                defs ← loadUnitYaml logger (unitYamlPath name)
                case defs of
                    [def] → do
                        uydName def `shouldBe` name
                        Map.size (uydAnimations def) `shouldBe` animCount
                        let anims = Map.elems (uydAnimations def)
                        map uyaFps anims `shouldSatisfy` all (≡ 8)
                        map uyaLoop anims `shouldSatisfy` and
                        map uyaFlip anims `shouldSatisfy` all (≡ flipV)
                        -- Every animation declares the expected
                        -- direction set, and no direction is empty.
                        forM_ (Map.toList (uydAnimations def)) $
                            \(animName, anim) → do
                                let frames = uyaFrames anim
                                (T.unpack animName, Map.size frames)
                                    `shouldBe` (T.unpack animName, dirCount)
                                Map.elems frames
                                    `shouldSatisfy` all (not ∘ null)
                    other → expectationFailure
                        ("expected exactly one declaration, got "
                         <> show (length other))

        it "every declared frame path exists on disk" $ do
            logger ← silentLogger
            paths ← concat ⊚ forM promotedUnits (\(name, _, _, _) → do
                defs ← loadUnitYaml logger (unitYamlPath name)
                pure [ p
                     | def ← defs
                     , anim ← Map.elems (uydAnimations def)
                     , ps ← Map.elems (uyaFrames anim)
                     , p ← ps ])
            length paths `shouldBe` 575
            missing ← filterM (fmap not ∘ doesFileExist ∘ T.unpack) paths
            missing `shouldBe` []

    describe "the promoted declarations reach the registry, minimally" $ do
        forM_ promotedUnits $ \(name, _, _, _) →
            it (T.unpack name <> " is returned by loadUnitYaml with a \
                \direct sprite and no invented gameplay design") $ do
                (logger, entriesRef) ← recordingLogger
                defs ← loadUnitYaml logger (unitYamlPath name)
                map uydName defs `shouldBe` [name]
                -- A decode failure would ALSO be quiet, so pin that the
                -- loader logged its success path (a debug count) rather
                -- than the warning it emits when a file does not decode.
                entries ← readIORef entriesRef
                map leLevel entries `shouldBe` [LevelDebug]
                case defs of
                    [def] → do
                        -- The one required direct texture, which may
                        -- legitimately be an animation source frame
                        -- (#1257: reuse is not a duplicate claim).
                        uydSprite def `shouldSatisfy` not ∘ T.null
                        exists ← doesFileExist (T.unpack (uydSprite def))
                        exists `shouldBe` True
                        -- "Minimal" is the contract requirement 3 sets:
                        -- no state mappings, roles, body or combat
                        -- design were invented for these trees.
                        uydStateAnimations def `shouldBe` Map.empty
                        uydNamePool def `shouldBe` Nothing
                        uydBodyParts def `shouldBe` []
                        uydNaturalWeapon def `shouldSatisfy` isNothing
                        uydStartingInventory def `shouldBe` []
                        uydStats def `shouldBe` Map.empty
                        uydSkills def `shouldBe` Map.empty
                    _ → pure ()

        it "registers exactly the shipped gameplay declarations" $ do
            logger ← silentLogger
            loaded ← concat ⊚ forM allGameplayUnits (\name →
                map uydName ⊚ loadUnitYaml logger (unitYamlPath name))
            sort loaded `shouldBe` sort allGameplayUnits

        it "keeps every shipped gameplay declaration out of the \
           \asset-only registry" $ do
            logger ← silentLogger
            forM_ allGameplayUnits $ \name → do
                assets ← loadUnitYamlAssets logger (unitYamlPath name)
                map uyadName assets `shouldBe` []

    describe "the declaration form itself" $ do
        it "refuses a file that declares neither key, rather than \
           \silently decoding it as zero units" $
            withTempUnitYaml "unit:\n  - name: typo\n" $ \path → do
                (logger, entriesRef) ← recordingLogger
                defs ← loadUnitYaml logger path
                defs `shouldBe` []
                entries ← readIORef entriesRef
                map leLevel entries `shouldBe` [LevelWarn]

        it "refuses a file whose only key is an explicit null — aeson's \
           \.:? reads that as absent, so it declares neither form" $
            withTempUnitYaml "units: null\n" $ \path → do
                (logger, entriesRef) ← recordingLogger
                defs ← loadUnitYaml logger path
                defs `shouldBe` []
                entries ← readIORef entriesRef
                map leLevel entries `shouldBe` [LevelWarn]

        it "accepts a file carrying BOTH keys, routing each entry to \
           \its own side" $
            withTempUnitYaml
                (T.unpack (T.unlines
                    [ "units:"
                    , "  - name: real_unit"
                    , "    sprite: \"assets/textures/units/acolyte/portrait.png\""
                    , "asset_units:"
                    , "  - name: props_only"
                    , "    animations:"
                    , "      idle:"
                    , "        frames:"
                    , "          south:"
                    , "            - \"a/b/c.png\""
                    ])) $ \path → do
                logger ← silentLogger
                defs   ← loadUnitYaml logger path
                assets ← loadUnitYamlAssets logger path
                map uydName defs    `shouldBe` ["real_unit"]
                map uyadName assets `shouldBe` ["props_only"]

        it "an asset-only entry carrying a gameplay field is a decode \
           \FAILURE, not a silently ignored key" $
            -- Aeson ignores keys a parser does not ask for, so without
            -- an explicit key check this decodes cleanly and is then
            -- skipped by loadUnitYaml — indistinguishable from a unit
            -- that simply failed to register. #1257 requires it to be
            -- an error.
            withTempUnitYaml
                (T.unpack (T.unlines
                    [ "asset_units:"
                    , "  - name: props_only"
                    , "    sprite: \"assets/textures/units/acolyte/portrait.png\""
                    , "    animations:"
                    , "      idle:"
                    , "        frames:"
                    , "          south:"
                    , "            - \"a/b/c.png\""
                    ])) $ \path → do
                logger ← silentLogger
                assets ← loadUnitYamlAssets logger path
                map uyadName assets `shouldBe` []

        it "an asset-only entry carrying an unknown field fails too — the \
           \rule is a whitelist, not a gameplay blacklist" $
            withTempUnitYaml
                (T.unpack (T.unlines
                    [ "asset_units:"
                    , "  - name: props_only"
                    , "    typo: true"
                    , "    animations:"
                    , "      idle:"
                    , "        frames:"
                    , "          south:"
                    , "            - \"a/b/c.png\""
                    ])) $ \path → do
                logger ← silentLogger
                assets ← loadUnitYamlAssets logger path
                map uyadName assets `shouldBe` []

        it "a well-formed asset-only declaration still decodes, so the \
           \key checks above are not merely rejecting everything — the \
           \form remains supported, it is just that #1261 promoted the \
           \three files that used it" $
            withTempUnitYaml
                (T.unpack (T.unlines
                    [ "asset_units:"
                    , "  - name: props_only"
                    , "    animations:"
                    , "      idle:"
                    , "        fps: 8"
                    , "        loop: true"
                    , "        flip: true"
                    , "        frames:"
                    , "          south:"
                    , "            - \"a/b/c.png\""
                    ])) $ \path → do
                logger ← silentLogger
                defs ← loadUnitYamlAssets logger path
                map uyadName defs `shouldBe` ["props_only"]
                map (Map.keys ∘ uyadAnimations) defs `shouldBe` [["idle"]]

        it "an asset-only entry with no animations: block is a decode \
           \failure, not an empty unit" $
            withTempUnitYaml "asset_units:\n  - name: hollow\n" $ \path → do
                logger ← silentLogger
                assets ← loadUnitYamlAssets logger path
                map uyadName assets `shouldBe` []

-- * Helpers

silentLogger ∷ IO LoggerState
silentLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

-- | Captures every emitted entry, with 'CatAsset' debug logging enabled
--   (off by default) so 'loadYamlList''s SUCCESS-path 'logDebug' call
--   actually reaches the callback — without it the success and failure
--   paths would be indistinguishable here, both recording nothing.
recordingLogger ∷ IO (LoggerState, IORef [LogEntry])
recordingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\entry → modifyIORef' ref (⧺ [entry]))
        , lcDebugCategories = [CatAsset]
        }
    pure (logger, ref)

withTempUnitYaml ∷ String → (FilePath → IO α) → IO α
withTempUnitYaml contents act =
    withExclusiveTempDirectory "synarchy-unit-inventory-1257" $ \dir → do
        let path = dir </> "candidate.yaml"
        writeFile path contents
        act path
