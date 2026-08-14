-- | The asset-only unit declaration form (#1257) and the boundary that
--   keeps it out of the gameplay unit registry.
--
--   Three shipped unit asset trees — @tiller@, @unknown_unit@,
--   @white_tailed_deer@ — are part of the authoritative animation
--   inventory but are deliberately NOT gameplay units. They declare
--   their frames under a top-level @asset_units:@ key rather than
--   @units:@, which is the whole mechanism: 'loadUnitYaml' reads
--   'uyfUnits' and so never returns one, meaning nothing downstream can
--   register it, load a gameplay texture for it, list it, or spawn it.
--
--   The point of this group is that BOTH halves hold at once. Proving
--   only the exclusion would be satisfied by a file that fails to parse
--   — which is exactly the accident the issue's "not by relying on
--   missing gameplay fields or parse failures" clause rules out — so
--   every exclusion assertion here is paired with a positive one
--   showing the same file's animations decoded with real content.
module Test.Headless.Asset.UnitInventory (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (sort)
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing
    , removeDirectoryRecursive, doesFileExist )
import System.FilePath ((</>))
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogEntry(..), LogLevel(..), LoggerState )
import Engine.Asset.YamlUnits
    ( UnitYamlDef(..), UnitYamlAnim(..), UnitYamlAssetDef(..)
    , loadUnitYaml, loadUnitYamlAssets )

-- | Every shipped asset-only tree, with the direction count its
--   declared layout must produce. Five means the canonical authored
--   half (@flip: true@, W/SW/NW mirrored at draw time); eight means a
--   fully authored set (@flip: false@).
assetOnlyUnits ∷ [(Text, Int, Bool, Int)]
assetOnlyUnits =
    --  name                animations  flip   directions
    [ ("tiller",            2,          True,  5)
    , ("unknown_unit",      2,          False, 8)
    , ("white_tailed_deer", 13,         True,  5)
    ]

gameplayUnits ∷ [Text]
gameplayUnits = ["acolyte", "bear_brown", "red_squirrel", "technomule"]

unitYamlPath ∷ Text → FilePath
unitYamlPath name = "data" </> "units" </> T.unpack name <> ".yaml"

spec ∷ Spec
spec = do
    describe "asset-only declarations parse" $ do
        forM_ assetOnlyUnits $ \(name, animCount, flipV, dirCount) →
            it (T.unpack name <> " declares its shipped animations with \
                \real frame content") $ do
                logger ← silentLogger
                defs ← loadUnitYamlAssets logger (unitYamlPath name)
                case defs of
                    [def] → do
                        uyadName def `shouldBe` name
                        Map.size (uyadAnimations def) `shouldBe` animCount
                        let anims = Map.elems (uyadAnimations def)
                        -- Playback metadata is the issue's stated
                        -- contract, and `flip` is what preserves the
                        -- viewer's CURRENT visible mirroring: with a
                        -- YAML entry present, Engine.Preview.Unit's
                        -- effectiveFlip takes this value verbatim
                        -- instead of inferring one.
                        map uyaFps anims `shouldSatisfy` all (≡ 8)
                        map uyaLoop anims `shouldSatisfy` and
                        map uyaFlip anims `shouldSatisfy` all (≡ flipV)
                        -- Every animation declares the expected
                        -- direction set, and no direction is empty.
                        forM_ (Map.toList (uyadAnimations def)) $
                            \(animName, anim) → do
                                let frames = uyaFrames anim
                                (T.unpack animName, Map.size frames)
                                    `shouldBe` (T.unpack animName, dirCount)
                                Map.elems frames
                                    `shouldSatisfy` all (not ∘ null)
                    other → expectationFailure
                        ("expected exactly one asset-only declaration, got "
                         <> show (length other))

        it "every declared frame path exists on disk" $ do
            logger ← silentLogger
            paths ← concat ⊚ forM assetOnlyUnits (\(name, _, _, _) → do
                defs ← loadUnitYamlAssets logger (unitYamlPath name)
                pure [ p
                     | def ← defs
                     , anim ← Map.elems (uyadAnimations def)
                     , ps ← Map.elems (uyaFrames anim)
                     , p ← ps ])
            length paths `shouldBe` 575
            missing ← filterM (fmap not ∘ doesFileExist ∘ T.unpack) paths
            missing `shouldBe` []

    describe "asset-only declarations stay out of the gameplay registry" $ do
        forM_ assetOnlyUnits $ \(name, _, _, _) →
            it (T.unpack name <> " yields no UnitYamlDef, and does so \
                \without a decode failure") $ do
                (logger, entriesRef) ← recordingLogger
                defs ← loadUnitYaml logger (unitYamlPath name)
                -- The exclusion itself.
                map uydName defs `shouldBe` []
                -- ...and the reason for it. A parse failure would ALSO
                -- produce an empty list, so pin that the loader logged
                -- its success path (a debug count) rather than the
                -- warning it emits when a file does not decode.
                entries ← readIORef entriesRef
                map leLevel entries `shouldBe` [LevelDebug]

        it "the gameplay units are unaffected — they still load, and no \
           \asset-only unit joins them" $ do
            logger ← silentLogger
            loaded ← concat ⊚ forM gameplayUnits (\name →
                map uydName ⊚ loadUnitYaml logger (unitYamlPath name))
            sort loaded `shouldBe` sort gameplayUnits
            -- The two lists are disjoint: no name reaches the registry
            -- from an `asset_units:` entry.
            let assetNames = [n | (n, _, _, _) ← assetOnlyUnits]
            filter (`elem` assetNames) loaded `shouldBe` []

        it "a gameplay YAML holds no asset-only declarations" $ do
            logger ← silentLogger
            forM_ gameplayUnits $ \name → do
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

        it "the shipped asset-only declarations still decode, so the key \
           \check above is not merely rejecting everything" $ do
            logger ← silentLogger
            forM_ assetOnlyUnits $ \(name, _, _, _) → do
                defs ← loadUnitYamlAssets logger (unitYamlPath name)
                map uyadName defs `shouldBe` [name]

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
withTempUnitYaml contents act = do
    tmp ← getTemporaryDirectory
    let dir = tmp </> "synarchy-unit-inventory-1257"
    createDirectoryIfMissing True dir
    let path = dir </> "candidate.yaml"
    writeFile path contents
    act path `finally` removeDirectoryRecursive dir
