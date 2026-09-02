-- | Focused coverage for 'Engine.Asset.YamlList.loadYamlList' (#1008),
--   the shared body every `load<Thing>Yaml` loader under `Engine.Asset`
--   is now defined in terms of: decode, warn + [] on failure, debug
--   count + the accessor's list on success. Asserted through a
--   callback logger so the emitted level/category/message text is
--   pinned, not just the returned list.
--
--   The attribution cases below pin the OTHER half of that extraction's
--   contract (#2167): the shared body holds the family's only logging
--   calls, so it must carry 'HasCallStack' for the logger's
--   outermost-frame rule ('Engine.Core.Log.extractCallSite', #945) to
--   keep reporting the owning domain loader rather than this helper.
--   Dropping the constraint — or reintroducing a constraint-free
--   wrapper between a loader and the helper — collapses every loader's
--   warnings and debug counts onto one module, which is exactly the
--   regression these cases exist to catch.
module Test.Headless.Asset.YamlList (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory
    (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import GHC.Generics (Generic)
-- 'HasCallStack' is deliberately NOT listed here: it reaches this
-- module through UPrelude's @module Prelude@ re-export (base-4.21
-- exports it from Prelude), so naming it as well is a redundant
-- import and @-Werror=unused-imports@ makes that a build failure.
-- 'Test.Headless.Core.LogMonad' imports it the same way.
import GHC.Stack (SrcLoc(..))
import Data.Aeson (FromJSON(..), (.:), withObject)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState
    )
import Engine.Asset.YamlList (loadYamlList)
import Engine.Asset.YamlItems (loadItemYaml)

newtype TestYamlFile = TestYamlFile
    { tyfItems ∷ [Text]
    } deriving (Show, Eq, Generic)

instance FromJSON TestYamlFile where
    parseJSON = withObject "TestYamlFile" $ \v → TestYamlFile ⊚ v .: "items"

-- | The smallest item definition 'loadItemYaml' accepts: a name, a
--   sprite, and a strictly positive bulk.
validItemYaml ∷ String
validItemYaml = unlines
    [ "items:"
    , "  - name: \"yamllist_spec_widget\""
    , "    sprite: \"assets/textures/items/tool/axe_steel.png\""
    , "    bulk: 1.0"
    ]

spec ∷ Spec
spec = describe "loadYamlList" $ do
    it "extracts the accessor's list and logs a CatAsset debug count on a valid file" $
        withTempYaml "valid.yaml" "items:\n  - a\n  - b\n  - c\n" $ \path → do
            (logger, entriesRef) ← callbackLogger True
            xs ← loadYamlList logger "widget" "widgets" tyfItems path
            xs `shouldBe` ["a", "b", "c"]
            entries ← readIORef entriesRef
            case entries of
                [entry] → do
                    leLevel entry `shouldBe` LevelDebug
                    leCategory entry `shouldBe` CatAsset
                    leMessage entry `shouldBe`
                        "Loaded 3 widgets from " <> T.pack path
                other → expectationFailure $
                    "expected exactly one captured log entry, got " ⧺ show (length other)

    it "returns [] and logs a CatAsset warning on a parse failure" $
        withTempYaml "invalid.yaml" "items: [unterminated\n" $ \path → do
            (logger, entriesRef) ← callbackLogger True
            xs ← loadYamlList logger "widget" "widgets" tyfItems path
            xs `shouldBe` ([] ∷ [Text])
            entries ← readIORef entriesRef
            case entries of
                [entry] → do
                    leLevel entry `shouldBe` LevelWarn
                    leCategory entry `shouldBe` CatAsset
                    leMessage entry `shouldSatisfy` T.isPrefixOf
                        ("Failed to parse widget YAML " <> T.pack path <> ": ")
                other → expectationFailure $
                    "expected exactly one captured log entry, got " ⧺ show (length other)

    it "attributes a domain loader's success debug count to that loader's own module, not the shared helper (#2167)" $
        withTempYaml "attribution-valid.yaml" validItemYaml $ \path → do
            (logger, entriesRef) ← callbackLogger True
            xs ← loadItemYaml logger path
            length xs `shouldBe` 1
            entry ← expectSingleEntry entriesRef
            leLevel entry `shouldBe` LevelDebug
            expectOwnedBy "Engine.Asset.YamlItems" "Engine/Asset/YamlItems.hs" entry

    it "attributes a domain loader's parse-failure warning to that loader's own module, not the shared helper (#2167)" $
        withTempYaml "attribution-invalid.yaml" "items: [unterminated\n" $ \path → do
            (logger, entriesRef) ← callbackLogger True
            xs ← loadItemYaml logger path
            length xs `shouldBe` 0
            entry ← expectSingleEntry entriesRef
            leLevel entry `shouldBe` LevelWarn
            expectOwnedBy "Engine.Asset.YamlItems" "Engine/Asset/YamlItems.hs" entry

    it "captures no source location for a domain loader when lcShowLocation is disabled" $ do
        withTempYaml "no-location-valid.yaml" validItemYaml $ \path → do
            (logger, entriesRef) ← callbackLoggerWith True False
            _ ← loadItemYaml logger path
            successEntry ← expectSingleEntry entriesRef
            leLevel successEntry `shouldBe` LevelDebug
            leSrcLoc successEntry `shouldSatisfy` isNothing
        withTempYaml "no-location-invalid.yaml" "items: [unterminated\n" $ \path → do
            (logger, entriesRef) ← callbackLoggerWith True False
            _ ← loadItemYaml logger path
            failureEntry ← expectSingleEntry entriesRef
            leLevel failureEntry `shouldBe` LevelWarn
            leSrcLoc failureEntry `shouldSatisfy` isNothing

    it "attributes a direct loadYamlList call to its own call site, never to the helper's body" $
        withTempYaml "direct.yaml" "items:\n  - a\n" $ \path → do
            (logger, entriesRef) ← callbackLogger True
            _ ← loadYamlList logger "widget" "widgets" tyfItems path
            entry ← expectSingleEntry entriesRef
            expectOwnedBy "Test.Headless.Asset.YamlList"
                "Test/Headless/Asset/YamlList.hs" entry

-- | The captured entry names @modulePath@ as the place its logging call
--   was written, and does NOT name the shared helper.
expectOwnedBy ∷ HasCallStack ⇒ String → String → LogEntry → Expectation
expectOwnedBy expectedModule expectedFile entry = case leSrcLoc entry of
    Nothing  → expectationFailure "expected the entry to capture a source location"
    Just loc → do
        srcLocModule loc `shouldBe` expectedModule
        srcLocFile loc `shouldSatisfy` (expectedFile `isInfixOf`)
        srcLocFile loc `shouldNotSatisfy` ("Engine/Asset/YamlList.hs" `isInfixOf`)

expectSingleEntry ∷ HasCallStack ⇒ IORef [LogEntry] → IO LogEntry
expectSingleEntry entriesRef = do
    entries ← readIORef entriesRef
    case entries of
        [entry] → pure entry
        other   → do
            expectationFailure $
                "expected exactly one captured log entry, got " ⧺ show (length other)
            error "unreachable: expectationFailure throws"

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef', with 'CatAsset' debug logging enabled (off by default) so
--   the success-path 'logDebug' call actually reaches the callback.
callbackLogger ∷ Bool → IO (LoggerState, IORef [LogEntry])
callbackLogger assetDebug = callbackLoggerWith assetDebug True

-- | As 'callbackLogger', with explicit control over 'lcShowLocation'.
callbackLoggerWith ∷ Bool → Bool → IO (LoggerState, IORef [LogEntry])
callbackLoggerWith assetDebug showLocation = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :))
        , lcDebugCategories = if assetDebug then [CatAsset] else []
        , lcShowLocation = showLocation
        }
    pure (logger, entriesRef)

withTempYaml ∷ FilePath → String → (FilePath → IO a) → IO a
withTempYaml name contents action = do
    tmp ← getTemporaryDirectory
    let dir = tmp </> "synarchy-yamllist-spec"
        path = dir </> name
    createDirectoryIfMissing True dir
    writeFile path contents
    action path `finally` removeDirectoryRecursive dir
