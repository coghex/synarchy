-- | Focused coverage for 'Engine.Asset.YamlList.loadYamlList' (#1008),
--   the shared body every `load<Thing>Yaml` loader under `Engine.Asset`
--   is now defined in terms of: decode, warn + [] on failure, debug
--   count + the accessor's list on success. Asserted through a
--   callback logger so the emitted level/category/message text is
--   pinned, not just the returned list.
module Test.Headless.Asset.YamlList (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as T
import System.Directory
    (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), (.:), withObject)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState
    )
import Engine.Asset.YamlList (loadYamlList)

newtype TestYamlFile = TestYamlFile
    { tyfItems ∷ [Text]
    } deriving (Show, Eq, Generic)

instance FromJSON TestYamlFile where
    parseJSON = withObject "TestYamlFile" $ \v → TestYamlFile ⊚ v .: "items"

spec ∷ Spec
spec = describe "loadYamlList" $ do
    it "extracts the accessor's list and logs a CatAsset debug count on a valid file" $
        withTempYaml "valid.yaml" "items:\n  - a\n  - b\n  - c\n" $ \path → do
            (logger, entriesRef) ← callbackLogger
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
            (logger, entriesRef) ← callbackLogger
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

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef', with 'CatAsset' debug logging enabled (off by default) so
--   the success-path 'logDebug' call actually reaches the callback.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :))
        , lcDebugCategories = [CatAsset]
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
