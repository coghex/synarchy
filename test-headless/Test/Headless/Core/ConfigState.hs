{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables #-}
-- | Config load/save contract tests (#638): the local-runtime-vs-
--   versioned-default resolution in 'Engine.Core.Init.resolveConfigPath'
--   (config/video.yaml + config/keybinds.yaml fall back to their tracked
--   @_default.yaml@ template when the gitignored local file is absent),
--   and the notification-overrides materialize-if-absent + round-trip
--   contract in 'Engine.Asset.YamlNotifications' (config/notifications.yaml
--   has no separate default file — it self-materializes from the registry).
module Test.Headless.Core.ConfigState (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import System.Directory
  ( getTemporaryDirectory, createDirectoryIfMissing
  , removeDirectoryRecursive, doesDirectoryExist, doesFileExist )
import System.FilePath ((</>))
import System.IO (stderr)
import qualified Data.HashMap.Strict as HM
import Engine.Core.Init (resolveConfigPath)
import Engine.Core.Log (initLogger, defaultLogConfig, LogConfig(..), LogBackend(..))
import Engine.Asset.YamlNotifications (loadNotificationCfg, writeNotificationOverrides)
import Engine.PlayerEvent (CategoryCfg(..))

-- | A scratch directory, wiped clean before and after use. The suite runs
--   sequentially, so a single fixed path (under the system temp dir, never
--   inside the repo) is safe to reuse across 'it's.
withTempDir ∷ (FilePath → IO a) → IO a
withTempDir action = do
    tmp ← getTemporaryDirectory
    let dir = tmp </> "synarchy-config-state-spec"
    reset dir
    action dir `finally` reset dir
  where
    reset d = do
        exists ← doesDirectoryExist d
        when exists (removeDirectoryRecursive d)
        createDirectoryIfMissing True d

registryYaml ∷ String
registryYaml = unlines
    [ "categories:"
    , "  - id: debug"
    , "    display_name: Debug"
    , "    default_settings: {log: false, popup: false, pause: false}"
    ]

spec ∷ Spec
spec = do
    describe "Engine.Core.Init config path resolution (#638)" $ do
        it "prefers the local runtime file when it exists" $ withTempDir $ \dir → do
            let local = dir </> "local.yaml"
                deflt = dir </> "default.yaml"
            writeFile local "local"
            writeFile deflt "default"
            resolveConfigPath local deflt `shouldReturn` local

        it "falls back to the versioned default template when the local \
           \file is absent (fresh-clone boot)" $
            withTempDir $ \dir → do
                let local = dir </> "local.yaml"
                    deflt = dir </> "default.yaml"
                writeFile deflt "default"
                resolveConfigPath local deflt `shouldReturn` deflt

    describe "Engine.Asset.YamlNotifications config load/save contract (#638)" $ do
        it "materializes the overrides file from registry defaults when absent" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let registryPath  = dir </> "registry.yaml"
                    overridesPath = dir </> "notifications.yaml"
                writeFile registryPath registryYaml
                existsBefore ← doesFileExist overridesPath
                existsBefore `shouldBe` False
                (cfg, order) ← loadNotificationCfg logger registryPath overridesPath
                existsAfter ← doesFileExist overridesPath
                existsAfter `shouldBe` True
                order `shouldBe` ["debug"]
                case HM.lookup "debug" cfg of
                    Nothing → expectationFailure "debug category missing from resolved config"
                    Just c  → ccLog c `shouldBe` False

        it "round-trips a saved override back through load" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let registryPath  = dir </> "registry.yaml"
                    overridesPath = dir </> "notifications.yaml"
                writeFile registryPath registryYaml
                (cfg0, _) ← loadNotificationCfg logger registryPath overridesPath
                let updated = HM.adjust (\c → c { ccLog = True }) "debug" cfg0
                writeNotificationOverrides overridesPath updated
                (cfg1, _) ← loadNotificationCfg logger registryPath overridesPath
                case HM.lookup "debug" cfg1 of
                    Nothing → expectationFailure "debug category missing from resolved config"
                    Just c  → ccLog c `shouldBe` True
