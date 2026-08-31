{-# LANGUAGE ScopedTypeVariables #-}
-- | Config load/save contract tests (#638): the local-runtime-vs-
--   versioned-default resolution in 'Engine.Core.Init.resolveConfigPath'
--   (config/video.local.yaml + config/keybinds.local.yaml fall back to
--   their tracked @_default.yaml@ template when the gitignored local
--   file is absent), 'Engine.Core.Init.migrateLegacyConfig's pre-#786
--   legacy-config upgrade path INCLUDING #1937's neutral-placeholder
--   suppression, and the notification-overrides
--   materialize-if-absent + round-trip contract in
--   'Engine.Asset.YamlNotifications' (config/notifications.local.yaml
--   has no separate default file — it self-materializes from the registry).
module Test.Headless.Core.ConfigState (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef, modifyIORef')
import System.Directory
  ( getTemporaryDirectory, createDirectoryIfMissing
  , removeDirectoryRecursive, doesDirectoryExist, doesFileExist )
import System.FilePath ((</>))
import System.IO (stderr)
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Engine.Core.Init
  ( resolveConfigPath, migrateLegacyConfig
  , LegacyNeutralityCheck(..) )
import Engine.Core.Log
  ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
  , LoggerState, LogEntry(..) )
import Engine.Asset.YamlNotifications (loadNotificationCfg, writeNotificationOverrides)
import Engine.PlayerEvent (CategoryCfg(..))

-- | A minimal FromJSON type with one REQUIRED field, standing in for a
--   real subsystem's config type (e.g. 'Engine.Graphics.Config.VideoConfigFile'
--   requiring @resolution@) so 'migrateLegacyConfig's schema-aware
--   validation — decoding against the real target type, not just
--   checking for syntactically valid YAML — can be exercised without
--   depending on a concrete engine config type.
newtype ProbeCfg = ProbeCfg { pcRequired ∷ Int } deriving (Show, Eq)

instance FromJSON ProbeCfg where
    parseJSON = withObject "ProbeCfg" $ \v → ProbeCfg ⊚ v .: "required"

probeCfg ∷ Proxy ProbeCfg
probeCfg = Proxy

-- | A logger whose entries are captured in memory, so a spec can assert
--   on the exact line 'migrateLegacyConfig' emitted (#1937 requires the
--   neutral-suppression outcome to be distinguishable from a real
--   migration, which is only observable through the log).
capturingLogger ∷ IO (LoggerState, IO [Text])
capturingLogger = do
    seen ← newIORef ([] ∷ [Text])
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' seen (leMessage e :)) }
    return (logger, reverse ⊚ readIORef seen)

-- | Does any captured line contain this fragment?
logged ∷ [Text] → Text → Bool
logged msgs frag = any (T.isInfixOf frag) msgs

-- | The exact successful-migration line 'migrateLegacyConfig' emits.
migratedLine ∷ FilePath → FilePath → Text
migratedLine legacy local =
    "Migrated legacy config " <> T.pack legacy <> " -> " <> T.pack local

-- | The #1937 reference points for a subsystem that HAS a tracked
--   @_default.yaml@ template to be neutral against.
neutralityCheck ∷ FilePath → FilePath → LegacyNeutralityCheck
neutralityCheck deflt record = LegacyNeutralityCheck
    { lncDefaultPath = deflt
    , lncRecordPath  = record
    }

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

    describe "Engine.Core.Init.migrateLegacyConfig legacy config upgrade \
             \(#786), no neutrality reference (notifications' path)" $ do
        it "copies a valid legacy file to the local path when local is absent" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                writeFile legacy "required: 5\n"
                migrateLegacyConfig probeCfg logger Nothing legacy local
                existsAfter ← doesFileExist local
                existsAfter `shouldBe` True
                migrated ← readFile local
                migrated `shouldBe` "required: 5\n"

        it "is a no-op when there is no legacy file to migrate" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                migrateLegacyConfig probeCfg logger Nothing legacy local
                doesFileExist local `shouldReturn` False

        it "never overwrites an existing local file, even with a legacy file present" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                writeFile legacy "required: 5\n"
                writeFile local "required: 9\n"
                migrateLegacyConfig probeCfg logger Nothing legacy local
                kept ← readFile local
                kept `shouldBe` "required: 9\n"

        it "is idempotent: a second call after a successful migration changes nothing" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                writeFile legacy "required: 5\n"
                migrateLegacyConfig probeCfg logger Nothing legacy local
                writeFile local "required: 90\n" -- simulate a later player Save
                migrateLegacyConfig probeCfg logger Nothing legacy local
                kept ← readFile local
                kept `shouldBe` "required: 90\n"

        it "leaves a syntactically malformed legacy file unmigrated (falls \
           \back safely, no local file appears)" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                writeFile legacy "required: [this, is: not, valid: {yaml"
                migrateLegacyConfig probeCfg logger Nothing legacy local
                doesFileExist local `shouldReturn` False

        it "leaves a structurally valid but schema-incomplete legacy file \
           \unmigrated, rather than copying it and masking the load failure" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                -- Valid YAML, but missing the "required" field ProbeCfg's
                -- FromJSON instance demands — the exact gap a generic
                -- "is this valid YAML" check would miss.
                writeFile legacy "unrelated_field: 1\n"
                migrateLegacyConfig probeCfg logger Nothing legacy local
                doesFileExist local `shouldReturn` False

        it "does not destroy a valid newer local file next to a \
           \schema-incomplete legacy file" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                writeFile legacy "unrelated_field: 1\n"
                writeFile local "required: 42\n"
                migrateLegacyConfig probeCfg logger Nothing legacy local
                kept ← readFile local
                kept `shouldBe` "required: 42\n"

    describe "Engine.Core.Init.migrateLegacyConfig neutral legacy config \
             \suppression (#1937)" $ do
        it "leaves the local file absent when the legacy file only \
           \restates the versioned default" $
            withTempDir $ \dir → do
                (logger, dumpLog) ← capturingLogger
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                writeFile deflt  "required: 5\n"
                writeFile legacy "required: 5\n"
                migrateLegacyConfig probeCfg logger
                    (Just (neutralityCheck deflt record)) legacy local
                doesFileExist local `shouldReturn` False
                msgs ← dumpLog
                logged msgs "carries no player state" `shouldBe` True
                logged msgs (migratedLine legacy local) `shouldBe` False

        it "judges neutrality by the subsystem's DECODE, not by bytes: a \
           \semantically equal but differently spelled legacy file is \
           \still a placeholder" $
            withTempDir $ \dir → do
                (logger, dumpLog) ← capturingLogger
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                -- Same decoded ProbeCfg, deliberately different bytes:
                -- a comment, flow style, and an extra field the decoder
                -- ignores. `cmp` would call these two files different.
                writeFile deflt  "required: 5\n"
                writeFile legacy "# pre-#786 placeholder\n\
                                 \{required: 5, ignored_by_decode: 1}\n"
                migrateLegacyConfig probeCfg logger
                    (Just (neutralityCheck deflt record)) legacy local
                doesFileExist local `shouldReturn` False
                msgs ← dumpLog
                logged msgs "carries no player state" `shouldBe` True

        it "still migrates a legacy file whose decoded content genuinely \
           \differs from the versioned default, with the unchanged log line" $
            withTempDir $ \dir → do
                (logger, dumpLog) ← capturingLogger
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                writeFile deflt  "required: 5\n"
                writeFile legacy "required: 42\n"
                migrateLegacyConfig probeCfg logger
                    (Just (neutralityCheck deflt record)) legacy local
                doesFileExist local `shouldReturn` True
                readFile local `shouldReturn` "required: 42\n"
                msgs ← dumpLog
                logged msgs (migratedLine legacy local) `shouldBe` True
                logged msgs "carries no player state" `shouldBe` False
                doesFileExist record `shouldReturn` False

        it "keeps the first boot's neutral determination across a later \
           \revision of the versioned default (the untouched placeholder \
           \is never promoted, so the revised default reaches a player \
           \who never saved)" $
            withTempDir $ \dir → do
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                    check  = Just (neutralityCheck deflt record)
                writeFile deflt  "required: 5\n"
                writeFile legacy "required: 5\n"
                (logger0, _) ← capturingLogger
                migrateLegacyConfig probeCfg logger0 check legacy local
                doesFileExist local  `shouldReturn` False
                doesFileExist record `shouldReturn` True
                -- Second boot: only the TEMPLATE changed. The legacy
                -- file is byte-for-byte what it was, so a stateless
                -- "does it equal the current default" test would now
                -- call it player state and promote it.
                writeFile deflt "required: 7\n"
                (logger1, dumpLog1) ← capturingLogger
                migrateLegacyConfig probeCfg logger1 check legacy local
                doesFileExist local `shouldReturn` False
                msgs ← dumpLog1
                logged msgs (migratedLine legacy local) `shouldBe` False
                -- resolveConfigPath therefore keeps resolving the
                -- revised template, which is requirement 5 end to end.
                resolveConfigPath local deflt `shouldReturn` deflt

        it "re-examines a legacy file the player has since edited, even \
           \though a neutral record exists for its old content" $
            withTempDir $ \dir → do
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                    check  = Just (neutralityCheck deflt record)
                writeFile deflt  "required: 5\n"
                writeFile legacy "required: 5\n"
                (logger0, _) ← capturingLogger
                migrateLegacyConfig probeCfg logger0 check legacy local
                doesFileExist local `shouldReturn` False
                writeFile legacy "required: 99\n"
                (logger1, _) ← capturingLogger
                migrateLegacyConfig probeCfg logger1 check legacy local
                readFile local `shouldReturn` "required: 99\n"

        it "never touches an existing local file, and records nothing, \
           \even next to a neutral legacy file" $
            withTempDir $ \dir → do
                (logger, _) ← capturingLogger
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                writeFile deflt  "required: 5\n"
                writeFile legacy "required: 5\n"
                writeFile local  "required: 9\n"
                migrateLegacyConfig probeCfg logger
                    (Just (neutralityCheck deflt record)) legacy local
                readFile local `shouldReturn` "required: 9\n"
                doesFileExist record `shouldReturn` False

        it "leaves a malformed legacy file unmigrated and unrecorded, \
           \warning exactly as before" $
            withTempDir $ \dir → do
                (logger, dumpLog) ← capturingLogger
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                writeFile deflt  "required: 5\n"
                writeFile legacy "required: [this, is: not, valid: {yaml"
                migrateLegacyConfig probeCfg logger
                    (Just (neutralityCheck deflt record)) legacy local
                doesFileExist local  `shouldReturn` False
                doesFileExist record `shouldReturn` False
                msgs ← dumpLog
                logged msgs "could not be migrated" `shouldBe` True

        it "migrates when the versioned default itself is missing or \
           \undecodable — neutrality is never assumed, only proven" $
            withTempDir $ \dir → do
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                    record = dir </> "legacy-neutral.local.yaml"
                (logger, _) ← capturingLogger
                writeFile legacy "required: 5\n"   -- no default file at all
                migrateLegacyConfig probeCfg logger
                    (Just (neutralityCheck deflt record)) legacy local
                readFile local `shouldReturn` "required: 5\n"

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
