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
--   has no separate default file — it self-materializes from the registry),
--   plus #1938's per-FIELD override resolution: a checkbox the overrides
--   file omits inherits its category's registry default instead of
--   reading as an explicit @false@.
module Test.Headless.Core.ConfigState (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, readIORef, modifyIORef')
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (stderr)
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Proxy (Proxy(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Engine.Core.ConfigWrite (copyConfigFile)
import Engine.Core.Init
  ( resolveConfigPath, migrateLegacyConfig
  , LegacyNeutralityCheck(..) )
import Engine.Core.Log
  ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
  , LoggerState, LogEntry(..), LogLevel(..) )
import Engine.Asset.YamlNotifications
  ( loadNotificationCfg, writeNotificationOverrides, OverridesFile )
import Engine.PlayerEvent (CategoryCfg(..))
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

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
    (logger, dumpLog) ← capturingLoggerLeveled
    return (logger, map snd ⊚ dumpLog)

-- | As 'capturingLogger', but keeping each entry's LEVEL alongside its
--   text. #2210's split is only meaningful if the destination-failure
--   line is a WARNING rather than one of the two info lines, so the
--   examples that pin it need more than the message.
capturingLoggerLeveled ∷ IO (LoggerState, IO [(LogLevel, Text)])
capturingLoggerLeveled = do
    seen ← newIORef ([] ∷ [(LogLevel, Text)])
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → modifyIORef' seen ((leLevel e, leMessage e) :)) }
    return (logger, reverse ⊚ readIORef seen)

-- | Does any captured line contain this fragment?
logged ∷ [Text] → Text → Bool
logged msgs frag = any (T.isInfixOf frag) msgs

-- | The exact successful-migration line 'migrateLegacyConfig' emits.
migratedLine ∷ FilePath → FilePath → Text
migratedLine legacy local =
    "Migrated legacy config " <> T.pack legacy <> " -> " <> T.pack local

-- | The SOURCE-SIDE classification 'migrateLegacyConfig' keeps for a
--   legacy file that fails to decode. #2210 splits the destination
--   write out of that diagnosis, so this exact text is what must
--   SURVIVE the split — requirement 2 — and what the destination
--   warning must not borrow.
sourceFailureLine ∷ Text
sourceFailureLine =
    "could not be migrated (malformed, partial, schema-incomplete, or \
    \unreadable); falling back to the versioned default"

-- | The four faults 'sourceFailureLine' names. Every one is a property
--   of reading or decoding the LEGACY file, so a destination write
--   failure may not mention any of them (#2210 requirement 1).
sourceSideFaults ∷ [Text]
sourceSideFaults = ["malformed", "partial", "schema-incomplete", "unreadable"]

-- | Every captured warning that 'migrateLegacyConfig' emitted about a
--   legacy config, in order.
legacyWarnings ∷ [(LogLevel, Text)] → [Text]
legacyWarnings entries =
    [m | (LevelWarn, m) ← entries, "Legacy config " `T.isInfixOf` m]

-- | The cause 'copyConfigFile' itself reports for a fixture, which is
--   verbatim what 'migrateLegacyConfig' renders. DERIVED by attempting
--   the same copy rather than hard-coded, so the assertion pins "the
--   warning carries the real cause" without pinning one @directory@
--   release's wording. Fails loudly if the fixture is not actually a
--   destination failure.
--
--   #2202 moved the copy from a raw 'copyFile' to the durable
--   config-write helper, so the cause is now that helper's own 'Left'
--   rather than a rendered exception; deriving it through the SAME
--   function production calls is what keeps this honest across that
--   kind of change.
copyFailureText ∷ FilePath → FilePath → IO Text
copyFailureText legacy local = do
    attempt ← copyConfigFile legacy local
    case attempt of
        Right () → "" <$ expectationFailure
            "fixture is not a destination failure: the copy succeeded"
        Left err → return err

-- | #2210 requirement 1: the destination failure is a WARNING, it names
--   the local path it could not write and carries the real exception,
--   and it accuses the legacy file of none of the source-side faults.
shouldBeWriteFailureWarning ∷ [(LogLevel, Text)] → FilePath → Text
                            → Expectation
shouldBeWriteFailureWarning entries local expected =
    case legacyWarnings entries of
        [msg] → do
            (T.pack local `T.isInfixOf` msg) `shouldBe` True
            (expected `T.isInfixOf` msg) `shouldBe` True
            mapM_ (\fault → (fault, fault `T.isInfixOf` msg)
                              `shouldBe` (fault, False))
                  sourceSideFaults
        other → expectationFailure $
            "expected exactly one legacy-config warning, got: " <> show other

-- | #2210 requirement 3: a failed migration leaves the world exactly as
--   a missing legacy file would — the source byte-for-byte untouched, no
--   local file, and the boot still resolving the versioned default.
shouldHaveLeftNoTrace ∷ FilePath → BS.ByteString → FilePath → FilePath
                      → Expectation
shouldHaveLeftNoTrace legacy legacyBytes local deflt = do
    BS.readFile legacy `shouldReturn` legacyBytes
    doesFileExist local `shouldReturn` False
    resolveConfigPath local deflt `shouldReturn` deflt

-- | The #1937 reference points for a subsystem that HAS a tracked
--   @_default.yaml@ template to be neutral against.
neutralityCheck ∷ FilePath → FilePath → LegacyNeutralityCheck
neutralityCheck deflt record = LegacyNeutralityCheck
    { lncDefaultPath = deflt
    , lncRecordPath  = record
    }

-- | A scratch directory this invocation owns outright (#2163): created
--   fresh under a random name, never inside the repo, and torn down
--   afterwards. Nothing is wiped on the way IN, because there is nothing
--   at the path to wipe — which is what lets two suite processes run this
--   group at once without one clearing the other's fixture.
withTempDir ∷ (FilePath → IO a) → IO a
withTempDir = withExclusiveTempDirectory "synarchy-config-state-spec"

registryYaml ∷ String
registryYaml = unlines
    [ "categories:"
    , "  - id: debug"
    , "    display_name: Debug"
    , "    default_settings: {log: false, popup: false, pause: false}"
    ]

-- | A registry whose defaults are NOT uniformly false, which is what
--   makes inheritance distinguishable from the pre-#1938 false-filling:
--
--     * @survival_critical@ mirrors the shipped entry — all three
--       checkboxes default TRUE, so an omitted override field that
--       resolved to 'False' would be visible.
--     * @building@ mirrors a mixed shipped entry.
--     * @sparse_defaults@ has no counterpart in
--       @data/notification_categories.yaml@ (every shipped entry states
--       all three), and exists solely to pin requirement 4: the REGISTRY
--       is the bottom layer, so a field IT omits has nothing to inherit
--       and stays false.
mixedRegistryYaml ∷ String
mixedRegistryYaml = unlines
    [ "categories:"
    , "  - id: survival_critical"
    , "    display_name: Survival (Critical)"
    , "    default_settings: {log: true, popup: true, pause: true}"
    , "  - id: building"
    , "    display_name: Building"
    , "    default_settings: {log: true, popup: false, pause: false}"
    , "  - id: sparse_defaults"
    , "    display_name: Sparse Defaults"
    , "    default_settings: {log: true}"
    ]

-- | Resolve one category out of a loaded config, failing the example
--   with a useful message rather than pattern-matching on 'Nothing'.
categoryOf ∷ HM.HashMap Text CategoryCfg → Text → IO CategoryCfg
categoryOf cfg catId = case HM.lookup catId cfg of
    Just c  → return c
    Nothing → do
        expectationFailure $
            "category " <> T.unpack catId <> " missing from resolved config"
        fail "unreachable"

-- | The resolved checkbox triple, the shape every #1938 example asserts on.
triple ∷ CategoryCfg → (Bool, Bool, Bool)
triple c = (ccLog c, ccPopup c, ccPause c)

-- | Load a registry + a hand-written overrides file from a scratch dir.
loadWithOverrides ∷ FilePath → String → String
                  → IO (HM.HashMap Text CategoryCfg)
loadWithOverrides dir registry overrides = do
    logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
    let registryPath  = dir </> "registry.yaml"
        overridesPath = dir </> "notifications.local.yaml"
    writeFile registryPath registry
    writeFile overridesPath overrides
    fst ⊚ loadNotificationCfg logger registryPath overridesPath

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
           \warning with the unchanged source-side classification" $
            withTempDir $ \dir → do
                (logger, dumpLog) ← capturingLoggerLeveled
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
                -- #2210 requirement 2: splitting the destination write
                -- out of the diagnosis must leave this text alone, so
                -- the whole classification is pinned, not the broad
                -- "could not be migrated" fragment it shares with a
                -- hypothetical successor line.
                entries ← dumpLog
                case legacyWarnings entries of
                    [msg] → (sourceFailureLine `T.isInfixOf` msg)
                                `shouldBe` True
                    other → expectationFailure $
                        "expected exactly one legacy-config warning, got: "
                          <> show other

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

    describe "Engine.Core.Init.migrateLegacyConfig destination write \
             \failure (#2210)" $ do
        -- A directory occupying the local path is the portable way to
        -- make the destination unwritable: the 'doesFileExist' gate
        -- still reports no local file, so migration runs to the copy,
        -- but the copy cannot install a regular file there — @rename(2)@
        -- refuses to replace a directory with a file (#2202 made that
        -- copy a durable publish). No permission bit is touched, so this
        -- behaves the same for a root-privileged runner.
        let unwritableFixture legacyBody dir = do
                let legacy = dir </> "legacy.yaml"
                    local  = dir </> "local.yaml"
                    deflt  = dir </> "default.yaml"
                writeFile deflt  "required: 5\n"
                writeFile legacy legacyBody
                createDirectoryIfMissing True local
                bytes    ← BS.readFile legacy
                expected ← copyFailureText legacy local
                return (legacy, local, deflt, bytes, expected)

        it "blames the destination write, not the legacy file, when an \
           \UNCHECKED migration cannot write the local path" $
            withTempDir $ \dir → do
                (legacy, local, deflt, bytes, expected)
                    ← unwritableFixture "required: 42\n" dir
                (logger, dumpLog) ← capturingLoggerLeveled
                migrateLegacyConfig probeCfg logger Nothing legacy local
                entries ← dumpLog
                shouldBeWriteFailureWarning entries local expected
                logged (map snd entries) (migratedLine legacy local)
                    `shouldBe` False
                shouldHaveLeftNoTrace legacy bytes local deflt

        it "blames the destination write for a CHECKED migration whose \
           \legacy file is not a neutral placeholder" $
            withTempDir $ \dir → do
                (legacy, local, deflt, bytes, expected)
                    ← unwritableFixture "required: 42\n" dir
                let record = dir </> "legacy-neutral.local.yaml"
                (logger, dumpLog) ← capturingLoggerLeveled
                migrateLegacyConfig probeCfg logger
                    (Just (neutralityCheck deflt record)) legacy local
                entries ← dumpLog
                shouldBeWriteFailureWarning entries local expected
                logged (map snd entries) "carries no player state"
                    `shouldBe` False
                -- The copy was reached, so no neutrality record was
                -- written on the way past.
                doesFileExist record `shouldReturn` False
                shouldHaveLeftNoTrace legacy bytes local deflt

        it "re-attempts the migration on the next boot, warning again, \
           \because the failed copy left the existence gate open" $
            withTempDir $ \dir → do
                (legacy, local, deflt, bytes, expected)
                    ← unwritableFixture "required: 42\n" dir
                (logger0, _) ← capturingLoggerLeveled
                migrateLegacyConfig probeCfg logger0 Nothing legacy local
                (logger1, dumpLog1) ← capturingLoggerLeveled
                migrateLegacyConfig probeCfg logger1 Nothing legacy local
                entries ← dumpLog1
                shouldBeWriteFailureWarning entries local expected
                shouldHaveLeftNoTrace legacy bytes local deflt

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
                    `shouldReturn` Right ()
                (cfg1, _) ← loadNotificationCfg logger registryPath overridesPath
                case HM.lookup "debug" cfg1 of
                    Nothing → expectationFailure "debug category missing from resolved config"
                    Just c  → ccLog c `shouldBe` True

    describe "Engine.Asset.YamlNotifications sparse config overrides \
             \inherit registry defaults (#1938)" $ do
        it "resolves a checkbox the overrides file omits to that \
           \category's own registry default, not to false" $
            withTempDir $ \dir → do
                cfg ← loadWithOverrides dir mixedRegistryYaml $ unlines
                    [ "categories:"
                    , "  survival_critical:"
                    , "    log: true"
                    ]
                c ← categoryOf cfg "survival_critical"
                triple c `shouldBe` (True, True, True)

        it "reads the authored field even when it is the only one \
           \present, without disturbing the inherited two" $
            withTempDir $ \dir → do
                cfg ← loadWithOverrides dir mixedRegistryYaml $ unlines
                    [ "categories:"
                    , "  survival_critical:"
                    , "    popup: false"
                    ]
                c ← categoryOf cfg "survival_critical"
                triple c `shouldBe` (True, False, True)

        it "lets an explicit false defeat a true registry default" $
            withTempDir $ \dir → do
                cfg ← loadWithOverrides dir mixedRegistryYaml $ unlines
                    [ "categories:"
                    , "  survival_critical: {log: false, popup: false, \
                      \pause: false}"
                    ]
                c ← categoryOf cfg "survival_critical"
                triple c `shouldBe` (False, False, False)

        it "treats an explicit YAML null the same as an omitted field" $
            withTempDir $ \dir → do
                cfg ← loadWithOverrides dir mixedRegistryYaml $ unlines
                    [ "categories:"
                    , "  survival_critical:"
                    , "    log: false"
                    , "    popup: null"
                    , "    pause: ~"
                    ]
                c ← categoryOf cfg "survival_critical"
                triple c `shouldBe` (False, True, True)

        it "leaves a category absent from the overrides file resolving \
           \entirely from the registry" $
            withTempDir $ \dir → do
                cfg ← loadWithOverrides dir mixedRegistryYaml $ unlines
                    [ "categories:"
                    , "  survival_critical: {pause: false}"
                    ]
                b ← categoryOf cfg "building"
                triple b `shouldBe` (True, False, False)

        it "keeps false as the registry's own base for a default_settings \
           \field the registry omits (nothing beneath it to inherit)" $
            withTempDir $ \dir → do
                cfg ← loadWithOverrides dir mixedRegistryYaml $ unlines
                    [ "categories:"
                    , "  survival_critical: {log: true}"
                    ]
                c ← categoryOf cfg "sparse_defaults"
                triple c `shouldBe` (True, False, False)

        it "falls back to the registry defaults when an overrides file \
           \states a non-boolean value, exactly as a malformed file does" $
            withTempDir $ \dir → do
                cfg ← loadWithOverrides dir mixedRegistryYaml $ unlines
                    [ "categories:"
                    , "  survival_critical: {log: \"yes\"}"
                    ]
                c ← categoryOf cfg "survival_critical"
                triple c `shouldBe` (True, True, True)

        it "round-trips the settings tab's write through load unchanged, \
           \explicit falses included" $
            withTempDir $ \dir → do
                logger ← initLogger defaultLogConfig
                    { lcBackend = LogToHandle stderr }
                let registryPath  = dir </> "registry.yaml"
                    overridesPath = dir </> "notifications.local.yaml"
                writeFile registryPath mixedRegistryYaml
                (cfg0, _) ← loadNotificationCfg logger registryPath overridesPath
                let updated = HM.adjust
                        (\c → c { ccPause = False }) "survival_critical" cfg0
                writeNotificationOverrides overridesPath updated
                    `shouldReturn` Right ()
                (cfg1, _) ← loadNotificationCfg logger registryPath overridesPath
                c ← categoryOf cfg1 "survival_critical"
                triple c `shouldBe` (True, True, False)

        it "migrates a PARTIAL legacy notifications.yaml and resolves its \
           \omitted fields from the registry (#786's partial legacy \
           \state, resolved by overlay semantics)" $
            withTempDir $ \dir → do
                (logger, dumpLog) ← capturingLogger
                let legacy        = dir </> "notifications.yaml"
                    overridesPath = dir </> "notifications.local.yaml"
                    registryPath  = dir </> "registry.yaml"
                writeFile registryPath mixedRegistryYaml
                writeFile legacy $ unlines
                    [ "categories:"
                    , "  survival_critical:"
                    , "    log: false"
                    ]
                migrateLegacyConfig (Proxy ∷ Proxy OverridesFile) logger
                    Nothing legacy overridesPath
                msgs ← dumpLog
                logged msgs (migratedLine legacy overridesPath)
                    `shouldBe` True
                (cfg, _) ← loadNotificationCfg logger registryPath overridesPath
                c ← categoryOf cfg "survival_critical"
                triple c `shouldBe` (False, True, True)
