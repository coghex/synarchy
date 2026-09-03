{-# LANGUAGE ScopedTypeVariables #-}
-- | The @config/@ durable-write contract (#2202).
--
--   Three layers, in the order a failure should be diagnosed:
--
--     1. 'Engine.Core.ConfigWrite' itself — the publish sequence, and
--        what each PHASE's failure leaves on disk. The phases are driven
--        through the module's own 'ConfigWriteOps' seam, because the one
--        thing a real filesystem will not do on demand is fail in the
--        exact step an example is about.
--     2. Every production writer that goes through it, driven against a
--        target it genuinely cannot write, with the previous file's
--        bytes and the emitted warning both asserted.
--     3. The four Lua verbs, in a real engine, returning @false@ instead
--        of raising — and keeping their live state, which is each
--        family's stated policy on a failed write.
--
--   __Why no permission-bit fixture.__ CI runs the suite as root, where
--   a @chmod 0500@ directory is still writable, so a permission-based
--   "unwritable directory" would pass vacuously there (the same reason
--   'Test.Headless.Load.Terminalize' uses a socket rather than a mode).
--   Every real-filesystem refusal here is structural instead: a path
--   component that is a regular file (@ENOTDIR@ for every user), or a
--   target occupied by a DIRECTORY, which @rename(2)@ refuses to replace
--   with a file for root as well.
module Test.Headless.Core.ConfigWrite (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent
  (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception
  (ErrorCall(..), SomeAsyncException(..), SomeException, throwIO, toException
  , try)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, modifyIORef', readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List (sort)
import System.Directory
  ( createDirectory, createDirectoryIfMissing, doesFileExist, listDirectory
  , removeFile )
import System.FilePath ((</>), takeDirectory)

import Engine.Asset.YamlNotifications
  (loadNotificationCfg, writeNotificationOverrides)
import Engine.Core.ConfigWrite
  ( ConfigWriteOps(..), copyConfigFile, realConfigWriteOps, removeConfigFile
  , removeConfigFileWith, writeConfigBytes, writeConfigBytesWith )
import Engine.Core.Init
  (LegacyNeutralityCheck(..), migrateLegacyConfig, resolveConfigPath)
import Engine.Core.Log
  ( LogBackend(..), LogConfig(..), LogEntry(..), LoggerState
  , defaultLogConfig, initLogger )
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (VideoConfig, defaultVideoConfig, saveVideoConfig)
import Engine.Input.Bindings (loadKeyBindings, saveKeyBindings)
import Engine.PlayerEvent (CategoryCfg(..))
import Engine.Save.Config
  (SaveConfig(..), defaultSaveConfig, loadSaveConfig, writeSaveConfig)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Proxy (Proxy(..))
import System.Timeout (timeout)
import World.Save.Storage.Durable (WriteStep(..), claimUniquePath)

import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation
  (isInsideIsolatedResourceRoot, withExclusiveTempDirectory
  , withIsolatedResourceRoot)

-- * Fixtures ---------------------------------------------------------

-- | A scratch directory this example exclusively created.
inTemp ∷ (FilePath → IO α) → IO α
inTemp = withExclusiveTempDirectory "synarchy-config-write-spec"

-- | A logger whose entries are captured in memory, so the warning a
--   failed write emits can be asserted to name the path and the cause.
capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    seen ← newIORef ([] ∷ [LogEntry])
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' seen (e :)) }
    pure (logger, reverse ⊚ readIORef seen)

-- | The captured messages, joined — every assertion here is "the
--   diagnostic mentions X", never "line N said exactly Y".
saidAll ∷ IO [LogEntry] → [Text] → Expectation
saidAll drain needles = do
    said ← T.concat ∘ map leMessage ⊚ drain
    forM_ needles $ \needle →
        (needle, needle `T.isInfixOf` said) `shouldBe` (needle, True)

-- | A path whose PARENT is a regular file. @ENOTDIR@ for every user,
--   root included, at both the directory-creation and the temporary-claim
--   steps — see the module header for why this replaces a mode fixture.
unwritablePath ∷ FilePath → String → IO FilePath
unwritablePath dir name = do
    let blocker = dir </> (name ⧺ "-blocker")
    BS.writeFile blocker "not a directory\n"
    pure (blocker </> name)

-- | Directory contents, ordered, so "no temporary was left behind" is a
--   comparison rather than a search.
entriesOf ∷ FilePath → IO [FilePath]
entriesOf = fmap sort ∘ listDirectory

-- | Ops that record the phase sequence and the arguments each phase was
--   handed, so ORDER (write, then rename, then sync) and PLACEMENT (the
--   temporary is a sibling of the target; the sync is of the target's
--   own directory) are observable rather than assumed.
recordingOps ∷ IO (ConfigWriteOps, IO [(Text, FilePath)])
recordingOps = do
    seen ← newIORef ([] ∷ [(Text, FilePath)])
    let note phase path = modifyIORef' seen ((phase, path) :)
    pure ( realConfigWriteOps
             { cwoWrite = \tmp bytes → do
                 note "write" tmp
                 cwoWrite realConfigWriteOps tmp bytes
             , cwoRename = \tmp dst → do
                 note "rename" tmp
                 cwoRename realConfigWriteOps tmp dst
             , cwoSyncDir = \dir → do
                 note "sync" dir
                 cwoSyncDir realConfigWriteOps dir
             , cwoRemoveTarget = \target → do
                 note "remove" target
                 cwoRemoveTarget realConfigWriteOps target
             }
         , reverse ⊚ readIORef seen )

-- | A write step that first creates the temporary and then fails in it,
--   which is what makes "no temporary remains" a real assertion: an
--   implementation that skipped the cleanup would leave this file.
failingWriteAfterCreating ∷ SomeException → FilePath → BS.ByteString
                          → IO (Either (WriteStep, SomeException) ())
failingWriteAfterCreating e tmp _ = do
    BS.writeFile tmp "half a document"
    pure (Left (StepWrite, e))

-- | A minimal config type for 'migrateLegacyConfig', mirroring
--   'Test.Headless.Core.ConfigState'’s: one REQUIRED field, so the
--   migration's schema gate is real without depending on a concrete
--   engine config type.
newtype ProbeCfg = ProbeCfg Int deriving (Show, Eq)

instance FromJSON ProbeCfg where
    parseJSON = withObject "ProbeCfg" $ \v → ProbeCfg ⊚ v .: "required"

probeCfg ∷ Proxy ProbeCfg
probeCfg = Proxy

-- | A bare Lua backend with the real engine API registered — the same
--   fixture 'Test.Headless.UI.SettingsRevert' uses, so the verbs under
--   test are the production ones.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | The debug console is single-line, so a scenario is spelled as
--   semicolon-separated statements joined here.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← executeDebugLua (lbsLuaState ls) code
    when ("error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t) $
        expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t

-- | Occupy a cwd-relative config path with a DIRECTORY. @rename(2)@
--   refuses to replace a directory with a file for every user, so the
--   publish step fails for real — no permission bits, no injection, and
--   nothing the engine can be running as makes it succeed.
blockWithDirectory ∷ FilePath → IO ()
blockWithDirectory path = do
    present ← doesFileExist path
    when present (removeFile path)
    createDirectory path

-- * Spec -------------------------------------------------------------

spec ∷ Spec
spec = do
    helperSpec
    writerSpec
    luaVerbSpec

-- ** The helper ------------------------------------------------------

helperSpec ∷ Spec
helperSpec = describe "Engine.Core.ConfigWrite publish sequence" $ do

    it "publishes through a sibling temporary, then syncs the target's \
       \own directory, and leaves nothing else behind" $ inTemp $ \dir → do
        (ops, drain) ← recordingOps
        let target = dir </> "config" </> "video.local.yaml"
        writeConfigBytesWith ops target "video: {}\n" `shouldReturn` Right ()
        BS.readFile target `shouldReturn` "video: {}\n"
        entriesOf (takeDirectory target) `shouldReturn` ["video.local.yaml"]
        phases ← drain
        map fst phases `shouldBe` ["write", "rename", "sync"]
        -- The temporary is a sibling of the target: a cross-filesystem
        -- rename is not atomic, so a temporary in /tmp would silently
        -- degrade the whole contract to a copy.
        let tempPaths = [ p | (phase, p) ← phases, phase ≢ "sync" ]
        map takeDirectory tempPaths
            `shouldBe` replicate (length tempPaths) (takeDirectory target)
        tempPaths `shouldNotContain` [target]
        -- The directory that is synced is the one whose entry the rename
        -- replaced, not the file.
        [ p | ("sync", p) ← phases ] `shouldBe` [takeDirectory target]

    it "replaces an existing target rather than appending to it" $
        inTemp $ \dir → do
            let target = dir </> "keybinds.local.yaml"
            writeConfigBytes target "first\n" `shouldReturn` Right ()
            writeConfigBytes target "second\n" `shouldReturn` Right ()
            BS.readFile target `shouldReturn` "second\n"
            entriesOf dir `shouldReturn` ["keybinds.local.yaml"]

    it "leaves the previous target byte-identical, and removes its \
       \temporary, when the write fails" $ inTemp $ \dir → do
        let target = dir </> "video.local.yaml"
            ops = realConfigWriteOps
                { cwoWrite = failingWriteAfterCreating
                    (toException (ErrorCall "disk full")) }
        BS.writeFile target "previous\n"
        outcome ← writeConfigBytesWith ops target "replacement\n"
        outcome `shouldSatisfy` isLeft
        leftText outcome `shouldContainText` T.pack target
        leftText outcome `shouldContainText` "disk full"
        BS.readFile target `shouldReturn` "previous\n"
        entriesOf dir `shouldReturn` ["video.local.yaml"]

    it "leaves the previous target byte-identical, and removes its \
       \temporary, when the publish rename fails" $ inTemp $ \dir → do
        let target = dir </> "video.local.yaml"
            ops = realConfigWriteOps
                { cwoRename = \_ _ → throwIO (ErrorCall "rename refused") }
        BS.writeFile target "previous\n"
        outcome ← writeConfigBytesWith ops target "replacement\n"
        outcome `shouldSatisfy` isLeft
        leftText outcome `shouldContainText` "rename refused"
        BS.readFile target `shouldReturn` "previous\n"
        entriesOf dir `shouldReturn` ["video.local.yaml"]

    it "reports an unconfirmed directory sync as Left, with the COMPLETE \
       \new file visible and no temporary left" $ inTemp $ \dir → do
        let target = dir </> "video.local.yaml"
            ops = realConfigWriteOps
                { cwoSyncDir = \_ → throwIO (ErrorCall "sync refused") }
        BS.writeFile target "previous\n"
        outcome ← writeConfigBytesWith ops target "replacement\n"
        outcome `shouldSatisfy` isLeft
        leftText outcome `shouldContainText` "sync refused"
        -- Past the rename the publication already happened: the visible
        -- file is the whole new document, never a partial one, even
        -- though durability could not be confirmed.
        BS.readFile target `shouldReturn` "replacement\n"
        entriesOf dir `shouldReturn` ["video.local.yaml"]

    it "returns Left and creates nothing when the target's directory \
       \cannot be made" $ inTemp $ \dir → do
        target ← unwritablePath dir "video.local.yaml"
        outcome ← writeConfigBytes target "replacement\n"
        outcome `shouldSatisfy` isLeft
        leftText outcome `shouldContainText` T.pack target
        doesFileExist target `shouldReturn` False
        entriesOf dir `shouldReturn` ["video.local.yaml-blocker"]

    it "rethrows an asynchronous exception instead of converting it to \
       \Left, after removing the temporary" $ inTemp $ \dir → do
        let target = dir </> "video.local.yaml"
            ops = realConfigWriteOps
                { cwoWrite = failingWriteAfterCreating
                    (toException (SomeAsyncException (ErrorCall "killed"))) }
        BS.writeFile target "previous\n"
        raised ← try (writeConfigBytesWith ops target "replacement\n")
        case raised ∷ Either SomeException (Either Text ()) of
            Left _  → pure ()
            Right r → expectationFailure $
                "expected the async exception to propagate, got " ⧺ show r
        -- Async propagation is what keeps shutdown's killThread able to
        -- reach the Lua thread; the cleanup still has to have happened.
        BS.readFile target `shouldReturn` "previous\n"
        entriesOf dir `shouldReturn` ["video.local.yaml"]

    it "still removes the temporary when an ASYNCHRONOUS exception \
       \escapes from the publish rename" $ inTemp $ \dir → do
        -- Round-1 review: the rename's async exception is rethrown
        -- before the rename-failure branch can clean up, so cleanup
        -- ownership has to be held from the moment the name is claimed.
        let target = dir </> "video.local.yaml"
            ops = realConfigWriteOps
                { cwoRename = \_ _ →
                    throwIO (SomeAsyncException (ErrorCall "killed")) }
        BS.writeFile target "previous\n"
        raised ← try (writeConfigBytesWith ops target "replacement\n")
        case raised ∷ Either SomeException (Either Text ()) of
            Left _  → pure ()
            Right r → expectationFailure $
                "expected the async exception to propagate, got " ⧺ show r
        BS.readFile target `shouldReturn` "previous\n"
        entriesOf dir `shouldReturn` ["video.local.yaml"]

    it "names a temporary it could not remove in the Left, rather than \
       \leaving one silently behind" $ inTemp $ \dir → do
        let target = dir </> "video.local.yaml"
            ops = realConfigWriteOps
                { cwoWrite = failingWriteAfterCreating
                    (toException (ErrorCall "disk full"))
                , cwoDiscardTemp = \tmp →
                    pure ["failed to remove stale artifact " <> T.pack tmp] }
        BS.writeFile target "previous\n"
        outcome ← writeConfigBytesWith ops target "replacement\n"
        outcome `shouldSatisfy` isLeft
        leftText outcome `shouldContainText` "disk full"
        leftText outcome `shouldContainText` "failed to remove stale artifact"
        BS.readFile target `shouldReturn` "previous\n"

    it "removes a config file and syncs its directory, so the removal is \
       \durable before it is reported" $ inTemp $ \dir → do
        (ops, drain) ← recordingOps
        let target = dir </> "save.local.yaml"
        BS.writeFile target "save: {}\n"
        removeConfigFileWith ops target `shouldReturn` Right True
        doesFileExist target `shouldReturn` False
        phases ← drain
        map fst phases `shouldBe` ["remove", "sync"]
        -- The directory whose entry vanished is the one that is synced.
        [ p | ("sync", p) ← phases ] `shouldBe` [dir]
        -- And the production entry point behaves the same way.
        let other = dir </> "notifications.local.yaml"
        BS.writeFile other "categories: {}\n"
        removeConfigFile other `shouldReturn` Right True
        doesFileExist other `shouldReturn` False

    it "reports nothing removed, and syncs nothing, when the file is \
       \already absent" $ inTemp $ \dir → do
        (ops, drain) ← recordingOps
        removeConfigFileWith ops (dir </> "save.local.yaml")
            `shouldReturn` Right False
        drain `shouldReturn` []

    it "reports a failed unlink rather than claiming the file is gone" $
        inTemp $ \dir → do
            let target = dir </> "save.local.yaml"
                ops = realConfigWriteOps
                    { cwoRemoveTarget = \_ → throwIO (ErrorCall "unlink refused") }
            BS.writeFile target "save: {}\n"
            outcome ← removeConfigFileWith ops target
            outcome `shouldSatisfy` isLeftBool
            leftTextBool outcome `shouldContainText` T.pack target
            leftTextBool outcome `shouldContainText` "unlink refused"
            doesFileExist target `shouldReturn` True

    it "reports an unconfirmed directory sync after an unlink that DID \
       \happen" $ inTemp $ \dir → do
        let target = dir </> "save.local.yaml"
            ops = realConfigWriteOps
                { cwoSyncDir = \_ → throwIO (ErrorCall "sync refused") }
        BS.writeFile target "save: {}\n"
        outcome ← removeConfigFileWith ops target
        outcome `shouldSatisfy` isLeftBool
        leftTextBool outcome `shouldContainText` "sync refused"
        -- The unlink itself already happened and is visible; only its
        -- durability is unconfirmed.
        doesFileExist target `shouldReturn` False

    it "leaves no placeholder behind when an asynchronous exception \
       \lands inside the temporary CLAIM" $ inTemp $ \dir → do
        -- Round-2 review: the claim opens a real file and only then
        -- removes it, so between those two steps a file existed that
        -- nothing owned — and 'writeConfigBytesWith' cannot own it
        -- either, because the claim has not returned its name yet. A
        -- @killThread@ landing there left a tmp- file in the player's
        -- config/ for ever, under a name no later run reproduces.
        --
        -- Kill a claiming thread repeatedly so a delivery lands inside
        -- that window. Every iteration of the loop the thread runs is
        -- spent inside the claim, so the window is where a kill almost
        -- always lands; forty rounds make "it never landed there" not a
        -- way this can pass.
        finished ← timeout 20000000 $ forM_ [1 ∷ Int .. 40] $ \_ → do
            started ← newEmptyMVar
            tid ← forkIO $ do
                putMVar started ()
                forever (void (claimUniquePath dir "tmp-claim"))
            takeMVar started
            threadDelay 300
            killThread tid
        finished `shouldBe` Just ()
        entriesOf dir `shouldReturn` []

    it "copies a config file durably, and reports an unreadable source \
       \without touching the destination" $ inTemp $ \dir → do
        let src = dir </> "legacy.yaml"
            dst = dir </> "local.yaml"
        BS.writeFile src "required: 1\n"
        copyConfigFile src dst `shouldReturn` Right ()
        BS.readFile dst `shouldReturn` "required: 1\n"
        outcome ← copyConfigFile (dir </> "absent.yaml") dst
        outcome `shouldSatisfy` isLeft
        BS.readFile dst `shouldReturn` "required: 1\n"

-- ** The production writers ------------------------------------------

writerSpec ∷ Spec
writerSpec = describe "config writers report a failed write" $ do

    it "saveVideoConfig returns Left, warns with the path, and leaves an \
       \existing file byte-identical" $ inTemp $ \dir → do
        (logger, drain) ← capturingLogger
        let good = dir </> "video.local.yaml"
        saveVideoConfig logger good probeVideo `shouldReturn` Right ()
        BS.writeFile good "video: {resolution: {width: 1, height: 1}}\n"
        bad ← unwritablePath dir "video.local.yaml"
        outcome ← saveVideoConfig logger bad probeVideo
        outcome `shouldSatisfy` isLeft
        BS.readFile good
            `shouldReturn` "video: {resolution: {width: 1, height: 1}}\n"
        saidAll drain [T.pack bad, "Video config not saved"]

    it "saveKeyBindings returns Left and warns with the path" $
        inTemp $ \dir → do
            (logger, drain) ← capturingLogger
            let good = dir </> "keybinds.local.yaml"
                bindings = Map.fromList [("toggleEventLog", ["F9"])]
            saveKeyBindings logger good bindings `shouldReturn` Right ()
            -- The loader merges what it reads over the built-in
            -- defaults, so the round trip is asserted on the action this
            -- example wrote rather than on the whole table.
            reloaded ← loadKeyBindings logger good
            Map.lookup "toggleEventLog" reloaded `shouldBe` Just ["F9"]
            bad ← unwritablePath dir "keybinds.local.yaml"
            outcome ← saveKeyBindings logger bad bindings
            outcome `shouldSatisfy` isLeft
            doesFileExist bad `shouldReturn` False
            saidAll drain [T.pack bad, "Keybindings not saved"]

    it "writeNotificationOverrides returns Left without creating a \
       \partial file" $ inTemp $ \dir → do
        (logger, _) ← capturingLogger
        let registry = dir </> "registry.yaml"
            good     = dir </> "notifications.local.yaml"
        writeFile registry registryYaml
        (cfg, _) ← loadNotificationCfg logger registry good
        bad ← unwritablePath dir "notifications.local.yaml"
        outcome ← writeNotificationOverrides bad cfg
        outcome `shouldSatisfy` isLeft
        leftText outcome `shouldContainText` T.pack bad
        doesFileExist bad `shouldReturn` False

    it "writeSaveConfig returns Left and warns with the path" $
        inTemp $ \dir → do
            (logger, drain) ← capturingLogger
            let deflt = dir </> "save_default.yaml"
            writeFile deflt "save:\n  enabled: false\n  interval_minutes: 10\n\
                            \  rotation_depth: 3\n"
            bad ← unwritablePath dir "save.local.yaml"
            -- A value that DIFFERS from the template, so this is a write
            -- rather than the "matches the template, remove the file"
            -- path.
            outcome ← writeSaveConfig logger deflt bad
                defaultSaveConfig { scEnabled = True }
            outcome `shouldSatisfy` isLeft
            saidAll drain [T.pack bad, "save config"]

    it "writeSaveConfig's template-match path removes the local file \
       \durably, and reports a failure instead of claiming success" $
        inTemp $ \dir → do
            (logger, drain) ← capturingLogger
            let deflt = dir </> "save_default.yaml"
                local = dir </> "save.local.yaml"
            writeFile deflt "save:\n  enabled: false\n  interval_minutes: 10\n\
                            \  rotation_depth: 3\n"
            -- Establish an override, then ask for the template's own
            -- values back: that is the branch that PUBLISHES by
            -- removing the file rather than by writing one.
            writeSaveConfig logger deflt local
                defaultSaveConfig { scEnabled = True } `shouldReturn` Right ()
            doesFileExist local `shouldReturn` True
            writeSaveConfig logger deflt local defaultSaveConfig
                `shouldReturn` Right ()
            doesFileExist local `shouldReturn` False
            loadSaveConfig logger deflt local
                `shouldReturn` defaultSaveConfig
            saidAll drain ["matches the tracked defaults; removed"]

    it "the notification materializer keeps the registry defaults, \
       \creates no partial file, and does not claim success" $
        inTemp $ \dir → do
            (logger, drain) ← capturingLogger
            let registry = dir </> "registry.yaml"
            writeFile registry registryYaml
            bad ← unwritablePath dir "notifications.local.yaml"
            (cfg, order) ← loadNotificationCfg logger registry bad
            -- The defaults are derived BEFORE the write, so the boot
            -- continues on them rather than losing the category.
            order `shouldBe` ["debug"]
            fmap ccLog (HM.lookup "debug" cfg) `shouldBe` Just False
            doesFileExist bad `shouldReturn` False
            said ← T.concat ∘ map leMessage ⊚ drain
            said `shouldContainText` "Could not write default notification"
            said `shouldContainText` T.pack bad
            -- Requirement: never log the success line after a Left.
            said `shouldNotContainText` "Wrote default notification overrides"

    it "migrateLegacyConfig reports a failed copy and leaves the local \
       \path absent, so a later boot can retry" $ inTemp $ \dir → do
        (logger, drain) ← capturingLogger
        let legacy = dir </> "video.yaml"
        writeFile legacy "required: 7\n"
        local ← unwritablePath dir "video.local.yaml"
        migrateLegacyConfig probeCfg logger Nothing legacy local
        doesFileExist local `shouldReturn` False
        saidAll drain ["could not be migrated", T.pack local]
        -- The migration never happened, so resolution still falls back
        -- to the versioned default rather than to a half-written file.
        resolveConfigPath local legacy `shouldReturn` legacy

    it "migrateLegacyConfig reports a failed neutrality record and still \
       \leaves the local path absent" $ inTemp $ \dir → do
        (logger, drain) ← capturingLogger
        let legacy = dir </> "video.yaml"
            deflt  = dir </> "video_default.yaml"
            local  = dir </> "video.local.yaml"
        writeFile legacy "required: 7\n"
        writeFile deflt  "required: 7\n"
        record ← unwritablePath dir "video.legacy-neutral.local.yaml"
        migrateLegacyConfig probeCfg logger
            (Just (LegacyNeutralityCheck deflt record)) legacy local
        doesFileExist local `shouldReturn` False
        doesFileExist record `shouldReturn` False
        saidAll drain
            ["Could not record the neutral-placeholder determination"
            , T.pack record]

-- ** The Lua verbs ---------------------------------------------------

luaVerbSpec ∷ Spec
luaVerbSpec =
  describe "config Lua verbs return false instead of raising (#2202)" $ do

    it "engine.saveVideoConfig returns false and keeps the live config" $
        withBlockedConfig "config/video.local.yaml" $ \ls → do
            result ← evalOk ls
                "engine.setTooltipDwellMs(311); \
                \local ok = engine.saveVideoConfig(); \
                \return ok, engine.getTooltipDwellMs()"
            T.splitOn "\t" result `shouldBe` ["false", "311"]

    it "a failed video save leaves Settings Back's baseline on the \
       \configuration that is actually saved" $
        withBlockedConfig "config/video.local.yaml" $ \ls → do
            -- Round-2 review: data.save() refreshed the revert baseline
            -- unconditionally, so a failed write adopted values that
            -- only ever reached the live ref — and Back could no longer
            -- reach the configuration genuinely on disk.
            result ← evalOk ls $ luaLines
                [ "local d = require('scripts.settings.data');"
                , "engine.setTooltipDwellMs(400);"
                , "d.reload(); d.resetPending();"
                -- Edit, then save into the blocked path.
                , "d.apply({tooltipDwellMs = 250});"
                , "local live = engine.getTooltipDwellMs();"
                , "d.save({});"
                , "d.revert();"
                , "return live, engine.getTooltipDwellMs(),"
                , "  d.savedVideo.tooltipDwellMs"
                ]
            -- The edit was live before the revert, and the revert put
            -- the SAVED value back rather than adopting the unsaved one.
            T.splitOn "\t" result `shouldBe` ["250", "400", "400"]

    it "engine.saveKeybinds returns false and keeps the live bindings" $
        withBlockedConfig "config/keybinds.local.yaml" $ \ls → do
            result ← evalOk ls
                "engine.addActionKey('toggleEventLog', 'F9'); \
                \local ok = engine.saveKeybinds(); \
                \local keys = engine.getKeybinds()['toggleEventLog']; \
                \local held = false; \
                \for _, k in ipairs(keys) do if k == 'F9' then held = true end end; \
                \return ok, held"
            T.splitOn "\t" result `shouldBe` ["false", "true"]

    it "engine.setNotificationOverrides returns false and keeps the live \
       \merge" $
        withBlockedConfig "config/notifications.local.yaml" $ \ls → do
            result ← evalOk ls
                "local cfg = engine.getNotificationCfg(); \
                \local id = cfg[1].id; \
                \local ok = engine.setNotificationOverrides(\
                  \{[id] = {log = true, popup = true, pause = true}}); \
                \local after; \
                \for _, c in ipairs(engine.getNotificationCfg()) do \
                  \if c.id == id then after = c.log end end; \
                \return ok, after"
            T.splitOn "\t" result `shouldBe` ["false", "true"]

    it "engine.setSaveConfig returns false and the scheduler keeps the \
       \applied values" $
        withBlockedConfig "config/save.local.yaml" $ \ls → do
            result ← evalOk ls
                "local d = require('scripts.settings.data'); \
                \d.reloadSave(); \
                \d.currentSave.enabled = true; \
                \d.currentSave.intervalMinutes = 17; \
                \local ok = d.saveSaveConfig(); \
                \return ok, d.currentSave.intervalMinutes"
            T.splitOn "\t" result `shouldBe` ["false", "17"]

-- | Boot a real engine inside an isolated resource root, occupy one
--   cwd-relative config path with a directory, and hand a Lua backend to
--   the example. Isolation wraps the boot ('withIsolatedResourceRoot'
--   AROUND 'withHeadlessEngine', #1357), because engine initialization
--   is itself a config writer.
withBlockedConfig ∷ FilePath → (LuaBackendState → IO ()) → IO ()
withBlockedConfig path action =
    withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
        isInsideIsolatedResourceRoot `shouldReturn` True
        createDirectoryIfMissing True (takeDirectory path)
        blockWithDirectory path
        ls ← newBareLuaBackend env
        action ls

-- * Small helpers ----------------------------------------------------

registryYaml ∷ String
registryYaml = unlines
    [ "categories:"
    , "  - id: debug"
    , "    display_name: Debug"
    , "    default_settings: {log: false, popup: false, pause: false}"
    ]

-- | A video config well inside every #2198 domain, so these examples
--   fail only for the reason they are about.
probeVideo ∷ VideoConfig
probeVideo = defaultVideoConfig

isLeft ∷ Either α β → Bool
isLeft = either (const True) (const False)

leftText ∷ Either Text () → Text
leftText = either id (const "")

isLeftBool ∷ Either Text Bool → Bool
isLeftBool = either (const True) (const False)

leftTextBool ∷ Either Text Bool → Text
leftTextBool = either id (const "")

shouldContainText ∷ HasCallStack ⇒ Text → Text → Expectation
shouldContainText haystack needle =
    (needle, needle `T.isInfixOf` haystack) `shouldBe` (needle, True)

shouldNotContainText ∷ HasCallStack ⇒ Text → Text → Expectation
shouldNotContainText haystack needle =
    (needle, needle `T.isInfixOf` haystack) `shouldBe` (needle, False)
