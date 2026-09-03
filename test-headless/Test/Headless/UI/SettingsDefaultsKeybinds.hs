{-# LANGUAGE ScopedTypeVariables #-}
-- | #1357: the two halves of the developer-config isolation contract.
--
--   1. 'Test.Headless.Harness.Isolation.withIsolatedResourceRoot' really
--      does keep a cwd-relative production write out of the checkout —
--      asserted against the checkout's OWN @config/@ directory, byte for
--      byte, so the gate covers the developer's gitignored
--      @*.local.yaml@ files, the tracked templates beside them, and the
--      creation of any file that was not there before.
--
--   2. The player-facing keybinding contract survives that isolation:
--      the production Settings Defaults action still resets the live
--      bindings to the factory set AND still persists them immediately.
--      'Test.Headless.Input.Bindings' covers the YAML codec alone, and
--      the three examples in 'Test.Headless.UI.ResponsiveMenus' /
--      'Test.Headless.UI.ResponsiveGameplay' that reach @onDefaults@
--      assert resize fan-out rather than persistence — so without this
--      the write-through behavior @scripts/settings_menu.lua@ documents
--      would be an untested claim the moment the suite stopped writing
--      the developer's real file.
--
--   Reachable on its own as
--   @--match "Settings Defaults keybind persistence"@.
module Test.Headless.UI.SettingsDefaultsKeybinds (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Core.Capability.Input (InputCapability(..), toInputCapability)
import Engine.Core.Log
  (LogBackend(..), LogConfig(..), LoggerState, defaultLogConfig, initLogger)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Input.Bindings (KeyBindings, loadKeyBindings, saveKeyBindings)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import System.Directory
  ( createDirectoryLink, doesDirectoryExist, doesFileExist
  , getCurrentDirectory, listDirectory, pathIsSymbolicLink
  , withCurrentDirectory )
import System.FilePath ((</>))
import System.IO (stderr)
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation
  ( isInsideIsolatedResourceRoot, withExclusiveTempDirectory
  , withIsolatedResourceRoot )

-- | The cwd-relative path 'Engine.Scripting.Lua.API.Keybinds.saveKeybindsFn'
--   hard-codes, and therefore the exact path this issue is about.
keybindsLocalPath ∷ FilePath
keybindsLocalPath = "config" </> "keybinds.local.yaml"

-- | The tracked factory template 'loadDefaultKeybindsFn' resets from.
keybindsDefaultPath ∷ FilePath
keybindsDefaultPath = "config" </> "keybinds_default.yaml"

testLogger ∷ IO LoggerState
testLogger = initLogger defaultLogConfig { lcBackend = LogToHandle stderr }

-- | Every regular file under @dir@, as (relative path, bytes), ordered.
--   A missing directory is an empty snapshot, so "the run created
--   @config/@ where there was none" is a visible difference rather than
--   an exception.
snapshotTree ∷ FilePath → IO [(FilePath, BS.ByteString)]
snapshotTree dir = do
    present ← doesDirectoryExist dir
    if not present then pure [] else sort ⊚ go ""
  where
    go rel = do
        names ← listDirectory (dir </> rel)
        concat ⊚ forM names (\name → do
            let relPath = if null rel then name else rel </> name
            isDir ← doesDirectoryExist (dir </> relPath)
            if isDir
              then go relPath
              else do
                  bytes ← BS.readFile (dir </> relPath)
                  pure [(relPath, bytes)])

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← executeDebugLua (lbsLuaState ls) code
    when ("error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t) $
        expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t

liveBindings ∷ EngineEnv → IO KeyBindings
liveBindings env = readIORef (icKeyBindingsRef (toInputCapability env))

spec ∷ Spec
spec = describe "Settings Defaults keybind persistence (#1357)" $ do

    it "a cwd-relative production config write inside the isolated \
       \resource root leaves the checkout's own config/ byte-identical, \
       \and creates nothing in it" $ do
        srcRoot ← getCurrentDirectory
        let srcConfig = srcRoot </> "config"
        before ← snapshotTree srcConfig
        -- The exact call settings_menu.lua's Defaults action reaches
        -- through engine.saveKeybinds(), performed against the same
        -- hard-coded relative path, with a binding that is NOT the
        -- factory one — so a leak would be unmistakable in the diff.
        (wroteInScratch, scratchRoot) ← withIsolatedResourceRoot $ do
            logger ← testLogger
            saveKeyBindings logger keybindsLocalPath
                (Map.fromList [("toggleEventLog", ["F9"])])
                `shouldReturn` Right ()
            wrote ← doesFileExist keybindsLocalPath
            here  ← getCurrentDirectory
            pure (wrote, here)
        wroteInScratch `shouldBe` True
        scratchRoot `shouldNotBe` srcRoot
        after ← snapshotTree srcConfig
        after `shouldBe` before

    it "the scratch root is removed afterwards, so nothing accumulates \
       \between examples" $ do
        scratchRoot ← withIsolatedResourceRoot getCurrentDirectory
        doesDirectoryExist scratchRoot `shouldReturn` False

    -- PR #1373 review, blocker 1: a FIXED scratch path could already be
    -- occupied by a symlink, and teardown would then enumerate and
    -- recursively delete that link's target. The root is therefore
    -- created fresh and exclusively per invocation, so teardown can only
    -- ever remove a directory the fixture itself made.
    it "each invocation creates its own brand-new root directory, never \
       \adopting or reusing a path that already existed" $ do
        (firstRoot, firstWasRealDir) ← withIsolatedResourceRoot $ do
            here ← getCurrentDirectory
            link ← pathIsSymbolicLink here
            dir  ← doesDirectoryExist here
            pure (here, dir ∧ not link)
        (secondRoot, secondWasRealDir) ← withIsolatedResourceRoot $ do
            here ← getCurrentDirectory
            link ← pathIsSymbolicLink here
            dir  ← doesDirectoryExist here
            pure (here, dir ∧ not link)
        firstWasRealDir `shouldBe` True
        secondWasRealDir `shouldBe` True
        secondRoot `shouldNotBe` firstRoot
        doesDirectoryExist firstRoot `shouldReturn` False
        doesDirectoryExist secondRoot `shouldReturn` False

    -- The mechanism that keeps teardown off the checkout: every
    -- top-level entry of the scratch root but config/ is a symlink to a
    -- real checkout directory, and discarding the root must unlink those
    -- rather than walk into them.
    it "tearing the root down severs its symlinks without touching what \
       \they point at" $
        withExclusiveTempDirectory "synarchy-1357-symlink-victim" $ \victim → do
            let sentinel = victim </> "keep-me.txt"
            writeFile sentinel "untouched"
            withIsolatedResourceRoot $ do
                root ← getCurrentDirectory
                createDirectoryLink victim (root </> "linked-victim")
            doesDirectoryExist victim `shouldReturn` True
            doesFileExist sentinel `shouldReturn` True
            readFile sentinel `shouldReturn` "untouched"

    -- PR #1373 review, blocker 2: "am I isolated?" was answered by a
    -- marker FILE, so any file of that name anywhere made the real
    -- checkout look like a scratch root — which would have let the two
    -- UI suites' own guards pass while production code wrote the
    -- developer's config again. The answer now comes from fixture-owned
    -- state, so no file, in the checkout or in a lookalike directory,
    -- can forge it.
    it "no on-disk file can make a directory look like a scratch root" $ do
        inCheckout ← isInsideIsolatedResourceRoot
        inCheckout `shouldBe` False
        withExclusiveTempDirectory "synarchy-1357-lookalike-root" $ \lookalike → do
            -- Every marker name this fixture has ever used, plus the
            -- shape of a scratch root's own name.
            forM_ [ ".synarchy-isolated-resource-root"
                  , "synarchy-headless-isolated-root" ] $ \name →
                writeFile (lookalike </> name) "not a real scratch root"
            withCurrentDirectory lookalike $
                isInsideIsolatedResourceRoot `shouldReturn` False

    it "a nested call reuses the root already in effect instead of \
       \building and tearing down a second one" $ do
        (outer, inner, stillIsolated) ← withIsolatedResourceRoot $ do
            o ← getCurrentDirectory
            i ← withIsolatedResourceRoot getCurrentDirectory
            -- The nested call must not have discarded the outer root on
            -- the way out.
            s ← isInsideIsolatedResourceRoot
            pure (o, i, s)
        inner `shouldBe` outer
        stillIsolated `shouldBe` True
        doesDirectoryExist outer `shouldReturn` False

    it "production settingsMenu.onDefaults() resets the live bindings to \
       \factory AND persists them (the write-through contract #1357 must \
       \not weaken)" $
        withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
            logger  ← testLogger
            factory ← loadKeyBindings logger keybindsDefaultPath
            ls      ← newBareLuaBackend env

            -- Precondition: a live binding that is NOT the factory one,
            -- persisted, so "equals factory afterwards" cannot pass by
            -- having started there.
            accepted ← evalOk ls
                "return engine.setActionKeys('toggleEventLog', {'F9'})"
            accepted `shouldBe` "true"
            persisted ← evalOk ls "return engine.saveKeybinds()"
            persisted `shouldBe` "true"
            startLive ← liveBindings env
            Map.lookup "toggleEventLog" startLive `shouldBe` Just ["F9"]
            startDisk ← loadKeyBindings logger keybindsLocalPath
            startDisk `shouldNotBe` factory

            -- The real production entry point, with the real
            -- engine.loadDefaultKeybinds / engine.saveKeybinds behind it
            -- (nothing stubbed) — the same path the three fan-out
            -- examples drive, asserted here for what it PERSISTS.
            _ ← evalOk ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "m.onDefaults();"
                , "return true"
                ]

            endLive ← liveBindings env
            endLive `shouldBe` factory
            doesFileExist keybindsLocalPath `shouldReturn` True
            endDisk ← loadKeyBindings logger keybindsLocalPath
            endDisk `shouldBe` factory
