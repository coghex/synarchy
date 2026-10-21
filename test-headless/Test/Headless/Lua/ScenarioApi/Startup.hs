{-# LANGUAGE OverloadedStrings #-}
-- | The startup half of "Lua scenario API" (#2715): what a direct
--   @--scenario@ boot is made of, below the process.
--
--   * the three startup verbs (@engine.getScenarioStartup@,
--     @engine.startScenarioStartup@, @engine.failScenarioStartup@) and the
--     rule that only a boot that asked for a scenario may use them;
--   * the startup-failure LATCH: whatever way the startup load ends
--     unsuccessfully — a source that cannot be read, a failed migration, a
--     construction failure whose recovery published a flat arena, an
--     incomplete reconciliation — the cause is latched before the
--     operation exclusion releases, the first cause stands, a recovered
--     flat arena or a later success does not clear it, and an ordinary
--     (non-startup) load never latches;
--   * 'checkScenarioStartup', the loop's reading of the latch: a fatal
--     engine error in headless and offscreen only, and only when the boot
--     asked for a scenario;
--   * the REAL startup driver (@scripts/scenario_startup.lua@) against a
--     stand-in startup loader: headless drives the definitions itself,
--     graphical and offscreen never drain the queue a second time, the
--     load is submitted exactly once, and a definition failure becomes a
--     startup failure.
--
--   The processes (exit codes, the fatal tail's trace) are
--   @tools/scenario_entrypoint_probe.py@'s.
module Test.Headless.Lua.ScenarioApi.Startup (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (putMVar, takeMVar)
import qualified Data.Text as T
import qualified HsLua as Lua
import System.Directory (removeFile)
import System.FilePath ((</>))
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Error.Exception (EngineException(..), ExceptionType(..), SystemError(..))
import Engine.Core.Monad (runEngineM)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Types (BootMode(..), EngineConfig(..))
import Engine.Load.Status
import Engine.Loop.Mode (checkScenarioStartup)
import Engine.Scripting.Lua.API.Register.Engine (registerEngineAPIWith)
import Engine.Scripting.Lua.API.Scenario (ScenarioApi(..))
import Engine.Scripting.Lua.CallStats (newLuaCallStats)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Scenario.Load.Types
import Scenario.Path (ScenarioRoots(..))
import Scenario.Schema (ScenarioFormat(..), scenarioFormat)
import Test.Headless.Lua.ScenarioApi.Support
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))

spec ∷ Spec
spec = describe "Lua scenario API, startup" $ aroundAll withApi $ do
    verbSpec
    latchSpec
    loopSpec
    driverSpec

lsrOf ∷ Api → LoadStatusRef
lsrOf = slLoadStatusRef ∘ toSaveLoadCapability ∘ fxEnv ∘ hFx ∘ aH

-- | Re-register the verbs over an engine whose boot configuration says what
--   the example needs. The refs are shared, so the services see one engine.
configure ∷ Api → BootMode → Maybe FilePath → IO EngineEnv
configure a mode path = do
    let env = fxEnv (hFx (aH a))
        env' = env { engineConfig = (engineConfig env)
                       { ecScenarioStartup = path, ecBootMode = mode } }
    stats ← newLuaCallStats
    Lua.runWith (lbsLuaState (fxLua (hFx (aH a)))) $
        registerEngineAPIWith (ScenarioApi (pure (ScenarioRoots (aLaunch a) (aRoot a)))) stats env' (fxLua (hFx (aH a)))
    pure env'

-- | Submit the startup load through the verb and pump it to its end.
startup ∷ Api → IO ScenarioLoadStatus
startup a = do
    n ← loadStartupId a
    pumpUntilTerminal (aH a) n 60

loadStartupId ∷ Api → IO Int
loadStartupId a = do
    r ← call a "local id, why = engine.startScenarioStartup() return tostring(id) .. '|' .. tostring(why and why.reason)"
    case readInt (fst (T.breakOn "|" r)) of
        Just n → pure n
        Nothing → fail ("the startup load was not accepted: " ⧺ T.unpack r)

latched ∷ Api → IO (Maybe Text)
latched = readStartupFailure ∘ lsrOf

-- * The verbs

verbSpec ∷ SpecWith Api
verbSpec = describe "the startup verbs" $ do

    it "refuse everything when the boot asked for no scenario" $ \a → do
        _ ← configure a ModeHeadless Nothing
        resetStartupState (lsrOf a)
        before ← observed a
        call a "return tostring(engine.getScenarioStartup())" `shouldReturn` "nil"
        call a "local id, why = engine.startScenarioStartup() return tostring(id) .. '|' .. why.reason"
            `shouldReturn` "nil|invalid_path"
        call a "local ok, why = engine.failScenarioStartup('boom') return tostring(ok) .. '|' .. why.reason"
            `shouldReturn` "nil|invalid_path"
        latched a `shouldReturn` Nothing
        observed a `shouldReturn` before

    it "keep a boot to ONE startup load: a second submission is refused and queues nothing" $ \a → do
        let file = aLaunch a </> "once.yaml"
        writeFile file (doc ["units: []"])
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        n ← loadStartupId a
        _ ← pumpUntilTerminal (aH a) n 60
        before ← observed a
        call a "local id, why = engine.startScenarioStartup() return tostring(id) .. '|' .. why.reason"
            `shouldReturn` "nil|already_started"
        observed a `shouldReturn` before

    it "give the claim back when the line is busy, so the caller can ask again" $ \a → do
        let file = aLaunch a </> "retry.yaml"
        writeFile file (doc ["units: []"])
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        _ ← put a "holding.yaml" (doc ["units: []"])
        hold ← newHold
        armSeams (aH a) (holdAt PointPreflight hold)
        held ← loadViaLua a "'holding'"
        pump ← startPump (aH a)
        takeMVar (holdReached hold)
        call a "local id, why = engine.startScenarioStartup() return tostring(id) .. '|' .. why.reason"
            `shouldReturn` "nil|busy"
        putMVar (holdRelease hold) ()
        _ ← await (aH a) held 60
        stopPump pump
        disarm (aH a)
        n ← loadStartupId a
        n `shouldBe` held + 1
        s ← pumpUntilTerminal (aH a) n 60
        slsOutcome s `shouldBe` Just ScenarioReady

    it "refuse a wrong argument count and a non-string message" $ \a → do
        _ ← configure a ModeHeadless (Just (aLaunch a </> "x.yaml"))
        raises a "engine.getScenarioStartup(1)" "takes 0 argument(s)"
        raises a "engine.startScenarioStartup(1)" "takes 0 argument(s)"
        raises a "engine.failScenarioStartup()" "takes 1 argument(s)"
        raises a "engine.failScenarioStartup(5)" "message must be a string"
        latched a `shouldReturn` Nothing

    it "report the configured file verbatim as data, and who loads the definitions" $ \a → do
        let path = aLaunch a </> "ünï \"q\" 's' ]] x.yaml"
            lit = "[==[" <> T.pack path <> "]==]"
        forM_ [(ModeHeadless, "true"), (ModeOffscreen, "false"), (ModeGraphical, "false")] $ \(mode, loads) → do
            _ ← configure a mode (Just path)
            call a ("local s = engine.getScenarioStartup() return tostring(s.path == " <> lit
                    <> ") .. '|' .. tostring(s.loadsDefinitions)")
                `shouldReturn` ("true|" <> loads)

-- * The latch

latchSpec ∷ SpecWith Api
latchSpec = describe "the startup-failure latch" $ do

    it "is never written by an ordinary load, whatever way it fails" $ \a → do
        _ ← configure a ModeHeadless (Just (aLaunch a </> "unused.yaml"))
        resetStartupState (lsrOf a)
        _ ← put a "ordinary-bad.yaml" "version: 3\nunits: [unclosed\n"
        n ← loadViaLua a "'ordinary-bad'"
        s ← pumpUntilTerminal (aH a) n 60
        fmap isUnsuccessful (slsOutcome s) `shouldBe` Just True
        latched a `shouldReturn` Nothing

    it "is written when the startup source cannot be read, naming the file and the cause" $ \a → do
        let file = aLaunch a </> "gone.yaml"
        writeFile file "version: 3\n"
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        removeFile file
        s ← startup a
        slsRecovery s `shouldBe` RecoveryPreserved
        cause ← latched a
        cause `shouldSatisfy` mentions [T.pack file, "ScenarioUnreadable", "previous session stands"]

    it "is written for invalid YAML, a non-mapping, and an unsupported version" $ \a → do
        forM_ [ ("syntax.yaml", "version: 3\nunits: [unclosed\n", "ScenarioSyntaxError")
              , ("list.yaml", "- 1\n- 2\n", "ScenarioNotAMapping")
              , ("future.yaml", "version: 99\n", "ScenarioVersionUnsupported 99") ] $ \(name, text, needle) → do
            let file = aLaunch a </> name
            writeFile file text
            _ ← configure a ModeHeadless (Just file)
            resetStartupState (lsrOf a)
            _ ← startup a
            latched a >>= (`shouldSatisfy` mentions [T.pack file, needle])

    it "is written for a failed migration" $ \a → do
        let file = aLaunch a </> "migrating.yaml"
        writeFile file "version: 1\n"
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        armSeamsWith (aH a) (\_ → pure ())
            (Just scenarioFormat { sfMigrations = [(1, \_ → Left "step 1 cannot be applied"), (2, Right)] })
        _ ← startup a
        disarm (aH a)
        latched a >>= (`shouldSatisfy` mentions ["ScenarioMigrationFailed", "step 1 cannot be applied"])

    it "is written for a construction failure, and a flat arena that recovery published does not clear it" $ \a → do
        let file = aLaunch a </> "constructs.yaml"
        writeFile file (doc ["units: []"])
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        armSeams (aH a) (failAt PointUnits "injected construction fault")
        s ← startup a
        disarm (aH a)
        slsRecovery s `shouldBe` RecoveryFlatArena
        latched a >>= (`shouldSatisfy` mentions
            ["injected construction fault", "a default flat arena stands, NOT the requested scenario"])
        -- a later good load neither clears nor replaces it
        _ ← put a "after-flat.yaml" (doc ["units: []"])
        n ← loadViaLua a "'after-flat'"
        _ ← pumpUntilTerminal (aH a) n 60
        latched a >>= (`shouldSatisfy` mentions ["injected construction fault"])

    it "is written for an incomplete reconciliation" $ \a → do
        let file = aLaunch a </> "reconciles.yaml"
        writeFile file (doc ["units: []"])
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        registerLuaModule (aH a) 91 "scripts/startup_reconcile_fixture.lua"
            "return { onSaveLoaded = function() error('fixture reconcile fault') end }"
        s ← startup a
        unregister (aH a) [91]
        slsRecovery s `shouldBe` RecoveryIncomplete
        latched a >>= (`shouldSatisfy` mentions
            ["fixture reconcile fault", "the replacement session is incompletely initialized"])

    it "is not written by a startup load that succeeds, and a later failing ordinary load does not write it" $ \a → do
        let file = aLaunch a </> "fine.yaml"
        writeFile file (doc ["units: []"])
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        s ← startup a
        slsOutcome s `shouldBe` Just ScenarioReady
        latched a `shouldReturn` Nothing
        n ← loadViaLua a "'ordinary-bad'"
        _ ← pumpUntilTerminal (aH a) n 60
        latched a `shouldReturn` Nothing

    it "keeps the FIRST cause, whoever reports a second" $ \a → do
        let file = aLaunch a </> "first.yaml"
        writeFile file "- not a mapping\n"
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        _ ← startup a
        call a "return tostring(engine.failScenarioStartup('a later definition failure'))" `shouldReturn` "true"
        latched a >>= (`shouldSatisfy` mentions ["ScenarioNotAMapping"])
        latched a >>= (`shouldSatisfy` (maybe False (not ∘ T.isInfixOf "later definition failure")))

    it "is written by the driver's own report, naming the file and the cause" $ \a → do
        let file = aLaunch a </> "driver-report.yaml"
        _ ← configure a ModeHeadless (Just file)
        resetStartupState (lsrOf a)
        call a "return tostring(engine.failScenarioStartup('Startup failed: materials could not parse x.yaml'))"
            `shouldReturn` "true"
        latched a >>= (`shouldSatisfy` mentions [T.pack file, "Startup failed: materials could not parse x.yaml"])
  where
    isUnsuccessful = \case ScenarioUnsuccessful _ → True; _ → False

mentions ∷ [Text] → Maybe Text → Bool
mentions needles = maybe False (\t → all (`T.isInfixOf` t) needles)

-- * The loop's reading of the latch

loopSpec ∷ SpecWith Api
loopSpec = describe "checkScenarioStartup" $ do

    let verdict a mode path = do
            env' ← configure a mode path
            runEngineM (checkScenarioStartup env') env' pure
        latchedFault a = recordStartupFailure (lsrOf a) "scenario startup failed for x.yaml: boom"

    it "raises the latched cause as a fatal engine error in headless and in offscreen" $ \a → do
        forM_ [ModeHeadless, ModeOffscreen] $ \mode → do
            resetStartupState (lsrOf a)
            latchedFault a
            r ← verdict a mode (Just (aLaunch a </> "x.yaml"))
            case r of
                Left ex → do
                    errorMsg ex `shouldBe` "scenario startup failed for x.yaml: boom"
                    errorType ex `shouldBe` ExSystem (StartupError "scenario startup failed for x.yaml: boom")
                Right () → expectationFailure ("no fatal error in " ⧺ show mode)

    it "does nothing in graphical, which keeps its paused recovery controls" $ \a → do
        resetStartupState (lsrOf a)
        latchedFault a
        r ← verdict a ModeGraphical (Just (aLaunch a </> "x.yaml"))
        r `shouldSatisfy` isRight'

    it "does nothing when the boot asked for no scenario, even with a latch set" $ \a → do
        resetStartupState (lsrOf a)
        latchedFault a
        forM_ [ModeHeadless, ModeOffscreen, ModeGraphical] $ \mode →
            verdict a mode Nothing >>= (`shouldSatisfy` isRight')

    it "does nothing while nothing has failed" $ \a → do
        resetStartupState (lsrOf a)
        forM_ [ModeHeadless, ModeOffscreen] $ \mode →
            verdict a mode (Just (aLaunch a </> "x.yaml")) >>= (`shouldSatisfy` isRight')
  where isRight' = either (const False) (const True)

-- * The startup driver, as written

-- | The real @scripts/scenario_startup.lua@ over a stand-in startup loader
--   that counts what it is asked, and stand-in UI modules that record what
--   they are asked to show.
driverSpec ∷ SpecWith Api
driverSpec = describe "the startup driver (scripts/scenario_startup.lua)" $ do

    let install a loaderBehaviour = do
            _ ← call a $ T.concat
                [ "__calls = {} "
                , "local stub = {built = false, done = false, failed = false} "
                , "function stub.build(p) __calls[#__calls + 1] = 'build:' .. tostring(p) stub.built = true end "
                , "function stub.runAll() __calls[#__calls + 1] = 'runAll' " <> loaderBehaviour <> " end "
                , "function stub.isFailed() return stub.failed end "
                , "function stub.getFailure() return stub.failure end "
                , "function stub.isDone() return stub.done end "
                , "package.loaded['scripts.startup_loader'] = stub "
                , "package.loaded['scripts.ui_manager'] = {showMenu = function(m) __calls[#__calls + 1] = 'show:' .. m end} "
                , "package.loaded['scripts.test_arena'] = {} "
                , "package.loaded['scripts.scenario_startup'] = nil "
                , "_G.__driver = require('scripts.scenario_startup') "
                , "_G.__driver.phase = 'off' _G.__driver.id = nil "
                , "_G.__driver.init(1) return 'ok'" ]
            pure ()
        ticks a n = forM_ [1 .. n ∷ Int] $ \_ → call a "__driver.update(0.05) return 'ok'"
        calls a = call a "return table.concat(__calls, ',')"
        file a = aLaunch a </> "driven.yaml"

    it "does nothing at all when the boot asked for no scenario" $ \a → do
        _ ← configure a ModeHeadless Nothing
        install a "stub.done = true"
        ticks a 5
        calls a `shouldReturn` ""
        call a "return __driver.phase" `shouldReturn` "off"

    it "headless: drives the arena definitions ITSELF, once, then submits exactly one load and follows it" $ \a → do
        writeFile (file a) (doc ["units: []"])
        _ ← configure a ModeHeadless (Just (file a))
        resetStartupState (lsrOf a)
        install a "stub.done = true"
        ticks a 1
        calls a `shouldReturn` "build:arena,runAll"
        ticks a 1                                   -- submit
        n ← currentLoadId a
        ticks a 6                                   -- more ticks must not submit again
        currentLoadId a `shouldReturn` n
        _ ← pumpUntilTerminal (aH a) n 60
        ticks a 3
        call a "return __driver.phase .. '|' .. __driver.outcome" `shouldReturn` "done|ready"
        -- headless has no UI to show
        calls a `shouldReturn` "build:arena,runAll"
        latched a `shouldReturn` Nothing

    it "offscreen and graphical: wait for the UI boot's drain, never draining the queue a second time" $ \a → do
        forM_ [ModeOffscreen, ModeGraphical] $ \mode → do
            writeFile (file a) (doc ["units: []"])
            _ ← configure a mode (Just (file a))
            resetStartupState (lsrOf a)
            install a "stub.done = true"
            call a "package.loaded['scripts.startup_loader'].built = false return 'ok'" `shouldReturn` "ok"
            ticks a 4
            calls a `shouldReturn` ""               -- not built, not drained, nothing submitted
            call a "return __driver.phase" `shouldReturn` "definitions"
            -- the UI boot builds and drains the queue (as ui_manager_boot does)
            call a "local s = package.loaded['scripts.startup_loader'] s.built = true s.done = true return 'ok'"
                `shouldReturn` "ok"
            ticks a 2
            n ← currentLoadId a
            _ ← pumpUntilTerminal (aH a) n 60
            ticks a 2
            call a "return __driver.outcome" `shouldReturn` "ready"
            -- never built or drained by the driver; the arena view shown once the scenario stands
            calls a `shouldReturn` "show:test_arena_view"

    it "a busy submission is retried on the next tick, not treated as a failure" $ \a → do
        writeFile (file a) (doc ["units: []"])
        _ ← configure a ModeHeadless (Just (file a))
        resetStartupState (lsrOf a)
        install a "stub.done = true"
        _ ← put a "driver-holding.yaml" (doc ["units: []"])
        hold ← newHold
        armSeams (aH a) (holdAt PointPreflight hold)
        held ← loadViaLua a "'driver-holding'"
        pump ← startPump (aH a)
        takeMVar (holdReached hold)
        ticks a 4                                   -- definitions, then busy submits
        call a "return __driver.phase" `shouldReturn` "submit"
        latched a `shouldReturn` Nothing
        putMVar (holdRelease hold) ()
        _ ← await (aH a) held 60
        stopPump pump
        disarm (aH a)
        ticks a 1
        n ← currentLoadId a
        n `shouldBe` held + 1
        _ ← pumpUntilTerminal (aH a) n 60
        ticks a 2
        call a "return __driver.outcome" `shouldReturn` "ready"

    it "a definition failure becomes a startup failure and nothing is submitted" $ \a → do
        writeFile (file a) (doc ["units: []"])
        _ ← configure a ModeHeadless (Just (file a))
        resetStartupState (lsrOf a)
        install a "stub.failed = true stub.failure = {message = 'Startup failed: materials could not parse bad.yaml'}"
        before ← observed a
        ticks a 4
        call a "return __driver.phase" `shouldReturn` "failed"
        latched a >>= (`shouldSatisfy` mentions ["Startup failed: materials could not parse bad.yaml", T.pack (file a)])
        observed a `shouldReturn` before

    it "graphical: a failed scenario still shows the session that stands, and the driver never exits the process" $ \a → do
        let bad = aLaunch a </> "driven-bad.yaml"
        writeFile bad "version: 3\nunits: [unclosed\n"
        _ ← configure a ModeGraphical (Just bad)
        resetStartupState (lsrOf a)
        install a "stub.done = true"
        call a "local s = package.loaded['scripts.startup_loader'] s.built = true s.done = true return 'ok'"
            `shouldReturn` "ok"
        ticks a 2
        n ← currentLoadId a
        s ← pumpUntilTerminal (aH a) n 60
        fmap (\case ScenarioUnsuccessful _ → True; _ → False) (slsOutcome s) `shouldBe` Just True
        ticks a 2
        call a "return __driver.outcome" `shouldReturn` "failed"
        -- a session stands (the fixture's), so it is the one shown; the
        -- driver never exits the process, and the graphical loop ignores the latch
        calls a `shouldReturn` "show:test_arena_view"
        latched a >>= (`shouldSatisfy` isJust)
  where
    currentLoadId a = do
        st ← call a "local s = engine.getScenarioStatus() return tostring(s and s.id)"
        maybe (fail ("no scenario operation: " ⧺ T.unpack st)) pure (readInt st)
