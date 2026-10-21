{-# LANGUAGE Strict #-}
-- | The paused scenario-load service (#2713, SCN-13 of epic #2698): one
--   engine service that turns a supported scenario source into a fully
--   initialized, PAUSED testing session, or reports honestly why it
--   could not. The contract is documented once, in
--   @docs/engine_contracts.md@ (§Scenario load).
--
--   == Shape
--
--   A scenario load IS a whole-session load transaction
--   ("Engine.Load.Status", "World.Load.Stage", "World.Load.Publish"): the
--   same request ids, the same exclusion, the same save barrier, owner
--   parking, queue cutover, allocator and page-incarnation reset, and the
--   same Lua reconciliation. What differs is the session it publishes and
--   what happens after.
--
--   1. 'requestScenarioLoad' (any thread) admits the request through the
--      shared admission boundary, pauses, and hands the request to the Lua
--      thread. A conflicting request is answered 'Busy' at once and
--      touches nothing.
--   2. 'startScenarioLoad' (Lua thread) PREFLIGHTS: the live session must
--      be a testing one, and the source must read, parse, migrate and
--      decode. Any failure ends the request with the previous session
--      exactly as it was.
--   3. The empty replacement shell (one fresh arena page at the scenario's
--      bounds, "Scenario.Load.Session") is staged privately and published
--      through the ordinary transaction. From here the previous session is
--      gone.
--   4. 'scenarioPublished' and 'continueScenarioLoad' (Lua thread, from the
--      @LuaSaveLoaded@ handler) run once the session-bound Lua callbacks
--      have been attempted. A callback failure ends the request as
--      published-but-incompletely-reconciled. Otherwise the scenario's
--      content is installed ("Scenario.Load.Construct") and only then is
--      the request ready.
--   5. Content that cannot be completed triggers RECOVERY: a fresh default
--      flat arena is published the same way, paused, and the request still
--      ends unsuccessful.
--
--   Why the content is installed after the swap instead of into a private
--   session first: the adapters commit through the live owners (the world
--   thread's install commands, the simulation seed, the units' Lua
--   readiness hook), so a private session would need a second engine
--   environment. D-43 permits the flat-arena recovery in that case.
module Scenario.Load.Service
    ( requestScenarioLoad
    , requestStartupScenarioLoad
    , startScenarioLoad
    , scenarioPublishing
    , scenarioPublished
    , continueScenarioLoad
    ) where

import UPrelude
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import qualified Control.Monad.Catch as Catch
import Data.IORef (readIORef)
import qualified Data.Text as T
import qualified HsLua as Lua
import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.InputView (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (LoggerState, logInfo, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv)
import Engine.Load.Status
import Engine.Save.Barrier (saveInProgress)
import Engine.Scripting.Lua.API.Save.Bridge (abortLuaLoad, prepareLuaLoad)
import Engine.Scripting.Lua.API.Save.Integrity (knownEntitiesFromSaveData)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))
import Scenario.Load.Catalog (liveScenarioCatalog)
import Scenario.Load.Construct
import Scenario.Load.Session
import Scenario.Load.Types
import Scenario.Schema
    (decodeScenarioBytesPaths, loadScenarioFilePaths, scenarioFormat)
import Scenario.Types
    (MapDimensions(..), Scenario(..), ScenarioOutcome(..))
import World.Arena.Bounds (mkArenaBounds)
import World.Command.Types (WorldCommand(..))
import World.Material (mergeMaterialRegistry)
import World.Pause (imposePause)
import World.Types (SaveData)

-- * Entry

-- | Ask for a scenario to be loaded. Callable from any thread.
--
--   Admission is atomic with the check of every conflicting operation
--   ('claimScenarioLoad'): while an ordinary save, an autosave, an
--   ordinary load, another scenario load or a capture is active the answer
--   is 'Busy' immediately, no identity is allocated, and the active
--   operation's status and pause state are untouched. On acceptance the
--   simulation is paused SYNCHRONOUSLY (before this returns, before any
--   decoding), the request is handed to the Lua thread, and its identity
--   is returned; the outcome is read from 'readScenarioStatus'.
--
--   A synchronous exception between acceptance and the hand-off ends the
--   request unsuccessful (the previous session is untouched) rather than
--   leaving it permanently non-terminal; a thread cancellation propagates.
requestScenarioLoad ∷ EngineEnv → ScenarioSource → IO (Either Busy Int)
requestScenarioLoad = admitScenarioLoad False

-- | The load a direct @--scenario@ boot submits (#2715): exactly
--   'requestScenarioLoad', except that the request is marked as the
--   startup load, so an unsuccessful end is latched for the boot to exit
--   on. The mark is made before the request is handed to the Lua thread,
--   hence before any stage of it can run.
requestStartupScenarioLoad ∷ EngineEnv → ScenarioSource → IO (Either Busy Int)
requestStartupScenarioLoad = admitScenarioLoad True

admitScenarioLoad ∷ Bool → EngineEnv → ScenarioSource → IO (Either Busy Int)
admitScenarioLoad startup env src = do
    let sl = toSaveLoadCapability env
        lsr = slLoadStatusRef sl
    claimed ← claimScenarioLoad lsr (saveInProgress (slSaveBarrierRef sl)) (sourceIdentity src)
    forM_ claimed $ \rid → guarded (preFailure env rid StageAdmitted) $ do
        when startup $ markScenarioStartup lsr rid
        imposePause (toWorldSimCapability env)
        advanceLoad lsr rid LoadPaused
        scenarioSeamPoint lsr PointAdmitted
        Q.writeQueue (ivLuaQueue (toInputViewCapability env)) (LuaScenarioLoad rid src)
    pure claimed

-- * The Lua thread

-- | The Lua thread's first turn on an accepted request: preflight, then
--   stage and publish the replacement shell. Everything that can fail here
--   leaves the previous session exactly as it was.
startScenarioLoad ∷ EngineEnv → LuaBackendState → Int → ScenarioSource → IO ()
startScenarioLoad env ls rid src = guarded (preFailure env rid StagePreflight) $ do
    let lsr = loadStatusOf env
    stageTo env rid StagePreflight
    scenarioSeamPoint lsr PointPreflight
    verdict ← testingSessionVerdict (toWorldSimCapability env)
    case verdict of
        Left why → preflightFailure env rid (CauseNotTestingSession why) []
        Right () → do
            catalog ← liveScenarioCatalog env
            format ← fromMaybe scenarioFormat <$> scenarioSeamFormat lsr
            (outcome, paths) ← case src of
                SourceFile path → loadScenarioFilePaths format catalog path
                SourceBytes _ bytes → pure (decodeScenarioBytesPaths format catalog bytes)
            case outcome of
                ScenarioFailed f → preflightFailure env rid (CauseSource f) []
                ScenarioLoaded scenario warnings → do
                    mapM_ (advanceLoad lsr rid) [LoadSourceSelected, LoadContentValidated]
                    updateScenarioStatus lsr rid $ \s → s { slsWarnings = warnings }
                    scenarioSeamPoint lsr PointPreflighted
                    case planOf scenario paths of
                        Left why → preflightFailure env rid (CauseSetup why) []
                        Right plan → do
                            setScenarioPlan lsr rid plan
                            stageTo env rid StageStaging
                            shell ← shellSaveData ShellSpec
                                { shBounds = plBounds plan, shCamera = scCamera scenario }
                            handed ← handoffShell env ls rid shell
                            case handed of
                                Left why → preflightFailure env rid (CauseSetup why) []
                                Right () → pure ()
  where
    planOf scenario paths = case scMap scenario of
        Nothing → Right (ScenarioPlan scenario paths Nothing)
        Just (MapDimensions w h) → case mkArenaBounds w h of
            Left e → Left ("the scenario's map is not a finite arena: " <> tshow e)
            Right ab → Right (ScenarioPlan scenario paths (Just ab))

-- | Called by the Lua thread when the staged shell reaches the publish
--   barrier: the request is now publishing. A no-op for any request that
--   is not an active scenario load.
scenarioPublishing ∷ EngineEnv → Int → IO ()
scenarioPublishing env rid =
    updateScenarioStatus (loadStatusOf env) rid $ \s →
        if slsStage s ≡ StageRecovering then s else s { slsStage = StagePublishing }

-- | Called by the @LuaSaveLoaded@ handler BEFORE its reconciliation
--   broadcast. 'Nothing' when the request is not an active scenario load;
--   otherwise the request is recorded as published, and the answer carries
--   what an armed seam threw at the publication instant.
scenarioPublished ∷ EngineEnv → Int → IO (Maybe (Maybe Text))
scenarioPublished env rid = do
    let lsr = loadStatusOf env
    current ← readScenarioStatus lsr
    case current of
        Just s | slsRequestId s ≡ rid ∧ isNothing (slsOutcome s) → do
            updateScenarioStatus lsr rid $ \r → r
                { slsPublished = True
                , slsStage = if slsStage r ≡ StageRecovering then StageRecovering
                             else StageReconciling }
            fault ← syncTry (scenarioSeamPoint lsr PointPublished)
            pure (Just (either (Just ∘ renderException) (const Nothing) fault))
        _ → pure Nothing

-- | Called by the @LuaSaveLoaded@ handler AFTER the reconciliation
--   broadcast, with what that broadcast and the publication seam reported.
--   Never called for a request 'scenarioPublished' did not claim.
continueScenarioLoad ∷ EngineEnv → LuaBackendState → Int → Maybe Text → [(Text, Text)] → IO ()
continueScenarioLoad env ls rid seamFault failures = do
    current ← readScenarioStatus (loadStatusOf env)
    case current of
        Just s | slsRequestId s ≡ rid ∧ isNothing (slsOutcome s) →
            if slsStage s ≡ StageRecovering
                then finishRecovery env rid s failures
                else guarded (postFailure env ls rid StageReconciling) $
                    case (failures, seamFault) of
                        (_ : _, _) →
                            terminateScenario (loadStatusOf env) rid
                                (ScenarioUnsuccessful (FailureContext StageReconciling
                                    (CauseReconciliation failures) (maybe [] pure seamFault)))
                                RecoveryIncomplete
                        ([], Just err) →
                            recover env ls rid (FailureContext StageReconciling (CauseException err) [])
                        ([], Nothing) → construct env ls rid
        _ → pure ()

-- * Construction and recovery

-- | Install the scenario's content and, only if every accepted entry
--   installed, declare the request ready.
construct ∷ EngineEnv → LuaBackendState → Int → IO ()
construct env ls rid = do
    let lsr = loadStatusOf env
    mplan ← takeScenarioPlan lsr rid
    case mplan of
        Nothing →
            recover env ls rid (FailureContext StageConstructing
                (CauseConstruction "the scenario plan is missing") [])
        Just plan → do
            advanceLoad lsr rid LoadInitializing
            stageTo env rid StageConstructing
            logger ← loggerOf env
            built ← syncTry $ do
                scenarioSeamPoint lsr PointReconciled
                constructScenario ConstructEnv
                    { ceEnv = env, ceLua = ls, ceLogger = logger, cePage = arenaPageId
                    , cePaths = plPaths plan
                    , ceBounds = plBounds plan, ceSeam = scenarioSeamPoint lsr }
                    (plScenario plan)
            case built of
                Left e → recover env ls rid (FailureContext StageConstructing
                            (CauseException (renderException e)) [])
                Right c → do
                    updateScenarioStatus lsr rid $ \s →
                        s { slsWarnings = slsWarnings s ⧺ ccDiagnostics c }
                    if not (null (ccFailures c))
                        then recover env ls rid (FailureContext StageConstructing
                                (CauseConstruction (T.intercalate "; " (ccFailures c))) [])
                        else do
                            -- the content is installed on the world thread, but
                            -- the simulation hears of it through its own queue:
                            -- ready means it has CONSUMED it, not that it was
                            -- queued (requirement 6)
                            wait ← scenarioSeamSimWait lsr simulationWaitSeconds
                            simulated ← syncTry (awaitSimulation (toWorldSimCapability env) wait)
                            case simulated of
                                Left e → recover env ls rid (FailureContext StageConstructing
                                            (CauseException (renderException e)) [])
                                Right (Left why) → recover env ls rid
                                    (FailureContext StageConstructing (CauseConstruction why) [])
                                Right (Right ()) → do
                                    done ← syncTry (scenarioSeamPoint lsr PointInitialized)
                                    case done of
                                        Left e → recover env ls rid (FailureContext StageConstructing
                                                    (CauseException (renderException e)) [])
                                        Right () → do
                                            logInfo logger CatWorld $
                                                "Scenario load #" <> tshow rid <> " ready"
                                            terminateScenario lsr rid ScenarioReady RecoveryNone

-- | Construction failed after the swap: publish a fresh default flat arena
--   through the same transaction. The request still ends unsuccessful; the
--   recovery only decides what stands afterwards.
recover ∷ EngineEnv → LuaBackendState → Int → FailureContext → IO ()
recover env ls rid ctx = do
    let lsr = loadStatusOf env
    logger ← loggerOf env
    logWarn logger CatWorld $
        "Scenario load #" <> tshow rid <> " failed after publication ("
        <> failureText ctx <> "); recovering to a default flat arena"
    updateScenarioStatus lsr rid $ \s →
        s { slsStage = StageRecovering, slsPendingFailure = Just ctx }
    attempt ← syncTry $ do
        scenarioSeamPoint lsr PointRecovery
        shell ← shellSaveData defaultFlatShell
        handoffShell env ls rid shell
    case attempt of
        Right (Right ()) → pure ()
        Right (Left why) → unrecovered why
        Left e → unrecovered (renderException e)
  where
    unrecovered why = do
        logger ← loggerOf env
        logWarn logger CatWorld $
            "Scenario load #" <> tshow rid <> " recovery failed: " <> why
        terminateScenario (loadStatusOf env) rid
            (ScenarioUnsuccessful ctx { fcDetails = fcDetails ctx ⧺ ["recovery failed: " <> why] })
            (RecoveryFailed why)

-- | The recovery shell was published and reconciled (or its reconciliation
--   raised): end the request with the failure it was recovering from.
finishRecovery ∷ EngineEnv → Int → ScenarioLoadStatus → [(Text, Text)] → IO ()
finishRecovery env rid s failures = do
    let ctx = fromMaybe (FailureContext StageRecovering (CauseSetup "construction failed") [])
                        (slsPendingFailure s)
    -- the flat arena stands only once the simulation consumed it too
    wait ← scenarioSeamSimWait (loadStatusOf env) simulationWaitSeconds
    simulated ← syncTry (awaitSimulation (toWorldSimCapability env) wait)
    let simProblem = case simulated of
            Left e → Just (renderException e)
            Right (Left why) → Just why
            Right (Right ()) → Nothing
    case simProblem of
      Just why | null failures → terminateScenario (loadStatusOf env) rid
          (ScenarioUnsuccessful ctx { fcDetails = fcDetails ctx ⧺ ["recovery incomplete: " <> why] })
          (RecoveryFailed why)
      _ → finishRecoveryWith env rid ctx failures

finishRecoveryWith ∷ EngineEnv → Int → FailureContext → [(Text, Text)] → IO ()
finishRecoveryWith env rid ctx failures =
    if null failures
        then terminateScenario (loadStatusOf env) rid (ScenarioUnsuccessful ctx) RecoveryFlatArena
        else do
            let why = T.intercalate "; " [ m <> ": " <> e | (m, e) ← failures ]
            terminateScenario (loadStatusOf env) rid
                (ScenarioUnsuccessful ctx
                    { fcDetails = fcDetails ctx ⧺ ["recovery reconciliation failed: " <> why] })
                (RecoveryFailed why)

-- * The staging hand-off

-- | Prepare the shell's Lua state and queue its staging on the world
--   thread. 'Left' when the Lua components could not be prepared (nothing
--   was queued and nothing needs undoing); an exception after the prepare
--   aborts it again before propagating, so Lua's registry guard is never
--   left wedged.
handoffShell ∷ EngineEnv → LuaBackendState → Int → SaveData → IO (Either Text ())
handoffShell env ls rid shell = do
    let wsc = toWorldSimCapability env
        lsr = loadStatusOf env
    logger ← loggerOf env
    baseMaterials ← loadPopulatedMaterialRegistry logger "data/materials"
    liveMaterials ← readIORef (wsMaterialRegistryRef wsc)
    let materials = mergeMaterialRegistry baseMaterials liveMaterials
        known = knownEntitiesFromSaveData shell
        abort = Lua.runWith (lbsLuaState ls) (abortLuaLoad logger rid)
    -- a fresh session has no persisted Lua state: the legacy-baseline flag
    -- makes every required module supply its own empty default
    prepared ← Lua.runWith (lbsLuaState ls) (prepareLuaLoad logger rid [] True known)
    case prepared of
        Left err → pure (Left err)
        Right _refs → (do
            scenarioSeamPoint lsr PointStagingHandoff
            Q.writeQueue (wsWorldQueue wsc) (WorldLoadTransaction rid shell materials)
            pure (Right ())) `Catch.onException` abort

-- | How long the service waits for the simulation to consume what it
--   installed. Generous: a paused simulation drains its queue in a tick, so
--   this only bounds a simulation that is not running at all.
simulationWaitSeconds ∷ Double
simulationWaitSeconds = 30

-- * Failure plumbing

loadStatusOf ∷ EngineEnv → LoadStatusRef
loadStatusOf = slLoadStatusRef ∘ toSaveLoadCapability

loggerOf ∷ EngineEnv → IO LoggerState
loggerOf env = readIORef (ccLoggerRef (toCoreCapability env))

stageTo ∷ EngineEnv → Int → ScenarioLoadStage → IO ()
stageTo env rid st = updateScenarioStatus (loadStatusOf env) rid (\s → s { slsStage = st })

renderException ∷ SomeException → Text
renderException = T.pack ∘ displayException

-- | Run @act@, returning a synchronous exception instead of raising it. A
--   thread cancellation (any 'SomeAsyncException') is NOT converted: it
--   propagates unchanged, so shutdown keeps working and an interrupted
--   request is never reported as one that failed.
syncTry ∷ IO α → IO (Either SomeException α)
syncTry act = do
    r ← try act
    case r of
        Left e | Just (ae ∷ SomeAsyncException) ← fromException e → throwIO ae
        _ → pure r

-- | The exception boundary of a handoff: a synchronous exception runs
--   @onFailure@ (which terminalizes the request) instead of escaping; an
--   asynchronous one propagates unchanged.
guarded ∷ (SomeException → IO ()) → IO () → IO ()
guarded onFailure act = syncTry act ≫= either onFailure pure

-- | End the request before the session was replaced: the previous
--   session stands.
preflightFailure ∷ EngineEnv → Int → FailureCause → [Text] → IO ()
preflightFailure env rid cause details = do
    current ← readScenarioStatus (loadStatusOf env)
    let stage = maybe StagePreflight slsStage current
    logger ← loggerOf env
    logWarn logger CatWorld $
        "Scenario load #" <> tshow rid <> " refused: "
        <> failureText (FailureContext stage cause details)
    terminateScenario (loadStatusOf env) rid
        (ScenarioUnsuccessful (FailureContext stage cause details)) RecoveryPreserved

preFailure ∷ EngineEnv → Int → ScenarioLoadStage → SomeException → IO ()
preFailure env rid stage e = do
    current ← readScenarioStatus (loadStatusOf env)
    let st = maybe stage slsStage current
    logger ← loggerOf env
    logWarn logger CatWorld $
        "Scenario load #" <> tshow rid <> " ended by an exception: " <> renderException e
    terminateScenario (loadStatusOf env) rid
        (ScenarioUnsuccessful (FailureContext st (CauseException (renderException e)) []))
        RecoveryPreserved

-- | A synchronous exception after the session was replaced: recover.
postFailure ∷ EngineEnv → LuaBackendState → Int → ScenarioLoadStage → SomeException → IO ()
postFailure env ls rid stage e =
    recover env ls rid (FailureContext stage (CauseException (renderException e)) [])
