{-# LANGUAGE Strict #-}
-- | Failure outcomes of "Scenario.Load" (#2713, D-16, D-23, D-43): every
--   handled failure is honest about what stands afterwards, leaves the
--   session paused, releases the exclusion and the barrier, and lets the
--   next request through. Faults are injected at the service's named
--   seams, never by timing.
module Test.Headless.Scenario.Load.Failure (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Building.Types (BuildingManager(..))
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
import Engine.Save.Barrier (readSaveStatus, ssOutcome, saveInProgress)
import Location.Instance (instancesToList)
import Scenario.Load.Session (arenaPageId)
import Scenario.Load.Types
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsgs)
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Types (UnitManager(..))
import Control.Concurrent (threadDelay)
import GHC.Clock (getMonotonicTime)
import Test.Headless.Harness (sendWorldCommand)
import World.Chunk.Admit (pageIncarnation)
import World.Arena.Bounds (mkArenaBounds)
import World.Command.Types (WorldCommand(..))
import World.Generate.Types (WorldGenParams(..))
import World.State.Types

spec ∷ SpecWith H
spec = describe "failure outcomes" $ do
    preSpec
    postSpec
    fenceSpec
    recoverySpec
    reconcileSpec

-- | A scenario with real content, so a half-built replacement would show.
withContent ∷ String
withContent = doc
    [ "map: {width: 40, height: 40}"
    , "terrain:"
    , "  - {id: pad, rect: {x0: -3, y0: -3, x1: 3, y1: 3}, material: granite, surface_z: 6}"
    , "buildings:"
    , "  - {id: bat, definition: high_voltage_battery, x: 14, y: 4}"
    , "units:"
    , "  - {id: a, definition: acolyte, x: 1, y: 1}"
    , "  - {id: b, definition: nomad_primitive, x: 2, y: 2}" ]

-- | The session a flat-arena recovery leaves: the default arena page, no
--   bounds, no content, paused.
flatArenaStands ∷ H → Expectation
flatArenaStands h = do
    let env = fxEnv (hFx h)
    Just ws ← activeArena h
    Just params ← readIORef (wsGenParamsRef ws)
    wgpArenaBounds params `shouldBe` Nothing
    instancesToList (wgpLocationInstances params) `shouldBe` []
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    (HM.size (umInstances um), HM.size (bmInstances bm)) `shouldBe` (0, 0)
    readIORef (enginePausedRef env) `shouldReturn` True

-- | Whatever the failure, the line is clear: no load in flight, no barrier
--   open, and a fresh request is admitted and completes.
lineIsClear ∷ H → Expectation
lineIsClear h = do
    let env = fxEnv (hFx h)
        sl = toSaveLoadCapability env
    loadInProgress (slLoadStatusRef sl) `shouldReturn` False
    saveInProgress (slSaveBarrierRef sl) `shouldReturn` False
    -- no prepared Lua load is left active: a registration guard wedged open
    -- would refuse every later module registration until restart
    -- (a world-thread staging failure reaches the Lua thread as a QUEUED abort
    -- message, so the check pumps until it has been handled)
    waitCondition 10 $ do
        processLuaMsgs (fxEnv (hFx h)) (fxLua (hFx h)) (hCtl h)
        (≡ "false") <$> lua h "return tostring(require('scripts.lib.save_modules')._loadActive)"
    src ← SourceFile <$> writeScenario "after-failure.yaml" (doc ["units: []"])
    s ← runLoad h src
    slsOutcome s `shouldBe` Just ScenarioReady

-- * Before the session is replaced

preSpec ∷ SpecWith H
preSpec = describe "before the session is replaced" $ do
    forM_ [ (PointAdmitted, StageAdmitted)
          , (PointPreflight, StagePreflight)
          , (PointPreflighted, StagePreflight)
          , (PointStagingHandoff, StageStaging)
          , (PointStaging, StageStaging)
          , (PointPublishHandoff, StageStaging)
          , (PointBarrier, StagePublishing) ] $ \(point, stage) →
        it ("preserves the previous session when a fault strikes at " ⧺ show point) $ \h → do
            src0 ← SourceFile <$> writeScenario "before.yaml" withContent
            _ ← runLoad h src0
            before ← sessionFingerprint h
            armSeams h (failAt point "injected fault")
            src ← SourceFile <$> writeScenario "faulted.yaml" withContent
            s ← runLoad h src
            disarm h
            case slsOutcome s of
                Just (ScenarioUnsuccessful ctx) → do
                    fcStage ctx `shouldBe` stage
                    show (fcCause ctx) `shouldSatisfy` T.isInfixOf "injected fault" ∘ T.pack
                other → expectationFailure ("expected an unsuccessful outcome, got " ⧺ show other)
            (slsPublished s, slsRecovery s, slsStage s) `shouldBe` (False, RecoveryPreserved, StageFailed)
            -- the previous session is exactly as it was, and still paused
            sessionFingerprint h `shouldReturn` before
            Just ls ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability (fxEnv (hFx h))))
            lsPhase ls `shouldBe` LoadFailed
            -- no barrier is left open, no prepared Lua load is left active
            readSaveStatus (slSaveBarrierRef (toSaveLoadCapability (fxEnv (hFx h))))
                >>= \st → fmap ssOutcome st `shouldNotBe` Just Nothing
            lineIsClear h

-- * After it is replaced

postSpec ∷ SpecWith H
postSpec = describe "after the session is replaced" $ do
    forM_ [ PointPublished, PointReconciled, PointTerrain, PointUnits, PointPlacement
          , PointInitialized ] $ \point →
        it ("recovers to a default flat arena, still FAILED, when a fault strikes at " ⧺ show point) $ \h → do
            armSeams h (failAt point "injected fault")
            src ← SourceFile <$> writeScenario "faulted-after.yaml" withContent
            s ← runLoad h src
            disarm h
            case slsOutcome s of
                Just (ScenarioUnsuccessful ctx) →
                    show (fcCause ctx) `shouldSatisfy` T.isInfixOf "injected fault" ∘ T.pack
                other → expectationFailure ("expected an unsuccessful outcome, got " ⧺ show other)
            -- never a success: the recovered arena is not the requested file
            (slsPublished s, slsRecovery s, slsStage s) `shouldBe` (True, RecoveryFlatArena, StageFailed)
            Just ls ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability (fxEnv (hFx h))))
            (lsPhase ls, lsFailedAtPhase ls) `shouldBe` (LoadReplacementFailed, Nothing)
            flatArenaStands h
            lineIsClear h

-- * A failure that is not an exception

-- | Accepted content that the page no longer admits is a FAILURE, not a
--   warning: the page is then not the scenario. The page is replaced
--   underneath the install (a new incarnation of the same id), so the
--   epoch fence refuses every install, and nothing an exception reports.
fenceSpec ∷ SpecWith H
fenceSpec = describe "content the page no longer admits" $
    it "is a failure with recovery, never a ready request with warnings" $ \h → do
        let env = fxEnv (hFx h)
        armSeams h $ \p → when (p ≡ PointUnits) $ do
            -- a new incarnation of the arena page: the id is reused, so only
            -- the incarnation fence stands between the install and it
            Just old ← activeArena h
            oldEpoch ← pageIncarnation old
            sendWorldCommand env (WorldInitArena arenaPageId Nothing)
            -- the command is asynchronous: wait for the NEW incarnation itself
            let replaced = do
                    Just ws ← activeArena h
                    (≢ oldEpoch) <$> pageIncarnation ws
            waitUntil 30 replaced
        src ← SourceFile <$> writeScenario "fenced.yaml" withContent
        s ← runLoad h src
        disarm h
        case slsOutcome s of
            Just (ScenarioUnsuccessful ctx) → do
                fcStage ctx `shouldBe` StageConstructing
                case fcCause ctx of
                    CauseConstruction why → why `shouldSatisfy` T.isInfixOf "could not be installed"
                    other → expectationFailure ("wrong cause: " ⧺ show other)
            other → expectationFailure ("expected an unsuccessful outcome, got " ⧺ show other)
        (slsPublished s, slsRecovery s) `shouldBe` (True, RecoveryFlatArena)
        flatArenaStands h
        lineIsClear h

-- * Recovery itself failing

recoverySpec ∷ SpecWith H
recoverySpec = describe "when recovery fails too" $ do

    it "keeps both diagnostics and never reports ready (the recovery's own fault)" $ \h → do
        armSeams h $ \p → do
            failAt PointTerrain "construction fault" p
            failAt PointRecovery "recovery fault" p
        src ← SourceFile <$> writeScenario "double-fault.yaml" withContent
        s ← runLoad h src
        disarm h
        case slsOutcome s of
            Just (ScenarioUnsuccessful ctx) → do
                show (fcCause ctx) `shouldSatisfy` T.isInfixOf "construction fault" ∘ T.pack
                fcDetails ctx `shouldSatisfy` any (T.isInfixOf "recovery fault")
            other → expectationFailure ("expected an unsuccessful outcome, got " ⧺ show other)
        case slsRecovery s of
            RecoveryFailed why → why `shouldSatisfy` T.isInfixOf "recovery fault"
            other → expectationFailure ("expected RecoveryFailed, got " ⧺ show other)
        Just ls0 ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability (fxEnv (hFx h))))
        (lsPhase ls0, lsFailedAtPhase ls0) `shouldBe` (LoadReplacementFailed, Nothing)
        readIORef (enginePausedRef (fxEnv (hFx h))) `shouldReturn` True
        lineIsClear h

    it "reports the generic transaction as replaced, not as preserved, when the recovery's publish barrier fails" $ \h → do
        hits ← newIORef (0 ∷ Int)
        armSeams h $ \p → do
            failAt PointUnits "construction fault" p
            when (p ≡ PointBarrier) $ do
                n ← atomicModifyIORef' hits (\k → (k + 1, k + 1))
                -- the SECOND barrier is the recovery's
                when (n ≡ 2) (failAt PointBarrier "recovery barrier fault" p)
        src ← SourceFile <$> writeScenario "double-fault-barrier.yaml" withContent
        s ← runLoad h src
        disarm h
        readIORef hits `shouldReturn` 2
        slsRecovery s `shouldSatisfy` (\r → case r of RecoveryFailed _ → True; _ → False)
        Just ls ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability (fxEnv (hFx h))))
        (lsPhase ls, lsFailedAtPhase ls) `shouldBe` (LoadReplacementFailed, Nothing)
        lineIsClear h

    it "keeps both diagnostics when the recovery shell fails to stage" $ \h → do
        hits ← newIORef (0 ∷ Int)
        armSeams h $ \p → do
            failAt PointUnits "construction fault" p
            when (p ≡ PointStaging) $ do
                n ← atomicModifyIORef' hits (\k → (k + 1, k + 1))
                -- the FIRST staging is the scenario shell, the SECOND the recovery's
                when (n ≡ 2) (failAt PointStaging "recovery staging fault" p)
        src ← SourceFile <$> writeScenario "double-fault-staging.yaml" withContent
        s ← runLoad h src
        disarm h
        readIORef hits `shouldReturn` 2
        case slsOutcome s of
            Just (ScenarioUnsuccessful ctx) → do
                show (fcCause ctx) `shouldSatisfy` T.isInfixOf "construction fault" ∘ T.pack
                fcDetails ctx `shouldSatisfy` any (T.isInfixOf "recovery staging fault")
            other → expectationFailure ("expected an unsuccessful outcome, got " ⧺ show other)
        slsRecovery s `shouldSatisfy` (\r → case r of RecoveryFailed _ → True; _ → False)
        -- the generic transaction says the same: the old session did NOT survive,
        -- whichever path (here the world thread's staging) reported the failure
        Just ls ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability (fxEnv (hFx h))))
        (lsPhase ls, lsFailedAtPhase ls) `shouldBe` (LoadReplacementFailed, Nothing)
        lineIsClear h

-- * Post-publication callbacks

reconcileSpec ∷ SpecWith H
reconcileSpec = describe "post-publication reconciliation" $ do

    it "attempts every callback, reports the aggregate failure, and leaves the incompletely initialized replacement paused" $ \h → do
        _ ← lua h "__marks = {}; return 'ok'"
        registerLuaModule h 71 "scripts/reconcile_alpha.lua"
            "return { onSaveLoaded = function() __marks.alpha = true; error('alpha blew up') end }"
        registerLuaModule h 72 "scripts/reconcile_beta.lua"
            "return { onSaveLoaded = function() __marks.beta = true; error('beta blew up') end }"
        registerLuaModule h 73 "scripts/reconcile_gamma.lua"
            "return { onSaveLoaded = function() __marks.gamma = true end }"
        src ← SourceFile <$> writeScenario "reconcile.yaml" withContent
        s ← runLoad h src
        -- all three ran: a failing callback does not stop the rest
        lua h "return tostring(__marks.alpha) .. tostring(__marks.beta) .. tostring(__marks.gamma)"
            `shouldReturn` "truetruetrue"
        case slsOutcome s of
            Just (ScenarioUnsuccessful ctx) → case fcCause ctx of
                CauseReconciliation fs →
                    sort' (map fst fs) `shouldBe` ["scripts/reconcile_alpha.lua", "scripts/reconcile_beta.lua"]
                other → expectationFailure ("expected a reconciliation failure, got " ⧺ show other)
            other → expectationFailure ("expected an unsuccessful outcome, got " ⧺ show other)
        -- published, incompletely initialized: not "preserved", not "recovered"
        (slsPublished s, slsRecovery s) `shouldBe` (True, RecoveryIncomplete)
        Just ls ← readLoadStatus (slLoadStatusRef (toSaveLoadCapability (fxEnv (hFx h))))
        (lsPhase ls, lsFailedAtPhase ls) `shouldBe` (LoadReconciliationFailed, Nothing)
        length (lsReconciliationFailures ls) `shouldBe` 2
        -- the replacement stands (its content was NOT installed), paused
        Just ws ← activeArena h
        Just params ← readIORef (wsGenParamsRef ws)
        wgpArenaBounds params `shouldBe` either (const Nothing) Just (mkArenaBounds 40 40)
        um ← readIORef (unitManagerRef (fxEnv (hFx h)))
        HM.size (umInstances um) `shouldBe` 0
        readIORef (enginePausedRef (fxEnv (hFx h))) `shouldReturn` True
        -- the modules are removed again so the next examples run clean
        removeModules h
        lineIsClear h
  where
    sort' = foldr insertSorted []
    insertSorted x [] = [x]
    insertSorted x (y : ys) | x ≤ y = x : y : ys
                            | otherwise = y : insertSorted x ys

removeModules ∷ H → IO ()
removeModules h = void (lua h "__marks = nil; return 'ok'") >> unregister h [71, 72, 73]

-- | Poll (bounded) until a condition holds; fail the example, never skip,
--   if it does not.
waitUntil :: Double -> IO Bool -> IO ()
waitUntil secs cond = do
    start <- getMonotonicTime
    let go = do
            ok <- cond
            unless ok $ do
                now <- getMonotonicTime
                if now - start > secs
                    then expectationFailure "the awaited condition never held"
                    else threadDelay 500 >> go
    go
