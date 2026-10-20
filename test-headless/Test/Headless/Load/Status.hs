module Test.Headless.Load.Status (spec) where

import UPrelude
import Control.Concurrent
    (forkIO, threadDelay, newEmptyMVar, putMVar, tryTakeMVar)
import GHC.Clock (getMonotonicTime)
import qualified Data.Text as T
import Test.Hspec
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Engine.Load.Status
import Engine.Save.Barrier
import Scenario.Capture.Types
import Scenario.Load.Types
import World.Thread (partitionAuthorized)
import World.Command.Types (WorldCommand(..))
import World.Page.Types (WorldPageId(..))

-- | Pure coverage for the whole-session LOAD transaction status surface
--   (issue #763, save-overhaul C2) — the load-side counterpart to
--   "Test.Headless.Save.Barrier". No engine boot required: this module
--   is a plain 'Data.IORef.IORef', exactly like the barrier's own STM
--   state, so its lifecycle is directly testable in isolation. The
--   real cross-thread publish coordination it hands off to
--   ("Engine.Save.Barrier"'s owner-quiescence protocol, reused as-is —
--   see "Engine.Scripting.Lua.Thread.Dispatch") is already covered by
--   "Test.Headless.Save.Barrier"; end-to-end coverage through a real
--   engine.loadSave / engine.getLoadStatus round trip lives in
--   tools/transactional_load_probe.py.
spec ∷ Spec
spec = describe "transactional load" $ do
    it "moves through every phase in order for a single successful load" $ do
        ref ← newLoadStatusRef
        Right n ← beginLoad ref "my_save"
        status0 ← readLoadStatus ref
        lsRequestId <$> status0 `shouldBe` Just n
        lsSaveName <$> status0 `shouldBe` Just "my_save"
        lsPhase <$> status0 `shouldBe` Just LoadRequested
        loadInProgress ref `shouldReturn` True
        mapM_ (advanceLoad ref n)
            [ LoadPaused, LoadSourceSelected, LoadEnvelopeValidated
            , LoadComponentsDecoded, LoadComponentsMigrated
            , LoadSnapshotAssembled, LoadContentValidated, LoadStaged
            , LoadWaitingPublish ]
        status1 ← readLoadStatus ref
        lsPhase <$> status1 `shouldBe` Just LoadWaitingPublish
        lsOutcome <$> status1 `shouldBe` Just Nothing
        loadInProgress ref `shouldReturn` True
        finishLoad ref n
        status2 ← readLoadStatus ref
        lsPhase <$> status2 `shouldBe` Just LoadPublished
        lsOutcome <$> status2 `shouldBe` Just (Just LoadSucceeded)
        loadInProgress ref `shouldReturn` False

    it "rejects a second load request while one is already in flight" $ do
        ref ← newLoadStatusRef
        Right _ ← beginLoad ref "first"
        beginLoad ref "second"
            `shouldReturn` Left "a load transaction is already active"

    it "a failed load reaches a terminal, non-published outcome naming\
       \ the reason, and retains the phase it failed at (round 3\
       \ review)" $ do
        ref ← newLoadStatusRef
        Right n ← beginLoad ref "broken_save"
        advanceLoad ref n LoadPaused
        advanceLoad ref n LoadSourceSelected
        advanceLoad ref n LoadEnvelopeValidated
        failLoad ref n "envelope checksum mismatch"
        status ← readLoadStatus ref
        lsPhase <$> status `shouldBe` Just LoadFailed
        lsOutcome <$> status
            `shouldBe` Just (Just (LoadAborted "envelope checksum mismatch"))
        -- 'lsPhase' itself is now the terminal 'LoadFailed' value, which
        -- on its own would lose every phase 'advanceLoad' recorded --
        -- 'lsFailedAtPhase' is what survives to say how far it got.
        lsFailedAtPhase <$> status `shouldBe` Just (Just LoadEnvelopeValidated)
        loadInProgress ref `shouldReturn` False

    it "lsFailedAtPhase stays Nothing for an in-flight or a\
       \ successfully published load" $ do
        ref ← newLoadStatusRef
        Right n ← beginLoad ref "ok_save"
        advanceLoad ref n LoadPaused
        inflight ← readLoadStatus ref
        lsFailedAtPhase <$> inflight `shouldBe` Just Nothing
        finishLoad ref n
        done ← readLoadStatus ref
        lsFailedAtPhase <$> done `shouldBe` Just Nothing

    it "a terminal load frees the slot for the next one, with a fresh,\
       \ strictly increasing id" $ do
        ref ← newLoadStatusRef
        Right first ← beginLoad ref "a"
        finishLoad ref first
        Right second ← beginLoad ref "b"
        second `shouldBe` first + 1

    it "ignores a stale signal for an id that already reached a\
       \ terminal outcome" $ do
        ref ← newLoadStatusRef
        Right n ← beginLoad ref "a"
        finishLoad ref n
        -- A late/duplicate signal for the same (now-terminal) id must
        -- not reopen or corrupt the status.
        advanceLoad ref n LoadStaged
        failLoad ref n "too late"
        status ← readLoadStatus ref
        lsPhase <$> status `shouldBe` Just LoadPublished
        lsOutcome <$> status `shouldBe` Just (Just LoadSucceeded)

    it "readLoadStatus is Nothing before any load has ever been\
       \ requested" $ do
        ref ← newLoadStatusRef
        readLoadStatus ref `shouldReturn` Nothing
        loadInProgress ref `shouldReturn` False

    -- issue #763 review round 1: a load publish must not merely DEFER
    -- every other command still queued at the captureLocked boundary —
    -- deferring it would let it survive to run again, against the
    -- REPLACEMENT session, on the world thread's very next unlocked
    -- tick (World.Thread.processAuthorizedSave). A save has no such
    -- discontinuity (the live session is the SAME session before and
    -- after), so its own deferred commands are still preserved exactly
    -- as before. WorldCommand has no Eq instance (it can carry an
    -- MVar), so these compare rendered Show text instead of the values
    -- themselves.
    describe "captureLocked authorized-command discard (requirement 12)" $ do
        it "a save keeps its non-authorized commands, deferred in order" $ do
            let hide  = WorldHide (WorldPageId "a")
                save  = WorldSave (WorldPageId "alpha") "slot" "ts" [] [] Nothing
                shw   = WorldShow (WorldPageId "b")
                (authorized, deferred) = partitionAuthorized [hide, save, shw]
            map (T.pack . show) authorized `shouldBe` [T.pack (show save)]
            map (T.pack . show) deferred
                `shouldBe` [T.pack (show hide), T.pack (show shw)]

        it "a load publish discards every other queued command instead\
           \ of deferring it" $ do
            let hide = WorldHide (WorldPageId "a")
                pub  = WorldLoadPublish 7
                shw  = WorldShow (WorldPageId "b")
                (authorized, deferred) = partitionAuthorized [hide, pub, shw]
            map (T.pack . show) authorized `shouldBe` [T.pack (show pub)]
            deferred `shouldSatisfy` null

        it "a load publish discards a stale command targeting the SAME\
           \ page id the replacement session will also use" $ do
            -- The concrete danger scenario: a stale WorldSetTime for
            -- page "alpha", still queued when the load's own
            -- WorldLoadPublish for "alpha" reaches this boundary, must
            -- not survive to mutate the freshly-published "alpha".
            let stale = WorldSetTime (WorldPageId "alpha") 23 59
                pub   = WorldLoadPublish 1
                (authorized, deferred) = partitionAuthorized [stale, pub]
            map (T.pack . show) authorized `shouldBe` [T.pack (show pub)]
            deferred `shouldSatisfy` null

        it "an empty batch authorizes and defers nothing" $ do
            let (authorized, deferred) =
                    partitionAuthorized ([] ∷ [WorldCommand])
            (null authorized, null deferred) `shouldBe` (True, True)

    -- Issue #1181: the test-only staging gate that makes
    -- tools/transactional_load_probe.py's mutual-exclusion scenario
    -- deterministic. The probe drives it end to end through a real
    -- engine; what is pinned here is the lifecycle every one of those
    -- checks depends on — above all that an UNARMED gate is completely
    -- inert, since that is what keeps every ordinary load unaffected.
    describe "test-only staging gate (issue #1181)" $ do
        it "is unarmed on a fresh ref and holds nothing" $ do
            ref ← newLoadStatusRef
            gate ← readStageGate ref
            gate `shouldBe` unarmedStageGate

        it "an unarmed gate never holds: awaitStageGate returns\
           \ immediately, reports that it did not hold, and leaves the\
           \ gate untouched" $ do
            ref ← newLoadStatusRef
            before ← readStageGate ref
            -- Timed, because "inert" here means "does not sleep": a
            -- hold that silently ran its bound would change every
            -- ordinary load's timing.
            started ← getMonotonicTime
            awaitStageGate ref 1 `shouldReturn` False
            elapsed ← subtract started <$> getMonotonicTime
            elapsed `shouldSatisfy` (< 0.5)
            readStageGate ref `shouldReturn` before

        it "an unarmed gate leaves the load status completely alone —\
           \ an ordinary load still moves through the same phase\
           \ sequence" $ do
            -- The gate sits immediately before staging in
            -- World.Thread.Command.Save.handleWorldLoadTransactionCommand,
            -- i.e. between LoadContentValidated and LoadStaged. Drive
            -- the full phase sequence with the (unarmed) gate call in
            -- exactly that position and require the observed phases to
            -- equal the untouched sequence.
            let sequence' = [ LoadPaused, LoadSourceSelected
                            , LoadEnvelopeValidated, LoadComponentsDecoded
                            , LoadComponentsMigrated, LoadSnapshotAssembled
                            , LoadContentValidated ]
            ref ← newLoadStatusRef
            Right n ← beginLoad ref "ordinary_save"
            observed ← forM sequence' $ \phase → do
                advanceLoad ref n phase
                fmap lsPhase <$> readLoadStatus ref
            beforeGate ← readLoadStatus ref
            awaitStageGate ref n `shouldReturn` False
            readLoadStatus ref `shouldReturn` beforeGate
            mapM_ (advanceLoad ref n) [LoadStaged, LoadWaitingPublish]
            finishLoad ref n
            after ← readLoadStatus ref
            observed `shouldBe` map Just sequence'
            lsPhase <$> after `shouldBe` Just LoadPublished
            lsFailedAtPhase <$> after `shouldBe` Just Nothing
            loadInProgress ref `shouldReturn` False

        it "an armed gate holds the transaction until it is released,\
           \ naming the held request id while it does" $ do
            ref ← newLoadStatusRef
            Right n ← beginLoad ref "held_save"
            advanceLoad ref n LoadContentValidated
            armStageGate ref 60
            armed ← readStageGate ref
            sgArmed armed `shouldBe` True
            sgHeldRequest armed `shouldBe` Nothing
            done ← newEmptyMVar
            _ ← forkIO $ awaitStageGate ref n >>= putMVar done
            -- The hold is positively observed by request id, which is
            -- what a black-box caller must do before racing anything
            -- against it.
            held ← pollUntil 5 $ do
                g ← readStageGate ref
                pure $ if sgHeldRequest g ≡ Just n then Just g else Nothing
            fmap sgHeldRequest held `shouldBe` Just (Just n)
            -- Still parked, and the transaction is still in flight —
            -- which is exactly what the engine's own mutual-exclusion
            -- gates key off.
            tryTakeMVar done `shouldReturn` Nothing
            loadInProgress ref `shouldReturn` True
            releaseStageGate ref
            resumed ← pollUntil 5 (tryTakeMVar done)
            resumed `shouldBe` Just True
            final ← readStageGate ref
            -- One-shot: the hold disarmed the gate on its way out, so
            -- the NEXT load in the same session is not held by a stale
            -- arm; and it ended by release, not by its bound.
            sgArmed final `shouldBe` False
            sgHeldRequest final `shouldBe` Nothing
            sgExpired final `shouldBe` False
            awaitStageGate ref (n + 1) `shouldReturn` False

        it "a hold is bounded: it self-releases on its own deadline and\
           \ says so, so an abandoned arm cannot wedge the world thread" $ do
            ref ← newLoadStatusRef
            Right n ← beginLoad ref "abandoned_save"
            -- Deliberately never released.
            armStageGate ref 0.3
            awaitStageGate ref n `shouldReturn` True
            gate ← readStageGate ref
            sgExpired gate `shouldBe` True
            sgArmed gate `shouldBe` False
            sgHeldRequest gate `shouldBe` Nothing

        it "clamps the requested hold into the engine's own bound, so\
           \ no arming mistake can hold forever" $ do
            ref ← newLoadStatusRef
            let holdFor s = armStageGate ref s >> (sgHoldSeconds <$> readStageGate ref)
            holdFor 5 `shouldReturn` 5
            holdFor (maxStageGateHold * 10) `shouldReturn` maxStageGateHold
            holdFor 0 `shouldReturn` defaultStageGateHold
            holdFor (-1) `shouldReturn` defaultStageGateHold
            holdFor (0 / 0) `shouldReturn` defaultStageGateHold
            holdFor (1 / 0) `shouldReturn` defaultStageGateHold

        it "releasing a gate nothing ever reached just disarms it, so a\
           \ test's cleanup path can call it unconditionally" $ do
            ref ← newLoadStatusRef
            armStageGate ref 60
            releaseStageGate ref
            gate ← readStageGate ref
            sgArmed gate `shouldBe` False
            awaitStageGate ref 1 `shouldReturn` False

    -- #2713: the admission boundary and the scenario request record.
    describe "scenario request record and shared admission (#2713)" $ do
        it "claimLoad refuses while a save is active and while a load is, without allocating an identity" $ do
            ref ← newLoadStatusRef
            Right first ← claimLoad ref (pure False) "a"
            claimLoad ref (pure False) "b" `shouldReturn` Left BusyLoad
            finishLoad ref first
            claimLoad ref (pure True) "c" `shouldReturn` Left BusySave
            -- the refusals allocated nothing: the next admitted request follows the first
            Right second ← claimLoad ref (pure False) "d"
            second `shouldBe` first + 1

        it "claimSave refuses while a load is active and runs its claim otherwise" $ do
            ref ← newLoadStatusRef
            Right n ← claimLoad ref (pure False) "x"
            ran ← newIORef False
            claimSave ref (writeIORef ran True) `shouldReturn` Left BusyLoad
            readIORef ran `shouldReturn` False
            finishLoad ref n
            claimSave ref (writeIORef ran True) `shouldReturn` Right ()
            readIORef ran `shouldReturn` True

        it "a scenario claim creates the request's record under the load's own identity" $ do
            ref ← newLoadStatusRef
            Right n ← claimScenarioLoad ref (pure False) "scenarios/ruin.yaml"
            Just s ← readScenarioStatus ref
            (slsRequestId s, slsSource s, slsStage s, slsOutcome s, slsPublished s)
                `shouldBe` (n, "scenarios/ruin.yaml", StageAdmitted, Nothing, False)

        it "an ordinary load supersedes the record instead of reporting it under the newer request" $ do
            ref ← newLoadStatusRef
            Right n ← claimScenarioLoad ref (pure False) "s"
            terminateScenario ref n ScenarioReady RecoveryNone
            readScenarioStatus ref >>= \m → fmap slsStage m `shouldBe` Just StageReady
            Right _ ← beginLoad ref "ordinary"
            readScenarioStatus ref `shouldReturn` Nothing

        it "ends the generic transaction by where the request stood: before the swap, after it, and as a callback failure" $ do
            let ctx = FailureContext StageConstructing (CauseSetup "boom") []
                end ref n recovery published outcome = do
                    when published $ updateScenarioStatus ref n (\r → r { slsPublished = True })
                    terminateScenario ref n outcome recovery
                    Just l ← readLoadStatus ref
                    pure (lsPhase l, lsFailedAtPhase l)
            ref1 ← newLoadStatusRef
            Right n1 ← claimScenarioLoad ref1 (pure False) "a"
            advanceLoad ref1 n1 LoadPaused
            end ref1 n1 RecoveryPreserved False (ScenarioUnsuccessful ctx)
                `shouldReturn` (LoadFailed, Just LoadPaused)
            ref2 ← newLoadStatusRef
            Right n2 ← claimScenarioLoad ref2 (pure False) "b"
            end ref2 n2 RecoveryFlatArena True (ScenarioUnsuccessful ctx)
                `shouldReturn` (LoadReplacementFailed, Nothing)
            ref3 ← newLoadStatusRef
            Right n3 ← claimScenarioLoad ref3 (pure False) "c"
            end ref3 n3 RecoveryIncomplete True
                (ScenarioUnsuccessful ctx { fcCause = CauseReconciliation [("m", "e")] })
                `shouldReturn` (LoadReconciliationFailed, Nothing)

        it "settles a record the generic transaction failed on its own, never leaving it non-terminal" $ do
            ref ← newLoadStatusRef
            Right n ← claimScenarioLoad ref (pure False) "s"
            failLoad ref n "staging blew up"
            Just s ← readScenarioStatus ref
            (slsStage s, slsRecovery s) `shouldBe` (StageFailed, RecoveryPreserved)
            fmap fcCause (case slsOutcome s of Just (ScenarioUnsuccessful c) → Just c; _ → Nothing)
                `shouldBe` Just (CauseSetup "staging blew up")

        it "ignores updates and terminations that name another request or a finished one" $ do
            ref ← newLoadStatusRef
            Right n ← claimScenarioLoad ref (pure False) "s"
            updateScenarioStatus ref (n + 1) (\r → r { slsStage = StageFailed })
            terminateScenario ref (n + 1) ScenarioReady RecoveryNone
            fmap slsStage <$> readScenarioStatus ref `shouldReturn` Just StageAdmitted
            terminateScenario ref n ScenarioReady RecoveryNone
            updateScenarioStatus ref n (\r → r { slsStage = StageFailed })
            terminateScenario ref n (ScenarioUnsuccessful (FailureContext StageReady (CauseSetup "late") [])) (RecoveryFailed "late")
            fmap slsStage <$> readScenarioStatus ref `shouldReturn` Just StageReady

    describe "scenario capture request record and shared admission (#2714)" $ do
        let req = CaptureRequest "out.yaml" False
            failed = CaptureUnsuccessful CaptureFailure
                { cfStage = CaptureAdmitted, cfKind = FailCoordination, cfPhase = Nothing
                , cfDestination = "out.yaml", cfPublication = NotPublished, cfDetail = "d"
                , cfDiagnostics = [], cfWarnings = [] }
            owners = Set.fromList [SaveLua]

        it "is admitted through the barrier's own claim, with the record under the id the barrier allocated" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right n ← claimCapture ref (beginSave barrier owners) req
            Just st ← readSaveStatus barrier
            ssRequestId st `shouldBe` n
            Just r ← readCaptureRecord ref
            (cstRequestId r, cstStage r, cstOutcome r, cstPublication r) `shouldBe` (n, CaptureAdmitted, Nothing, NotPublished)
            captureInProgress ref barrier `shouldReturn` True

        it "refuses while a load (ordinary or scenario) is active, allocating nothing and recording nothing" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right _ ← claimLoad ref (saveInProgress barrier) "slot"
            claimCapture ref (beginSave barrier owners) req `shouldReturn` Left BusyLoad
            readSaveStatus barrier `shouldReturn` Nothing
            readCaptureRecord ref `shouldReturn` Nothing

        it "refuses while a save, an autosave or a capture holds the barrier, leaving all of it as it was" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right sid ← beginSave barrier owners
            before ← readSaveStatus barrier
            claimCapture ref (beginSave barrier owners) req `shouldReturn` Left BusySave
            readSaveStatus barrier `shouldReturn` before
            readCaptureRecord ref `shouldReturn` Nothing
            finishSave barrier sid
            -- and a capture refuses a capture
            Right n ← claimCapture ref (beginSave barrier owners) req
            before' ← readSaveStatus barrier
            claimCapture ref (beginSave barrier owners) req { crDestination = "other.yaml" } `shouldReturn` Left BusySave
            readSaveStatus barrier `shouldReturn` before'
            fmap cstRequestId <$> readCaptureRecord ref `shouldReturn` Just n

        it "refuses a load while the capture's transaction is open (the load's own check reads the barrier)" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right n ← claimCapture ref (beginSave barrier owners) req
            claimLoad ref (saveInProgress barrier) "slot" `shouldReturn` Left BusySave
            claimScenarioLoad ref (saveInProgress barrier) "s" `shouldReturn` Left BusySave
            terminateCapture ref n failed
            failSave barrier n "ended"
            Right _ ← claimLoad ref (saveInProgress barrier) "slot"
            pure ()

        it "ends the record before the transaction, and holds the pause only while both are open" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right n ← claimCapture ref (beginSave barrier owners) req
            captureInProgress ref barrier `shouldReturn` True
            terminateCapture ref n failed
            Just r ← readCaptureRecord ref
            (cstStage r, cstOutcome r) `shouldBe` (CaptureFailed, Just failed)
            -- the record has ended: the pause is released even though the
            -- transaction has not been told yet
            captureInProgress ref barrier `shouldReturn` False

        it "never lets a record the barrier has moved past hold anything" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right n ← claimCapture ref (beginSave barrier owners) req
            -- the transaction ended without the record being told ...
            failSave barrier n "ended elsewhere"
            captureInProgress ref barrier `shouldReturn` False
            -- ... and a newer transaction is not mistaken for it
            Right _ ← beginSave barrier owners
            captureInProgress ref barrier `shouldReturn` False

        it "ignores updates and terminations that name another request or a finished one" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right n ← claimCapture ref (beginSave barrier owners) req
            updateCaptureStatus ref (n + 1) (\r → r { cstStage = CaptureFailed })
            terminateCapture ref (n + 1) failed
            fmap cstStage <$> readCaptureRecord ref `shouldReturn` Just CaptureAdmitted
            terminateCapture ref n (CaptureSucceeded (CaptureReport "out.yaml" 1 False [] []))
            updateCaptureStatus ref n (\r → r { cstStage = CaptureFailed })
            terminateCapture ref n failed
            fmap cstStage <$> readCaptureRecord ref `shouldReturn` Just CaptureDone

        it "reports the publication the moment it is recorded, and on every unsuccessful end" $ do
            ref ← newLoadStatusRef
            barrier ← newSaveBarrier
            Right n ← claimCapture ref (beginSave barrier owners) req
            updateCaptureStatus ref n (\r → r { cstPublication = Published })
            fmap cstPublication <$> readCaptureRecord ref `shouldReturn` Just Published
            terminateCapture ref n failed
            -- the failure the request ends with is the one that says whether it was published
            fmap cstPublication <$> readCaptureRecord ref `shouldReturn` Just NotPublished

        it "serializes the seams: an unarmed seam is inert, and an armed one is consulted at its point" $ do
            ref ← newLoadStatusRef
            captureSeamPoint ref CpAdmitted
            hits ← newIORef []
            armCaptureSeams ref noCaptureSeams { cseamAt = \p → modifyIORef' hits (p :) }
            mapM_ (captureSeamPoint ref) [CpPreflight, CpEncoded]
            readIORef hits `shouldReturn` [CpEncoded, CpPreflight]
            disarmCaptureSeams ref
            captureSeamPoint ref CpReleased
            readIORef hits `shouldReturn` [CpEncoded, CpPreflight]

-- | Poll @act@ every 20 ms for up to @secs@ seconds, returning its
--   first 'Just'. Keeps the gate tests off fixed sleeps for the same
--   reason the probe itself is being changed.
pollUntil ∷ Double → IO (Maybe a) → IO (Maybe a)
pollUntil secs act = do
    deadline ← (+ secs) <$> getMonotonicTime
    let loop = do
            r ← act
            case r of
                Just _ → pure r
                Nothing → do
                    now ← getMonotonicTime
                    if now ≥ deadline
                      then pure Nothing
                      else threadDelay 20000 >> loop
    loop
