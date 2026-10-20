{-# LANGUAGE Strict #-}

-- | Diagnostic / coordination state for a whole-session LOAD transaction
--   (issue #763, save-overhaul C2) — the load-side counterpart to
--   "Engine.Save.Barrier". Unlike the save barrier, this module owns no
--   cross-thread owner-quiescence protocol of its own: staging (source
--   selection, envelope/component decode, migration, snapshot assembly,
--   content validation, runtime reconstruction) touches no live engine
--   state at all (requirement 6), so nothing needs to pause for it. The
--   brief PUBLISH step, where live state genuinely must be protected
--   from concurrent mutation, reuses "Engine.Save.Barrier"'s existing
--   'Engine.Save.Barrier.SaveOwner' quiescence protocol directly (see
--   "Engine.Scripting.Lua.Thread.Dispatch") rather than duplicating it —
--   this module exists only to expose the finer-grained phase sequence a
--   load moves through (requirement 2) and to give 'engine.loadSave' /
--   'engine.saveWorld' a single, simple "is a load already in flight"
--   gate so the two transaction kinds never overlap (requirement 1: a
--   save request is rejected for the WHOLE load duration, not just its
--   brief publish window, since 'Engine.Save.Barrier' itself is only
--   engaged right before publish).
--
--   There is at most one non-terminal load transaction at a time
--   (enforced by 'beginLoad'), and the phases below are driven by a
--   strict happens-before chain of message-passing between the Lua
--   thread (requirements 1-8, then again 10-12/failure) and the world
--   thread (requirement 9) — never two threads racing to mutate the same
--   transaction concurrently — so plain 'IORef' operations (not STM) are
--   sufficient; concurrent READERS (e.g. 'engine.getLoadStatus' polling
--   from the debug console while a load is in flight) are always safe
--   since a single 'IORef' read/write is never torn.
module Engine.Load.Status
    ( LoadPhase(..), LoadOutcome(..), LoadStatus(..)
    , ReconciliationFailure(..)
    , LoadStatusRef, newLoadStatusRef
    , beginLoad, advanceLoad, failLoad, finishLoad, failReconciliation
    , failPublishedLoad
    , readLoadStatus, loadInProgress
      -- * Admission (#2713): the one atomic check-and-claim
    , withAdmission, claimLoad, claimSave
      -- * The scenario request record (#2713)
    , claimScenarioLoad, readScenarioStatus, updateScenarioStatus
    , terminateScenario
    , setScenarioPlan, takeScenarioPlan, readScenarioPlan
      -- * The scenario capture request record (#2714)
    , claimCapture, readCaptureRecord, updateCaptureStatus, terminateCapture
    , captureInProgress
    , armCaptureSeams, disarmCaptureSeams, captureSeamPoint, captureSeamOps
    , captureSeamTransforms
    , armScenarioSeams, disarmScenarioSeams, scenarioSeamPoint, scenarioSeamFormat
    , scenarioSeamSimWait
    , StageGate(..), unarmedStageGate
    , defaultStageGateHold, maxStageGateHold
    , armStageGate, releaseStageGate, readStageGate, awaitStageGate
    ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Engine.Save.Barrier (SaveBarrier, SaveStatus(..), readSaveStatus)
import Scenario.Capture.Types
import Scenario.Load.Types
import Scenario.Schema (ScenarioFormat)
import Scenario.Types (Scenario)

-- | The 12 phases requirement 2 lists, in the order a successful load
--   moves through them, plus the 13th 'LoadReconciliationFailed' added
--   by issue #1204. 'LoadFailed' is terminal and reachable from any
--   non-terminal phase; 'LoadPublished' and 'LoadReconciliationFailed'
--   are the other two terminal phases, both reachable only from
--   'LoadWaitingPublish' (they are the two ways the post-publication
--   Lua reconciliation broadcast can end).
data LoadPhase
    = LoadRequested
    | LoadPaused
    | LoadSourceSelected
    | LoadEnvelopeValidated
    | LoadComponentsDecoded
    | LoadComponentsMigrated
    | LoadSnapshotAssembled
    | LoadContentValidated
    | LoadStaged
    | LoadWaitingPublish
    | LoadPublished
    | LoadFailed
    | LoadReconciliationFailed
      -- ^ Issue #1204: publication SUCCEEDED (the session swap and
      --   barrier release already happened — this phase is only
      --   reachable past them) but at least one Lua @onSaveLoaded@
      --   reconciliation callback raised, so the live session is
      --   incompletely reconciled. Deliberately NOT 'LoadPublished'
      --   (which every consumer treats as an unqualified success) and
      --   deliberately NOT 'LoadFailed', whose
      --   @docs/persistence_contract.md@ promise that a failed load
      --   leaves the old session unchanged this state cannot honor.
    | LoadInitializing
      -- ^ Issue #2713: a SCENARIO load whose replacement shell is
      --   published and reconciled and whose content is now being
      --   installed. Non-terminal: the request is not ready until the
      --   content is complete.
    | LoadReplacementFailed
      -- ^ Issue #2713: a scenario load whose replacement was published
      --   and then could not be completed. Terminal like
      --   'LoadReconciliationFailed', and for the same reason neither
      --   'LoadFailed' (the old session did NOT survive) nor
      --   'LoadPublished' (the requested scenario is NOT what stands).
      --   What stands instead is the scenario record's 'Recovery'.
    deriving (Eq, Show, Enum, Bounded)

-- | One Lua @onSaveLoaded@ callback that raised, and the error text it
--   raised with (issue #1204). 'rfModule' is the failing module's
--   'Engine.Scripting.Lua.Types.scriptPath', which is what makes the
--   module-to-error association unambiguous even when several modules
--   fail with similar-looking messages — a single flattened string
--   cannot promise that, since a Lua error message may itself contain
--   whatever separator the flattening picked.
data ReconciliationFailure = ReconciliationFailure
    { rfModule ∷ !Text
    , rfError  ∷ !Text
    } deriving (Eq, Show)

-- | Human-readable one-line summary of a reconciliation failure set,
--   used as the 'LoadReconciliationIncomplete' payload. The structured
--   'lsReconciliationFailures' list — not this string — is the
--   authoritative module-to-error association.
renderReconciliationFailures ∷ [ReconciliationFailure] → Text
renderReconciliationFailures fs =
    T.intercalate "; " [ rfModule f <> ": " <> rfError f | f ← fs ]

data LoadOutcome
    = LoadSucceeded
    | LoadAborted !Text
    | LoadReconciliationIncomplete !Text
      -- ^ Issue #1204: the terminal outcome paired with
      --   'LoadReconciliationFailed'. Non-'Nothing' like every other
      --   outcome, so 'loadInProgress' reports the transaction as over
      --   and a subsequent save/load request is not blocked forever.
    deriving (Eq, Show)

data LoadStatus = LoadStatus
    { lsRequestId ∷ !Int
    , lsSaveName  ∷ !Text
    , lsPhase     ∷ !LoadPhase
    , lsOutcome   ∷ !(Maybe LoadOutcome)
    , lsReconciliationFailures ∷ ![ReconciliationFailure]
        -- ^ Issue #1204: every Lua @onSaveLoaded@ callback that raised
        --   during this load's post-publication reconciliation
        --   broadcast, in broadcast order. Empty for a load that is
        --   still in flight, that failed before publication, or whose
        --   reconciliation completed cleanly — so a non-empty list is
        --   exactly the 'LoadReconciliationFailed' state.
    , lsFailedAtPhase ∷ !(Maybe LoadPhase)
        -- ^ 'failLoad' unconditionally overwrites
        --   'lsPhase' to the terminal 'LoadFailed' value, so whatever
        --   phase the preceding 'advanceLoad' calls last recorded would
        --   otherwise be lost the instant a load fails — defeating the
        --   whole point of reporting real progress on failure. This
        --   field captures that last-reached phase AT the moment of
        --   failure and is never touched by 'advanceLoad'/'finishLoad',
        --   so it survives 'lsPhase' becoming 'LoadFailed'. 'Nothing'
        --   before any load has failed, or for a load that is still in
        --   flight or that published successfully. Issue #1204 keeps it
        --   reserved for PRE-publication 'failLoad' attempts:
        --   'failReconciliation' never populates it, since its presence
        --   is what tells a consumer the old session was left unchanged
        --   — which a post-publication failure cannot claim.
    } deriving (Eq, Show)

-- | Issue #1181: the TEST-ONLY staging gate. A load transaction's one
--   long, observable in-flight window is world-thread staging, and it
--   is exactly as long as generating the saved world takes — so a
--   black-box probe racing 'engine.saveWorld' / a second
--   'engine.loadSave' against it wins or loses on machine speed alone,
--   which is what let @tools/transactional_load_probe.py@ SKIP every
--   mutual-exclusion check and still exit 0.
--
--   Armed (only ever by @debug.armLoadStageGate@ — see
--   "Engine.Scripting.Lua.API.LoadGate"), the next load to reach
--   'World.Thread.Command.Save.handleWorldLoadTransactionCommand' parks
--   at 'awaitStageGate' BEFORE staging begins and stays there until the
--   arming caller releases it or the hold bound expires, making the
--   in-flight window last as long as the test needs.
--
--   Three properties this deliberately keeps:
--
--     * __Inert when unarmed.__ 'awaitStageGate' on an unarmed gate is
--       one 'IORef' read returning 'False'. No phase is advanced, no
--       status field changes, nothing is logged and nothing sleeps, so
--       an ordinary @--dump@ \/ graphical \/ gameplay load behaves
--       byte-for-byte as before.
--     * __Bounded and self-releasing.__ 'sgHoldSeconds' is clamped into
--       @(0, 'maxStageGateHold']@ and the hold ends on its own at that
--       deadline ('sgExpired' then records that it was NOT a clean
--       release), so an aborted probe run or a failed assertion can
--       never wedge the world thread indefinitely.
--     * __One-shot.__ Arming holds exactly ONE transaction; the gate
--       disarms itself the moment that hold ends, so a later load in
--       the same session is never held by a stale arm.
data StageGate = StageGate
    { sgArmed       ∷ !Bool
      -- ^ True from 'armStageGate' until the hold it authorized ends.
    , sgHoldSeconds ∷ !Double
      -- ^ Upper bound on a single hold, in seconds.
    , sgHeldRequest ∷ !(Maybe Int)
      -- ^ The load request id currently parked at the gate. This is
      --   what lets a caller POSITIVELY observe the hold (rather than
      --   inferring it from a phase that a fast machine may already
      --   have moved past) and confirm it is holding the transaction it
      --   actually armed the gate for.
    , sgExpired     ∷ !Bool
      -- ^ Set when the most recent hold ended by hitting 'sgHoldSeconds'
      --   instead of by 'releaseStageGate'. A test that sees this knows
      --   its coordination did not hold and must fail rather than
      --   reporting a pass it never observed.
    } deriving (Eq, Show)

-- | The gate every 'newLoadStatusRef' starts in: disarmed, holding
--   nothing.
unarmedStageGate ∷ StageGate
unarmedStageGate = StageGate
    { sgArmed = False, sgHoldSeconds = 0
    , sgHeldRequest = Nothing, sgExpired = False }

-- | Hold bound used when 'armStageGate' is given a non-positive or
--   non-finite duration (e.g. @debug.armLoadStageGate()@ with no
--   argument).
defaultStageGateHold ∷ Double
defaultStageGateHold = 30

-- | Hard ceiling on any single hold, whatever the caller asked for.
--   Comfortably longer than the checks a probe runs inside the window,
--   and comfortably shorter than the 180 s budgets those probes wait
--   for a load with.
maxStageGateHold ∷ Double
maxStageGateHold = 120

-- | How often a parked world thread re-reads the gate. Small enough
--   that a release is picked up promptly, large enough not to spin.
stageGatePollMicros ∷ Int
stageGatePollMicros = 20000

-- | The load transaction's coordination state: the request-id
--   allocator, the current transaction's status, and the test-only
--   staging gate ('StageGate'). Opaque — the constructor is not
--   exported, so the gate rides INSIDE the handle 'EngineEnv' already
--   carries as @loadStatusRef@ (and 'Engine.Core.Capability.SaveLoad'
--   already projects as @slLoadStatusRef@) rather than becoming a new
--   root-owner field of its own.
data LoadStatusRef =
    LoadStatusRef !(IORef Int) !(IORef (Maybe LoadStatus)) !(IORef StageGate)
                  !(MVar ()) !(IORef (Maybe ScenarioLoadStatus))
                  !(IORef ScenarioLoadSeams) !(IORef (Maybe (Int, ScenarioPlan)))
                  !(IORef CaptureCell)
    deriving (Eq)
    -- ^ The scenario fields (#2713) are the admission lock, the scenario
    --   request record, the scenario test seams and the continuation's plan.
    --   The last (#2714) is the scenario CAPTURE request record and its test
    --   seams. They ride inside this handle, like the stage gate, so no new
    --   @EngineEnv@ field exists.

newLoadStatusRef ∷ IO LoadStatusRef
newLoadStatusRef = LoadStatusRef <$> newIORef 0 <*> newIORef Nothing
                                 <*> newIORef unarmedStageGate
                                 <*> newMVar () <*> newIORef Nothing
                                 <*> newIORef noSeams <*> newIORef Nothing
                                 <*> newIORef (CaptureCell Nothing noCaptureSeams)

-- | Accept a new load request, allocating a fresh request id. Fails if
--   another load is already in-flight (non-terminal outcome). Does NOT
--   check for a concurrent SAVE — callers cross-check
--   'Engine.Save.Barrier.saveInProgress' themselves alongside this (two
--   small, independent, purpose-specific checks rather than fusing this
--   module into the save barrier — either shape was acceptable per the
--   issue's review).
beginLoad ∷ LoadStatusRef → Text → IO (Either Text Int)
beginLoad (LoadStatusRef nextR statusR _ _ _ _ _ _) saveName = do
    current ← readIORef statusR
    case current of
        Just s | lsOutcome s ≡ Nothing →
            pure $ Left "a load transaction is already active"
        _ → do
            n ← atomicModifyIORef' nextR (\i → (i + 1, i + 1))
            writeIORef statusR $ Just LoadStatus
                { lsRequestId = n, lsSaveName = saveName
                , lsPhase = LoadRequested, lsOutcome = Nothing
                , lsReconciliationFailures = []
                , lsFailedAtPhase = Nothing }
            pure $ Right n

-- | Advance the in-flight load to a new non-terminal phase. A no-op if
--   @n@ no longer names the current transaction or it already reached a
--   terminal outcome — defensive; should not happen since only one load
--   is ever in flight and only 'failLoad'/'finishLoad' end it.
advanceLoad ∷ LoadStatusRef → Int → LoadPhase → IO ()
advanceLoad (LoadStatusRef _ statusR _ _ _ _ _ _) n phase =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing = s { lsPhase = phase }
           | otherwise                                 = s

-- | End a load that FAILED. Before a replacement session was published
--   (every ordinary load, and a scenario load until its shell is swapped in)
--   this is 'LoadFailed' with 'lsFailedAtPhase' set, the promise that the old
--   session survived. #2713: a scenario load whose replacement WAS published
--   (its record says so) has no such session to promise — a failure of its
--   recovery staging, of the recovery's publish barrier, or of anything else
--   the existing paths report through here ends it as 'LoadReplacementFailed'
--   with 'lsFailedAtPhase' unset, exactly as 'failPublishedLoad' does.
failLoad ∷ LoadStatusRef → Int → Text → IO ()
failLoad (LoadStatusRef _ statusR _ _ scenarioR _ _ _) n err = do
    scenario ← readIORef scenarioR
    let published = case scenario of
            Just r | slsRequestId r ≡ n → slsPublished r
            _ → False
        step s
            | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing =
                if published
                    then s { lsPhase = LoadReplacementFailed, lsOutcome = Just (LoadAborted err) }
                    else s { lsPhase = LoadFailed, lsOutcome = Just (LoadAborted err)
                           , lsFailedAtPhase = Just (lsPhase s) }
            | otherwise = s
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())

finishLoad ∷ LoadStatusRef → Int → IO ()
finishLoad (LoadStatusRef _ statusR _ _ _ _ _ _) n =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing =
               s { lsPhase = LoadPublished, lsOutcome = Just LoadSucceeded }
           | otherwise = s

-- | Issue #1204: terminate a load that PUBLISHED successfully but whose
--   post-publication Lua @onSaveLoaded@ reconciliation broadcast had at
--   least one callback raise. The counterpart to 'finishLoad' on the
--   same handoff — the caller (see
--   "Engine.Scripting.Lua.Thread.Dispatch") picks between them by
--   whether the broadcast reported any failure, and every callback has
--   already been attempted by the time either runs.
--
--   Deliberately NOT 'failLoad': that records the pre-publication
--   'LoadFailed' shape, whose contract ('docs/persistence_contract.md')
--   is that the old session survived unchanged. Here the session swap
--   and the barrier release already happened, so the honest report is a
--   published-but-incompletely-reconciled session — hence its own
--   terminal phase, its own outcome, and 'lsFailedAtPhase' left alone.
--
--   A no-op on an empty failure list (nothing failed, so there is
--   nothing to report) and, like its siblings, on a request id that no
--   longer names the current transaction or one that already reached a
--   terminal outcome.
failReconciliation ∷ LoadStatusRef → Int → [ReconciliationFailure] → IO ()
failReconciliation (LoadStatusRef _ statusR _ _ _ _ _ _) n failures
    | null failures = pure ()
    | otherwise     = atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing =
               s { lsPhase = LoadReconciliationFailed
                 , lsOutcome = Just
                     (LoadReconciliationIncomplete
                        (renderReconciliationFailures failures))
                 , lsReconciliationFailures = failures }
           | otherwise = s

-- | Issue #2713: terminate a scenario load whose replacement session was
--   PUBLISHED and then could not be completed. The counterpart of
--   'failReconciliation' for a failure that is not a callback's: the old
--   session is gone, so 'lsFailedAtPhase' (which promises it survived)
--   is left alone, and the phase is 'LoadReplacementFailed' rather than
--   'LoadFailed'. A no-op for a request id that no longer names the
--   current transaction or one that already ended.
failPublishedLoad ∷ LoadStatusRef → Int → Text → IO ()
failPublishedLoad (LoadStatusRef _ statusR _ _ _ _ _ _) n err =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing =
               s { lsPhase = LoadReplacementFailed
                 , lsOutcome = Just (LoadAborted err) }
           | otherwise = s

readLoadStatus ∷ LoadStatusRef → IO (Maybe LoadStatus)
readLoadStatus (LoadStatusRef _ statusR _ _ _ _ _ _) = readIORef statusR

loadInProgress ∷ LoadStatusRef → IO Bool
loadInProgress ref = maybe False ((≡ Nothing) . lsOutcome) <$> readLoadStatus ref

-- | Test-only (issue #1181): arm the staging gate so the NEXT load
--   transaction parks before staging. @holdSeconds@ is clamped into
--   @(0, 'maxStageGateHold']@, with a non-positive or non-finite
--   request falling back to 'defaultStageGateHold' — the bound is the
--   engine's, not the caller's, so no arming mistake can wedge the
--   world thread.
--
--   Arming while a hold is already in progress simply re-arms the gate
--   state; it does not extend the running hold's own deadline, which
--   was fixed when that hold began.
armStageGate ∷ LoadStatusRef → Double → IO ()
armStageGate (LoadStatusRef _ _ gateR _ _ _ _ _) holdSeconds =
    writeIORef gateR unarmedStageGate
        { sgArmed = True, sgHoldSeconds = clampedHold holdSeconds }

-- | The clamp 'armStageGate' applies. Split out so the hspec suite can
--   pin it without arming a real gate.
clampedHold ∷ Double → Double
clampedHold s
    | isNaN s ∨ isInfinite s ∨ s ≤ 0 = defaultStageGateHold
    | otherwise                      = min maxStageGateHold s

-- | Test-only (issue #1181): release the gate. A parked world thread
--   resumes on its next poll; an armed-but-never-reached gate is simply
--   disarmed, so a probe's @finally@ can call this unconditionally.
releaseStageGate ∷ LoadStatusRef → IO ()
releaseStageGate (LoadStatusRef _ _ gateR _ _ _ _ _) =
    atomicModifyIORef' gateR $ \g → (g { sgArmed = False }, ())

-- | Test-only (issue #1181): the gate's current state, for a caller
--   that must positively observe the hold before racing anything
--   against it.
readStageGate ∷ LoadStatusRef → IO StageGate
readStageGate (LoadStatusRef _ _ gateR _ _ _ _ _) = readIORef gateR

-- | Test-only (issue #1181): the world thread's park point, called
--   once per load transaction immediately before staging.
--
--   Returns 'False' immediately — after a single 'IORef' read, with no
--   sleep, no logging and no status mutation — when the gate is not
--   armed, which is every load outside a probe that armed it. Returns
--   'True' when this call actually held, whether it ended by release or
--   by expiry ('sgExpired' distinguishes them).
--
--   Ending a hold always disarms the gate and clears 'sgHeldRequest',
--   so exactly one transaction is ever held per arm.
awaitStageGate ∷ LoadStatusRef → Int → IO Bool
awaitStageGate (LoadStatusRef _ _ gateR _ _ _ _ _) requestId = do
    gate ← readIORef gateR
    if not (sgArmed gate)
      then pure False
      else do
        started ← getMonotonicTime
        atomicModifyIORef' gateR $ \g →
            (g { sgHeldRequest = Just requestId }, ())
        expired ← park (started + sgHoldSeconds gate)
        atomicModifyIORef' gateR $ \g →
            (g { sgArmed = False, sgHeldRequest = Nothing
               , sgExpired = expired }, ())
        pure True
  where
    park deadline = do
        g ← readIORef gateR
        if not (sgArmed g)
          then pure False
          else do
            now ← getMonotonicTime
            if now ≥ deadline
              then pure True
              else threadDelay stageGatePollMicros >> park deadline

-- * Admission (#2713)

-- | Run @act@ holding the admission lock.
--
--   Admission to a save, an autosave, a load, a scenario load or a
--   capture is a CHECK of the other kind's state followed by a CLAIM of
--   this kind's. Done as two reads and a write — the shape 'beginLoad' and
--   'Engine.Save.Barrier.beginSave' have on their own — two callers on two
--   threads can both pass both checks. Every claimant therefore runs its
--   check and its claim as ONE action under this lock, which is what makes
--   the busy rejection immediate and exclusive.
--
--   The lock is held only for the check and the claim, never for an
--   operation's lifetime: the operation's own state ('loadInProgress',
--   'Engine.Save.Barrier.saveInProgress') is what the next claimant
--   reads, and the publish barrier a load opens for itself
--   ('Engine.Save.Barrier.beginSave' on the load's own behalf) is not an
--   admission and does not take it.
withAdmission ∷ LoadStatusRef → IO α → IO α
withAdmission (LoadStatusRef _ _ _ lock _ _ _ _) act = withMVar lock (const act)

claimLoadLocked ∷ LoadStatusRef → IO Bool → Text → IO (Either Busy Int)
claimLoadLocked ref saving name = do
    isSaving ← saving
    isLoading ← loadInProgress ref
    if isSaving then pure (Left BusySave)
    else if isLoading then pure (Left BusyLoad)
    else either (const (Left BusyLoad)) Right <$> beginLoad ref name

-- | Admit a load: refuse while a save (or capture) or another load is
--   active, else accept it with a fresh request id. @saving@ reads the
--   save barrier. Nothing about the operation that held the line changes.
claimLoad ∷ LoadStatusRef → IO Bool → Text → IO (Either Busy Int)
claimLoad ref saving name = withAdmission ref (claimLoadLocked ref saving name)

-- | Admit a save, an autosave or a capture: refuse while a load is
--   active, else run @begin@ (the barrier's own 'Engine.Save.Barrier.beginSave',
--   which refuses a second save itself) INSIDE the same lock.
claimSave ∷ LoadStatusRef → IO β → IO (Either Busy β)
claimSave ref begin = withAdmission ref $ do
    isLoading ← loadInProgress ref
    if isLoading then pure (Left BusyLoad) else Right <$> begin

-- * The scenario request record (#2713)

-- | Admit a SCENARIO load: 'claimLoad' plus the request's own record,
--   created as one step so no identity exists without it.
claimScenarioLoad ∷ LoadStatusRef → IO Bool → Text → IO (Either Busy Int)
claimScenarioLoad ref@(LoadStatusRef _ _ _ _ scenarioR _ _ _) saving source =
    withAdmission ref $ do
        claimed ← claimLoadLocked ref saving source
        forM_ claimed $ \n →
            writeIORef scenarioR (Just (initialScenarioStatus n source))
        pure claimed

-- | The scenario request's record, only while it belongs to the CURRENT
--   load transaction: a later ordinary load supersedes it, so a stale
--   record is never reported under a newer request's name.
readScenarioStatus ∷ LoadStatusRef → IO (Maybe ScenarioLoadStatus)
readScenarioStatus ref@(LoadStatusRef _ _ _ _ scenarioR _ _ _) = do
    current ← readLoadStatus ref
    rec' ← readIORef scenarioR
    pure $ case (current, rec') of
        (Just l, Just s) | lsRequestId l ≡ slsRequestId s → Just (settle l s)
        _ → Nothing
  where
    -- A load failure the existing transaction recorded on its own — the
    -- world thread's staging failing, the publish barrier timing out, the
    -- prepared Lua state refusing to apply — ends the generic transaction
    -- without this record having been told. The record must never stay
    -- non-terminal under a terminal request, so it is settled here, from
    -- the generic outcome, rather than at each of those sites.
    settle l s = case lsOutcome l of
        Just (LoadAborted msg) | isNothing (slsOutcome s) →
            let base = fromMaybe (FailureContext (slsStage s) (CauseSetup msg) [])
                                 (slsPendingFailure s)
                recovered
                    | slsPublished s = RecoveryFailed msg
                    | otherwise = RecoveryPreserved
                ctx | slsPublished s = base { fcDetails = fcDetails base <> [msg] }
                    | otherwise = base
            in s { slsStage = StageFailed, slsOutcome = Just (ScenarioUnsuccessful ctx)
                 , slsRecovery = recovered }
        _ → s

-- | Change the record of request @n@. A no-op for any other request id
--   and once the request is terminal, so a stale completion can neither
--   finish nor overwrite a newer request.
updateScenarioStatus ∷ LoadStatusRef → Int → (ScenarioLoadStatus → ScenarioLoadStatus) → IO ()
updateScenarioStatus (LoadStatusRef _ _ _ _ scenarioR _ _ _) n f =
    atomicModifyIORef' scenarioR $ \m → case m of
        Just s | slsRequestId s ≡ n ∧ isNothing (slsOutcome s) → (Just (f s), ())
        _ → (m, ())

-- | End request @n@ with @outcome@, recording the recovery disposition
--   and ending the generic load transaction the way the outcome says:
--   ready is 'finishLoad'; a failure BEFORE the swap is 'failLoad' (the
--   previous session stands); after it, a callback failure is
--   'failReconciliation' and anything else 'failPublishedLoad'. The
--   scenario record ends FIRST, so the moment 'loadInProgress' clears a
--   reader already sees the terminal result.
terminateScenario ∷ LoadStatusRef → Int → ScenarioLoadOutcome → Recovery → IO ()
terminateScenario ref n outcome recovery = do
    current ← readScenarioStatus ref
    case current of
        Just s | slsRequestId s ≡ n ∧ isNothing (slsOutcome s) → do
            updateScenarioStatus ref n $ \r -> r
                { slsOutcome = Just outcome
                , slsStage = case outcome of
                    ScenarioReady → StageReady
                    ScenarioUnsuccessful _ → StageFailed
                , slsRecovery = recovery }
            case outcome of
                ScenarioReady → finishLoad ref n
                ScenarioUnsuccessful ctx
                    | not (slsPublished s) → failLoad ref n (failureText ctx)
                    | CauseReconciliation fs ← fcCause ctx →
                        failReconciliation ref n [ ReconciliationFailure m e | (m, e) ← fs ]
                    | otherwise → failPublishedLoad ref n (failureText ctx)
        _ → pure ()

-- | Test-only: install the scenario seams. Inert until armed.
armScenarioSeams ∷ LoadStatusRef → ScenarioLoadSeams → IO ()
armScenarioSeams (LoadStatusRef _ _ _ _ _ seamsR _ _) = writeIORef seamsR

disarmScenarioSeams ∷ LoadStatusRef → IO ()
disarmScenarioSeams ref = armScenarioSeams ref noSeams

-- | Consult the seam at @point@: one 'IORef' read, then whatever the
--   suite armed (which may block or throw). A synchronous exception it
--   throws is meant to reach the handoff's own exception boundary.
scenarioSeamPoint ∷ LoadStatusRef → ScenarioLoadPoint → IO ()
scenarioSeamPoint (LoadStatusRef _ _ _ _ _ seamsR _ _) point =
    readIORef seamsR ≫= \seams → seamAt seams point

-- | How long the service waits for the simulation to consume what it
--   installed: the armed override, else @shipped@ seconds.
scenarioSeamSimWait ∷ LoadStatusRef → Double → IO Double
scenarioSeamSimWait (LoadStatusRef _ _ _ _ _ seamsR _ _) shipped =
    fromMaybe shipped ∘ seamSimWait <$> readIORef seamsR

-- | The decode format the suite armed, if any (the shipped one otherwise).
scenarioSeamFormat ∷ LoadStatusRef → IO (Maybe ScenarioFormat)
scenarioSeamFormat (LoadStatusRef _ _ _ _ _ seamsR _ _) = seamFormat <$> readIORef seamsR

-- | Keep the plan the continuation will install (keyed by request id, so a
--   stale plan can never be mistaken for a newer request's).
setScenarioPlan ∷ LoadStatusRef → Int → ScenarioPlan → IO ()
setScenarioPlan (LoadStatusRef _ _ _ _ _ _ planR _) n plan = writeIORef planR (Just (n, plan))

-- | The plan of request @n@, if it is the one held.
readScenarioPlan ∷ LoadStatusRef → Int → IO (Maybe ScenarioPlan)
readScenarioPlan (LoadStatusRef _ _ _ _ _ _ planR _) n = do
    held ← readIORef planR
    pure $ case held of
        Just (m, plan) | m ≡ n → Just plan
        _ → Nothing

-- | Consume request @n@'s plan.
takeScenarioPlan ∷ LoadStatusRef → Int → IO (Maybe ScenarioPlan)
takeScenarioPlan ref@(LoadStatusRef _ _ _ _ _ _ planR _) n = do
    plan ← readScenarioPlan ref n
    forM_ plan $ \_ → writeIORef planR Nothing
    pure plan

-- * The scenario capture request record (#2714)

-- | The capture request's record beside its test seams: one cell, so the
--   handle gains one field.
data CaptureCell = CaptureCell
    { ccStatus ∷ !(Maybe ScenarioCaptureStatus)
    , ccSeams  ∷ !CaptureSeams
    }

-- | Admit a scenario CAPTURE: refuse while a load (ordinary or scenario) is
--   active, else run @begin@ — the save barrier's own 'Engine.Save.Barrier.beginSave',
--   which refuses a second save, an autosave or a capture itself — INSIDE
--   the same admission lock, and keep the request's record under the id the
--   barrier allocated. One step, so no identity exists without its record
--   and a conflicting request changes nothing about the operation that holds
--   the line.
claimCapture ∷ LoadStatusRef → IO (Either Text Int) → CaptureRequest → IO (Either Busy Int)
claimCapture ref@(LoadStatusRef _ _ _ _ _ _ _ cellR) begin req =
    withAdmission ref $ do
        loading ← loadInProgress ref
        if loading then pure (Left BusyLoad) else do
            started ← begin
            case started of
                Left _ → pure (Left BusySave)
                Right n → do
                    atomicModifyIORef' cellR $ \c →
                        (c { ccStatus = Just (initialCaptureStatus n req) }, ())
                    pure (Right n)

-- | The capture record as last written. It names whichever capture ran
--   last; whether it is still the CURRENT operation is the save barrier's
--   to say (see "Scenario.Capture.Service".@readCaptureStatus@).
readCaptureRecord ∷ LoadStatusRef → IO (Maybe ScenarioCaptureStatus)
readCaptureRecord (LoadStatusRef _ _ _ _ _ _ _ cellR) = ccStatus <$> readIORef cellR

-- | Change the record of request @n@. A no-op for any other request id and
--   once the request is terminal, so a stale completion can neither finish
--   nor overwrite a newer request.
updateCaptureStatus ∷ LoadStatusRef → Int → (ScenarioCaptureStatus → ScenarioCaptureStatus) → IO ()
updateCaptureStatus (LoadStatusRef _ _ _ _ _ _ _ cellR) n f =
    atomicModifyIORef' cellR $ \c → case ccStatus c of
        Just s | cstRequestId s ≡ n ∧ isNothing (cstOutcome s) → (c { ccStatus = Just (f s) }, ())
        _ → (c, ())

-- | End request @n@ with @outcome@. The record ends FIRST, so by the time
--   the caller ends the barrier transaction (which is what lets the next
--   operation in) a reader already sees the terminal result. A no-op for a
--   request that already ended or is not the recorded one.
terminateCapture ∷ LoadStatusRef → Int → CaptureOutcome → IO ()
terminateCapture ref n outcome = updateCaptureStatus ref n $ \r → r
    { cstOutcome = Just outcome
    , cstStage = case outcome of
        CaptureSucceeded _ → CaptureDone
        CaptureUnsuccessful _ → CaptureFailed
    , cstPublication = case outcome of
        CaptureUnsuccessful f → cfPublication f
        CaptureSucceeded _ → Published }

-- | Is a capture holding the line? True exactly while the recorded capture
--   has no outcome AND the save barrier's current transaction is still that
--   request: a record the barrier has moved past (a transaction that ended
--   or was replaced) holds nothing, so a request that somehow never
--   finished cannot pin the pause forever.
captureInProgress ∷ LoadStatusRef → SaveBarrier → IO Bool
captureInProgress ref barrier = do
    rec' ← readCaptureRecord ref
    case rec' of
        Just r | isNothing (cstOutcome r) → do
            cur ← readSaveStatus barrier
            pure $ case cur of
                Just b → ssRequestId b ≡ cstRequestId r ∧ isNothing (ssOutcome b)
                Nothing → False
        _ → pure False

-- | Test-only: install the capture seams. Inert until armed.
armCaptureSeams ∷ LoadStatusRef → CaptureSeams → IO ()
armCaptureSeams (LoadStatusRef _ _ _ _ _ _ _ cellR) seams =
    atomicModifyIORef' cellR $ \c → (c { ccSeams = seams }, ())

disarmCaptureSeams ∷ LoadStatusRef → IO ()
disarmCaptureSeams ref = armCaptureSeams ref noCaptureSeams

-- | Consult the seam at @point@: one 'IORef' read, then whatever the suite
--   armed (which may block or throw).
captureSeamPoint ∷ LoadStatusRef → CapturePoint → IO ()
captureSeamPoint (LoadStatusRef _ _ _ _ _ _ _ cellR) point =
    readIORef cellR ≫= \c → cseamAt (ccSeams c) point

-- | The storage operations the suite armed, if any (the production ones
--   otherwise).
captureSeamOps ∷ LoadStatusRef → IO (Maybe StorageOps)
captureSeamOps (LoadStatusRef _ _ _ _ _ _ _ cellR) = cseamOps ∘ ccSeams <$> readIORef cellR

-- | The transforms the suite armed on the projection and the encoded bytes
--   (the identity otherwise).
captureSeamTransforms ∷ LoadStatusRef → IO (Scenario → Scenario, BS.ByteString → BS.ByteString)
captureSeamTransforms (LoadStatusRef _ _ _ _ _ _ _ cellR) = do
    c ← readIORef cellR
    pure (cseamScenario (ccSeams c), cseamBytes (ccSeams c))
