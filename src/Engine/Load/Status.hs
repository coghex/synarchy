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
    , readLoadStatus, loadInProgress, loadPublishCommitted
    , StageGate(..), unarmedStageGate
    , defaultStageGateHold, maxStageGateHold
    , armStageGate, releaseStageGate, readStageGate, awaitStageGate
    ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import qualified Data.Text as T

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
    deriving (Eq)

newLoadStatusRef ∷ IO LoadStatusRef
newLoadStatusRef = LoadStatusRef <$> newIORef 0 <*> newIORef Nothing
                                 <*> newIORef unarmedStageGate

-- | Accept a new load request, allocating a fresh request id. Fails if
--   another load is already in-flight (non-terminal outcome). Does NOT
--   check for a concurrent SAVE — callers cross-check
--   'Engine.Save.Barrier.saveInProgress' themselves alongside this (two
--   small, independent, purpose-specific checks rather than fusing this
--   module into the save barrier — either shape was acceptable per the
--   issue's review).
beginLoad ∷ LoadStatusRef → Text → IO (Either Text Int)
beginLoad (LoadStatusRef nextR statusR _) saveName = do
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
advanceLoad (LoadStatusRef _ statusR _) n phase =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing = s { lsPhase = phase }
           | otherwise                                 = s

failLoad ∷ LoadStatusRef → Int → Text → IO ()
failLoad (LoadStatusRef _ statusR _) n err =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing =
               s { lsPhase = LoadFailed, lsOutcome = Just (LoadAborted err)
                 , lsFailedAtPhase = Just (lsPhase s) }
           | otherwise = s

finishLoad ∷ LoadStatusRef → Int → IO ()
finishLoad (LoadStatusRef _ statusR _) n =
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
failReconciliation (LoadStatusRef _ statusR _) n failures
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

readLoadStatus ∷ LoadStatusRef → IO (Maybe LoadStatus)
readLoadStatus (LoadStatusRef _ statusR _) = readIORef statusR

loadInProgress ∷ LoadStatusRef → IO Bool
loadInProgress ref = maybe False ((≡ Nothing) . lsOutcome) <$> readLoadStatus ref

-- | True only once this load's publication is COMMITTED — the Lua side
--   applied cleanly and 'World.Command.Types.WorldLoadPublish' has been
--   queued for the world thread — and not yet terminal (#2221).
--
--   'loadInProgress' cannot answer that question and must not be used
--   for it. A load is "in progress" from the moment it is requested,
--   including the whole window between 'Engine.Save.Barrier.reachSnapshot'
--   and the outcome of
--   'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'\'s
--   @applyLuaLoad@ — and an apply failure there aborts with the OLD
--   session still live and, by
--   @docs/persistence_contract.md@, unchanged. Anything IRREVERSIBLE
--   that a publication would justify (above all
--   'Engine.Scripting.Lua.Message.discardLuaMessagesForActiveLoad'
--   destroying that session's queued work) must therefore wait for
--   this, not for the capture boundary, which opens strictly earlier.
loadPublishCommitted ∷ LoadStatusRef → IO Bool
loadPublishCommitted ref =
    maybe False (\s → lsOutcome s ≡ Nothing ∧ lsPhase s ≡ LoadWaitingPublish)
        <$> readLoadStatus ref

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
armStageGate (LoadStatusRef _ _ gateR) holdSeconds =
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
releaseStageGate (LoadStatusRef _ _ gateR) =
    atomicModifyIORef' gateR $ \g → (g { sgArmed = False }, ())

-- | Test-only (issue #1181): the gate's current state, for a caller
--   that must positively observe the hold before racing anything
--   against it.
readStageGate ∷ LoadStatusRef → IO StageGate
readStageGate (LoadStatusRef _ _ gateR) = readIORef gateR

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
awaitStageGate (LoadStatusRef _ _ gateR) requestId = do
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
