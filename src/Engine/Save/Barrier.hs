{-# LANGUAGE Strict #-}

-- | The in-process coordination state for a save snapshot.  It deliberately
-- knows nothing about engine queues: an owner acknowledges only after it has
-- completed the work it accepted before the transaction.  Keeping that
-- protocol small makes the failure and ordering rules independently testable.
module Engine.Save.Barrier
    ( SaveOwner(..), SavePhase(..), SaveOutcome(..), SaveStatus(..)
    , SaveBarrier, newSaveBarrier, beginSave, acknowledgeSave, failSave
    , reachSnapshot, releaseCaptureLock, finishSave, waitForOwners
    , readSaveStatus, acknowledgeCurrent, captureLocked, saveInProgress
    , ownerGated, ownersGated
    ) where

import UPrelude
import qualified Data.Set as Set
import Control.Concurrent.STM

data SaveOwner = SaveLua | SaveWorld | SaveUnit | SaveBuilding | SaveCombat
               | SaveSimulation | SaveInput | SaveRender
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | 'SaveEncoding' (#758) is the window between "the snapshot is
--   captured and validated, state owners may resume" and "the encoded
--   save has actually landed on disk (or failed to)" — see
--   'releaseCaptureLock'.
data SavePhase = SaveRequested | SavePausing | SaveWaitingOwners
               | SaveSnapshotBoundary | SaveEncoding
               | SaveCaptureComplete | SaveFailed
    deriving (Eq, Show)

data SaveOutcome = SaveSucceeded | SaveAborted Text deriving (Eq, Show)

data SaveStatus = SaveStatus
    { ssRequestId ∷ !Int
    , ssPhase     ∷ !SavePhase
    , ssOwners    ∷ !(Set.Set SaveOwner)
    , ssAcknowledged ∷ !(Set.Set SaveOwner)
    , ssQuiescencePasses ∷ !Int
    , ssOutcome   ∷ !(Maybe SaveOutcome)
    } deriving (Eq, Show)

data SaveBarrier = SaveBarrier !(TVar Int) !(TVar (Maybe SaveStatus)) deriving (Eq)

-- | World→simulation→world is the longest currently supported persistent
-- command cycle.  Every pass drains every owner once; the first handles the
-- original work, the second its first-hop effects, and the third writeback.
requiredQuiescencePasses ∷ Int
requiredQuiescencePasses = 3

newSaveBarrier ∷ IO SaveBarrier
newSaveBarrier = SaveBarrier <$> newTVarIO 0 <*> newTVarIO Nothing

beginSave ∷ SaveBarrier → Set.Set SaveOwner → IO (Either Text Int)
beginSave (SaveBarrier next status) owners = atomically $ do
    current ← readTVar status
    case current of
        Just s | ssOutcome s ≡ Nothing → pure $ Left "a save transaction is already active"
        _ → do
            n ← (+ 1) ⊚ readTVar next
            writeTVar next n
            writeTVar status $ Just SaveStatus
                { ssRequestId = n, ssPhase = SavePausing, ssOwners = owners
                , ssAcknowledged = Set.empty, ssQuiescencePasses = 0
                , ssOutcome = Nothing }
            pure $ Right n

-- | An acknowledgment from an 'owner' that isn't a member of THIS
--   transaction's own 'ssOwners' (issue #763: a
--   conditionally-registered owner like 'SaveRender'/'SaveInput' may
--   still be ticking and acking during a transaction that never
--   included it — e.g. the render thread's per-tick ack during a plain
--   save, which never lists 'SaveRender') is a no-op, not an insert:
--   'Set.insert'ing it into 'ssAcknowledged' regardless of membership
--   would permanently break the exact-set-equality check
--   ('acks ≡ ssOwners s') this whole protocol is built on, wedging
--   that transaction until 'waitForOwners' times out even once every
--   REAL owner has acked.
--
--   'SaveWaitingOwners' is ignored for the same reason
--   'SaveSnapshotBoundary' and 'SaveEncoding' are (#2221): that phase is
--   reached only once every owner has acknowledged the FINAL quiescence
--   pass, so every further ack is a parked owner's routine per-tick
--   'acknowledgeCurrent' and has nothing left to record. Admitting them
--   would inflate 'ssQuiescencePasses' past 'requiredQuiescencePasses'
--   for as long as the initiator takes to reach the boundary, making the
--   per-request park state ('ownerGated') depend on how many ticks
--   happened to elapse rather than on the protocol.
acknowledgeSave ∷ SaveBarrier → Int → SaveOwner → IO ()
acknowledgeSave (SaveBarrier _ status) n owner = atomically $ do
    current ← readTVar status
    forM_ current $ \s → when (ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing
            ∧ ssPhase s ≢ SaveWaitingOwners
            ∧ ssPhase s ≢ SaveSnapshotBoundary ∧ ssPhase s ≢ SaveEncoding
            ∧ Set.member owner (ssOwners s)) $ do
        let acks = Set.insert owner (ssAcknowledged s)
        if acks ≡ ssOwners s ∧ ssQuiescencePasses s + 1 < requiredQuiescencePasses
            -- One full drain is not a boundary: a command handled by the
            -- last owner can causally enqueue work for one that acknowledged
            -- earlier.  Make every owner drain once more before capture.
            then writeTVar status $ Just s
                -- The save call itself owns Lua and blocks its interpreter
                -- while waiting, so it is already quiescent for every later
                -- worker-drain pass; only asynchronous owners must re-ack.
                { ssAcknowledged = Set.intersection (Set.singleton SaveLua) (ssOwners s)
                , ssQuiescencePasses = ssQuiescencePasses s + 1
                , ssPhase = SavePausing }
            else do
                let phase = if acks ≡ ssOwners s then SaveWaitingOwners else SavePausing
                    passes = if acks ≡ ssOwners s then ssQuiescencePasses s + 1
                                                     else ssQuiescencePasses s
                writeTVar status $ Just s { ssAcknowledged = acks, ssPhase = phase
                                           , ssQuiescencePasses = passes }

waitForOwners ∷ Int → SaveBarrier → Int → IO (Either Text ())
waitForOwners micros (SaveBarrier _ status) n = do
    delay ← registerDelay micros
    atomically $ do
        current ← readTVar status
        case current of
            Just s | ssRequestId s ≡ n, ssOutcome s ≡ Nothing
                   , ssAcknowledged s ≡ ssOwners s → pure $ Right ()
            Just s | ssRequestId s ≡ n, Just (SaveAborted err) ← ssOutcome s → pure $ Left err
            _ → do
                timedOut ← readTVar delay
                check timedOut
                -- Name the outstanding OWNERS and the PHASE the
                -- transaction stalled in: with owners parking from
                -- their own final acknowledgement (#2221), "which
                -- owners are missing" and "how far the transaction
                -- got" are different questions, and a timeout report
                -- that answers only the first cannot distinguish an
                -- owner that never ran from one still mid-pass.
                let missing = maybe [] (\s → Set.toList
                        (ssOwners s Set.\\ ssAcknowledged s)) current
                    phase = maybe "no transaction" (tshow ∘ ssPhase) current
                pure $ Left $ "timed out waiting for save state owners: "
                    <> tshow missing <> " in phase " <> phase

-- | Declare the snapshot boundary. Refuses unless the FINAL quiescence
--   pass is actually complete (#2221): the boundary means "no owner has
--   a gated tick in flight", and only 'ssAcknowledged ≡ ssOwners' — the
--   very condition 'waitForOwners' returns 'Right' on — establishes
--   that. A premature call (the initiator racing ahead of the pass, a
--   mis-sequenced caller) leaves the phase alone rather than locking
--   owners out mid-pass and declaring a boundary they never reached.
--   'SaveEncoding' is refused too, so a late call cannot re-close the
--   capture window #758's 'releaseCaptureLock' already opened.
reachSnapshot ∷ SaveBarrier → Int → IO ()
reachSnapshot (SaveBarrier _ status) n = atomically $ modifyTVar' status $ fmap $ \s →
    if ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing ∧ ssPhase s ≢ SaveEncoding
         ∧ ssAcknowledged s ≡ ssOwners s
       then s { ssPhase = SaveSnapshotBoundary } else s

-- | #758: unblock 'captureLocked' — every state owner may resume
--   ordinary work — the instant the snapshot is captured and
--   validated, WITHOUT yet declaring the save transaction terminally
--   complete. Deliberately does not touch 'ssOutcome': 'saveInProgress'
--   must stay True (so a second, overlapping 'beginSave' is still
--   refused, and 'engine.getSaveStatus()' doesn't report a premature
--   success) until 'finishSave'/'failSave' actually runs once encoding
--   and disk I/O finish — a disk-write failure after this point must
--   still surface as a real 'SaveFailed' outcome, not be silently
--   swallowed. 'acknowledgeSave' explicitly ignores 'SaveEncoding' too
--   (same as 'SaveSnapshotBoundary'), so a worker thread's routine
--   per-tick 'acknowledgeCurrent' call can't reopen or corrupt this
--   window while the encode/write step is in flight.
releaseCaptureLock ∷ SaveBarrier → Int → IO ()
releaseCaptureLock (SaveBarrier _ status) n = atomically $ modifyTVar' status $ fmap $ \s →
    if ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing
       then s { ssPhase = SaveEncoding } else s

finishSave ∷ SaveBarrier → Int → IO ()
finishSave (SaveBarrier _ status) n = atomically $ modifyTVar' status $ fmap $ \s →
    if ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing
       then s { ssPhase = SaveCaptureComplete, ssOutcome = Just SaveSucceeded } else s

failSave ∷ SaveBarrier → Int → Text → IO ()
failSave (SaveBarrier _ status) n err = atomically $ modifyTVar' status $ fmap $ \s →
    if ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing
       then s { ssPhase = SaveFailed, ssOutcome = Just (SaveAborted err) } else s

readSaveStatus ∷ SaveBarrier → IO (Maybe SaveStatus)
readSaveStatus (SaveBarrier _ status) = readTVarIO status

-- | Tick-boundary helper for worker loops.  A terminal transaction is never
-- acknowledged again, so a later save necessarily gets a fresh boundary.
acknowledgeCurrent ∷ SaveBarrier → SaveOwner → IO ()
acknowledgeCurrent barrier owner = do
    current ← readSaveStatus barrier
    forM_ current $ \s → when (ssOutcome s ≡ Nothing) $
        acknowledgeSave barrier (ssRequestId s) owner

-- | Once capture starts, persistent command consumers must leave subsequent
-- work queued for after the transaction.  The world owner is the sole
-- exception: it consumes the already-authorized WorldSave command itself.
--
-- This is the GLOBAL question, and it stays effective for every caller
-- during 'SaveSnapshotBoundary' — including one that is not an owner of
-- the transaction. An owner's own tick boundary asks 'ownerGated'
-- instead (#2221), which is this plus that owner's post-final-ack park.
captureLocked ∷ SaveBarrier → IO Bool
captureLocked barrier = do
    current ← readSaveStatus barrier
    pure $ maybe False ((≡ SaveSnapshotBoundary) ∘ ssPhase) current

saveInProgress ∷ SaveBarrier → IO Bool
saveInProgress barrier = do
    current ← readSaveStatus barrier
    pure $ maybe False ((≡ Nothing) ∘ ssOutcome) current

-- | The gate every state owner reads at its own tick boundary — the
--   global capture lock OR this owner's own post-final-acknowledgement
--   park (#2221). Owner loops read this instead of 'captureLocked';
--   'captureLocked' remains the global question, and stays the right
--   one for a caller that is not an owner of the transaction.
ownerGated ∷ SaveBarrier → SaveOwner → IO Bool
ownerGated barrier owner = ownersGated barrier [owner]

-- | 'ownerGated' for a loop that answers for SEVERAL owners, decided
--   against ONE reading of the barrier so the answer cannot straddle a
--   phase change. 'Unit.Thread' is the case that needs it: 'SaveUnit'
--   and 'SaveBuilding' share one physical loop, and the loop's gated
--   work is skipped once EITHER of them is parked.
ownersGated ∷ SaveBarrier → [SaveOwner] → IO Bool
ownersGated barrier owners = do
    current ← readSaveStatus barrier
    pure $ case current of
        Nothing → False
        Just s  → ssPhase s ≡ SaveSnapshotBoundary ∨ any (ownerParked s) owners

-- | #757 requirement 5, restored by #2221: once an owner acknowledges
--   the boundary it must not mutate persistent state until capture is
--   complete or the barrier is aborted.
--
--   The acknowledgement is not itself the boundary. The initiator still
--   needs every OTHER owner's final acknowledgement and its own
--   'reachSnapshot' before 'SaveSnapshotBoundary' exists, so an owner
--   whose gate asked only 'captureLocked' would read False, start a
--   fresh unlocked tick, and could still be running it when the world
--   owner captures the snapshot (save) or when
--   @World.Load.Publish.publishStagedSession@ swaps the session refs
--   (load). Parking the owner from its own final acknowledgement until
--   capture completes closes that window at its source, rather than
--   one straddling write at a time.
--
--   Deliberately OWNER-SPECIFIC. During the final pass an owner that
--   has acknowledged is parked while owners that have not are still
--   free to finish their own pass — otherwise the barrier could never
--   reach the boundary at all. Earlier passes park nobody:
--   'ssQuiescencePasses' counts COMPLETED passes, so the final pass is
--   the one running at @requiredQuiescencePasses - 1@, and the
--   multi-pass causal drain keeps today's drain-and-continue behaviour
--   exactly.
--
--   'SaveEncoding' is excluded because that is precisely where #758's
--   'releaseCaptureLock' resumes every owner — before encoding's disk
--   I/O finishes — and a terminal 'ssOutcome' is excluded so a failed
--   or aborted transaction unparks everyone on the spot, with no
--   intervening phase in which gameplay briefly runs.
ownerParked ∷ SaveStatus → SaveOwner → Bool
ownerParked s owner =
    ssOutcome s ≡ Nothing
    ∧ ssPhase s ≢ SaveEncoding
    ∧ ssQuiescencePasses s ≥ requiredQuiescencePasses - 1
    ∧ Set.member owner (ssOwners s)
    ∧ Set.member owner (ssAcknowledged s)
