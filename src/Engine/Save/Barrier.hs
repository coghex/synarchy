{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The in-process coordination state for a save snapshot.  It deliberately
-- knows nothing about engine queues: an owner acknowledges only after it has
-- completed the work it accepted before the transaction.  Keeping that
-- protocol small makes the failure and ordering rules independently testable.
module Engine.Save.Barrier
    ( SaveOwner(..), SavePhase(..), SaveOutcome(..), SaveStatus(..)
    , SaveBarrier, newSaveBarrier, beginSave, acknowledgeSave, failSave
    , reachSnapshot, releaseCaptureLock, finishSave, waitForOwners
    , readSaveStatus, acknowledgeCurrent, captureLocked, saveInProgress
    ) where

import UPrelude
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Concurrent.STM

data SaveOwner = SaveLua | SaveWorld | SaveUnit | SaveBuilding | SaveCombat | SaveSimulation
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

acknowledgeSave ∷ SaveBarrier → Int → SaveOwner → IO ()
acknowledgeSave (SaveBarrier _ status) n owner = atomically $ do
    current ← readTVar status
    forM_ current $ \s → when (ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing
            ∧ ssPhase s ≠ SaveSnapshotBoundary ∧ ssPhase s ≠ SaveEncoding) $ do
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
                let missing = maybe [] (\s → Set.toList
                        (ssOwners s Set.\\ ssAcknowledged s)) current
                pure $ Left $ "timed out waiting for save state owners: "
                    <> T.pack (show missing)

reachSnapshot ∷ SaveBarrier → Int → IO ()
reachSnapshot (SaveBarrier _ status) n = atomically $ modifyTVar' status $ fmap $ \s →
    if ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing
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
captureLocked ∷ SaveBarrier → IO Bool
captureLocked barrier = do
    current ← readSaveStatus barrier
    pure $ maybe False ((≡ SaveSnapshotBoundary) ∘ ssPhase) current

saveInProgress ∷ SaveBarrier → IO Bool
saveInProgress barrier = do
    current ← readSaveStatus barrier
    pure $ maybe False ((≡ Nothing) ∘ ssOutcome) current
