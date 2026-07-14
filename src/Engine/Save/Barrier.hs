{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The in-process coordination state for a save snapshot.  It deliberately
-- knows nothing about engine queues: an owner acknowledges only after it has
-- completed the work it accepted before the transaction.  Keeping that
-- protocol small makes the failure and ordering rules independently testable.
module Engine.Save.Barrier
    ( SaveOwner(..), SavePhase(..), SaveOutcome(..), SaveStatus(..)
    , SaveBarrier, newSaveBarrier, beginSave, acknowledgeSave, failSave
    , reachSnapshot, finishSave, waitForOwners, readSaveStatus, acknowledgeCurrent
    , captureLocked
    ) where

import UPrelude
import qualified Data.Set as Set
import Control.Concurrent.STM

data SaveOwner = SaveLua | SaveWorld | SaveUnit | SaveBuilding | SaveCombat | SaveSimulation
    deriving (Eq, Ord, Show, Enum, Bounded)

data SavePhase = SaveRequested | SavePausing | SaveWaitingOwners
               | SaveSnapshotBoundary | SaveCaptureComplete | SaveFailed
    deriving (Eq, Show)

data SaveOutcome = SaveSucceeded | SaveAborted Text deriving (Eq, Show)

data SaveStatus = SaveStatus
    { ssRequestId ∷ !Int
    , ssPhase     ∷ !SavePhase
    , ssOwners    ∷ !(Set.Set SaveOwner)
    , ssAcknowledged ∷ !(Set.Set SaveOwner)
    , ssOutcome   ∷ !(Maybe SaveOutcome)
    } deriving (Eq, Show)

data SaveBarrier = SaveBarrier !(TVar Int) !(TVar (Maybe SaveStatus)) deriving (Eq)

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
                , ssAcknowledged = Set.empty, ssOutcome = Nothing }
            pure $ Right n

acknowledgeSave ∷ SaveBarrier → Int → SaveOwner → IO ()
acknowledgeSave (SaveBarrier _ status) n owner = atomically $ do
    current ← readTVar status
    forM_ current $ \s → when (ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing) $ do
        let acks = Set.insert owner (ssAcknowledged s)
            phase = if acks ≡ ssOwners s then SaveWaitingOwners else SavePausing
        writeTVar status $ Just s { ssAcknowledged = acks, ssPhase = phase }

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
                pure $ Left "timed out waiting for save state owners"

reachSnapshot ∷ SaveBarrier → Int → IO ()
reachSnapshot (SaveBarrier _ status) n = atomically $ modifyTVar' status $ fmap $ \s →
    if ssRequestId s ≡ n ∧ ssOutcome s ≡ Nothing
       then s { ssPhase = SaveSnapshotBoundary } else s

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
