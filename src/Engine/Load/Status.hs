{-# LANGUAGE Strict, UnicodeSyntax #-}

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
    , LoadStatusRef, newLoadStatusRef
    , beginLoad, advanceLoad, failLoad, finishLoad
    , readLoadStatus, loadInProgress
    ) where

import UPrelude
import Data.IORef

-- | The 12 phases requirement 2 lists, in the order a successful load
--   moves through them. 'LoadFailed' is terminal and reachable from any
--   non-terminal phase; 'LoadPublished' is the only other terminal
--   phase.
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
    deriving (Eq, Show, Enum, Bounded)

data LoadOutcome = LoadSucceeded | LoadAborted !Text deriving (Eq, Show)

data LoadStatus = LoadStatus
    { lsRequestId ∷ !Int
    , lsSaveName  ∷ !Text
    , lsPhase     ∷ !LoadPhase
    , lsOutcome   ∷ !(Maybe LoadOutcome)
    } deriving (Eq, Show)

data LoadStatusRef = LoadStatusRef !(IORef Int) !(IORef (Maybe LoadStatus))
    deriving (Eq)

newLoadStatusRef ∷ IO LoadStatusRef
newLoadStatusRef = LoadStatusRef <$> newIORef 0 <*> newIORef Nothing

-- | Accept a new load request, allocating a fresh request id. Fails if
--   another load is already in-flight (non-terminal outcome). Does NOT
--   check for a concurrent SAVE — callers cross-check
--   'Engine.Save.Barrier.saveInProgress' themselves alongside this (two
--   small, independent, purpose-specific checks rather than fusing this
--   module into the save barrier — either shape was acceptable per the
--   issue's review).
beginLoad ∷ LoadStatusRef → Text → IO (Either Text Int)
beginLoad (LoadStatusRef nextR statusR) saveName = do
    current ← readIORef statusR
    case current of
        Just s | lsOutcome s ≡ Nothing →
            pure $ Left "a load transaction is already active"
        _ → do
            n ← atomicModifyIORef' nextR (\i → (i + 1, i + 1))
            writeIORef statusR $ Just LoadStatus
                { lsRequestId = n, lsSaveName = saveName
                , lsPhase = LoadRequested, lsOutcome = Nothing }
            pure $ Right n

-- | Advance the in-flight load to a new non-terminal phase. A no-op if
--   @n@ no longer names the current transaction or it already reached a
--   terminal outcome — defensive; should not happen since only one load
--   is ever in flight and only 'failLoad'/'finishLoad' end it.
advanceLoad ∷ LoadStatusRef → Int → LoadPhase → IO ()
advanceLoad (LoadStatusRef _ statusR) n phase =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing = s { lsPhase = phase }
           | otherwise                                 = s

failLoad ∷ LoadStatusRef → Int → Text → IO ()
failLoad (LoadStatusRef _ statusR) n err =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing =
               s { lsPhase = LoadFailed, lsOutcome = Just (LoadAborted err) }
           | otherwise = s

finishLoad ∷ LoadStatusRef → Int → IO ()
finishLoad (LoadStatusRef _ statusR) n =
    atomicModifyIORef' statusR $ \mS → (fmap step mS, ())
  where
    step s | lsRequestId s ≡ n ∧ lsOutcome s ≡ Nothing =
               s { lsPhase = LoadPublished, lsOutcome = Just LoadSucceeded }
           | otherwise = s

readLoadStatus ∷ LoadStatusRef → IO (Maybe LoadStatus)
readLoadStatus (LoadStatusRef _ statusR) = readIORef statusR

loadInProgress ∷ LoadStatusRef → IO Bool
loadInProgress ref = maybe False ((≡ Nothing) . lsOutcome) <$> readLoadStatus ref
