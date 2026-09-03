-- | The engine's shutdown lifecycle and the one monotonic transition
--   into it.
--
--   Extracted from 'Engine.Core.State' by #2283 so the shared worker
--   lifecycle ('Engine.Core.Thread') can perform the fail-stop
--   transition ITSELF — one owner, written before any fallible crash
--   reporting or cleanup — without importing the engine's central state
--   record and inverting the layering. That module deliberately knows
--   nothing of 'Engine.Core.State.EngineEnv'; a four-constructor enum
--   and one 'atomicModifyIORef'' is all it needs, and a bare 'IORef'
--   of this type is all a spec needs to drive it with no engine at all.
--
--   'Engine.Core.State' re-exports both names, so every existing
--   importer sees them exactly where it always did.
module Engine.Core.Lifecycle
  ( EngineLifecycle(..)
  , requestEngineCleanup
  ) where

import UPrelude
import Data.IORef (IORef, atomicModifyIORef')

data EngineLifecycle
  = EngineStarting
  | EngineRunning
  | CleaningUp
  | EngineStopped
  deriving (Eq, Show)

-- | Ask a running engine to shut down, monotonically:
--
--   > EngineStarting → CleaningUp
--   > EngineRunning  → CleaningUp
--   > CleaningUp     → CleaningUp
--   > EngineStopped  → EngineStopped
--
--   The counterpart to 'Engine.Loop.Mode.promoteToRunning', which
--   preserves an already-advanced lifecycle for the same reason and by
--   the same single atomic step: the read and the write must not be
--   separable, because the thread racing this one is precisely the
--   thread whose transition must not be lost.
--
--   Answers whether THIS call performed the transition, so a caller
--   that must report or act exactly once (the debug console's
--   at-most-once listener-loss shutdown, #2170) does not have to
--   re-read the lifecycle afterwards and reintroduce the race in the
--   report.
--
--   'EngineStopped' is deliberately NOT rewound to 'CleaningUp': a
--   worker discovering a failure after the engine already finished
--   stopping has nothing left to ask for.
requestEngineCleanup ∷ IORef EngineLifecycle → IO Bool
requestEngineCleanup ref = atomicModifyIORef' ref $ \cur → case cur of
    EngineStarting → (CleaningUp, True)
    EngineRunning  → (CleaningUp, True)
    CleaningUp     → (CleaningUp, False)
    EngineStopped  → (EngineStopped, False)
