{-# LANGUAGE Strict #-}
-- | Mutual exclusion for the generated-world library's mutating
--   operations (issue #2024): publication, reconciliation and cleanup
--   each run under 'withLibraryLock', and reads run without it.
--
--   The lock is what turns "is this temporary abandoned?" from a guess
--   into a proof. Every publisher creates its staging directory only
--   AFTER taking the lock and holds the lock until its commit (or
--   structured failure) is complete, and cleanup takes the same lock
--   before it looks at anything. So while cleanup holds the lock, no
--   publication is in progress in ANY process — a publisher that were
--   mid-flight would still be holding it — and every transient
--   directory present is therefore the leftover of an operation whose
--   owner is gone. Nothing needs a pid, a heartbeat or an age
--   threshold.
--
--   Two mechanisms compose to give that across processes AND threads:
--
--   * A POSIX record lock (@fcntl@ @F_SETLK@, 'System.Posix.IO.setLock')
--     on 'lockFileName' in the library root. The kernel releases it
--     when the holding process exits for any reason, which is exactly
--     the "owner is gone" event abandonment is defined by.
--   * A process-global 'MVar', because record locks are per PROCESS,
--     not per descriptor: a second thread in the same process could
--     take the same record lock again, and closing ANY descriptor on
--     the file drops the process's lock. The 'MVar' serialises the
--     process's own threads so at most one descriptor is ever open on
--     the lock file at a time. Pin transitions take this same mutex, so
--     cleanup cannot begin from a stale in-process liveness snapshot.
--
--   The wait is BOUNDED ('lcLockWaitMicros'): a lock held by a wedged
--   or foreign process produces a structured 'LibLock' failure rather
--   than hanging a save or a session, and a test proves that with a
--   real second process holding the lock.
module World.GeneratedLibrary.Lock
    ( withLibraryLock
    , withLibraryProcessMutex
    ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, bracket, try)
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath ((</>))
import System.IO (SeekMode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO
    ( OpenMode(..), OpenFileFlags(..), LockRequest(..), closeFd
    , defaultFileFlags, openFd, setLock )
import World.Save.Storage.Durable (rejectSymlinkedPath, rejectSymlinkedManagedPath)
import World.GeneratedLibrary.Types
import World.GeneratedLibrary.Layout (lockFileName)

-- | Run a mutating operation under the library lock. A structured
--   'LibLock' failure is returned (never thrown) when the lock file
--   cannot be opened or the wait bound expires; an exception the action
--   itself raises propagates after the lock is released.
withLibraryLock ∷ LibraryConfig → IO (Either LibraryFailure a) → IO (Either LibraryFailure a)
withLibraryLock cfg action = withLibraryProcessMutex $ do
    let root     = lcRoot cfg
        lockPath = root </> lockFileName
        failure phase path reason = Left (LibraryFailure phase Nothing (Just path) reason)
    -- Containment is re-established HERE, on every acquisition, not
    -- only when the library was opened: the root, its parent, and then
    -- the lock file itself. A root renamed away and replaced by a
    -- symlink after 'openLibrary' would otherwise have its lock file
    -- created — and locked — inside the symlink's target, outside the
    -- resource root, before any caller's own root check ran. Checking
    -- only the lock path cannot catch that: 'pathIsSymbolicLink' looks
    -- at a path's final component and resolves everything before it.
    rootSafe ← rejectSymlinkedManagedPath root
    lockSafe ← rejectSymlinkedPath lockPath
    case (rootSafe, lockSafe) of
        (Left reason, _) → pure (failure LibUnsafePath root reason)
        (_, Left reason) → pure (failure LibUnsafePath lockPath reason)
        (Right (), Right ()) → do
            opened ← try (openFd lockPath ReadWrite
                            defaultFileFlags { creat = Just 0o644 })
            case opened of
                Left (e ∷ IOException) →
                    pure (failure LibLock lockPath ("cannot open lock file: " <> tshow e))
                Right fd → bracket (pure fd) closeFd $ \_ → do
                    deadline ← (+ micros) ⊚ getMonotonicTimeNSec
                    acquired ← acquire fd deadline
                    case acquired of
                        Left reason → pure (failure LibLock lockPath reason)
                        Right ()    → action
  where
    micros = fromIntegral (max 0 (lcLockWaitMicros cfg)) * 1000
    acquire fd deadline = do
        r ← try (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
        case r of
            Right () → pure (Right ())
            Left (e ∷ IOException) → do
                now ← getMonotonicTimeNSec
                if now ≥ deadline
                    then pure (Left ("another process holds the library lock ("
                                     <> tshow e <> ")"))
                    else threadDelay pollMicros ≫ acquire fd deadline
    pollMicros = 20_000

-- | Serialise a short in-process state transition with every mutating
--   library operation, without taking the cross-process file lock. Pin
--   acquisition and release use this boundary: if a cleanup already owns
--   the mutex the pinned action cannot begin until cleanup finishes; if the
--   pin wins first, cleanup cannot snapshot the shared pin set until the pin
--   is visible. The action passed here must not itself call
--   'withLibraryLock' (the mutex is deliberately non-reentrant).
withLibraryProcessMutex ∷ IO a → IO a
withLibraryProcessMutex action = withMVar processLibraryLock (const action)

-- | Serialises this process's own lock holders — see the module header
--   for why a record lock alone cannot. Module-level state under
--   'unsafePerformIO'\/@NOINLINE@, the same shape as
--   'World.Page.GeneratedId''s allocation counter: it has no owner
--   thread and no lifecycle, so it belongs on no capability record.
processLibraryLock ∷ MVar ()
processLibraryLock = unsafePerformIO (newMVar ())
{-# NOINLINE processLibraryLock #-}
