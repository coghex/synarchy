{-# LANGUAGE Strict #-}
-- | Liveness pins for the generated-world library (issue #2024): how a
--   process declares that it depends on an entry no save references yet
--   — a world it generated and has not saved — so that cleanup in THIS
--   process or in ANY OTHER retains the entry.
--
--   Reference discovery reads saves, and a save does not exist until the
--   player saves. A pin is the filesystem-visible stand-in for that
--   missing reference, and it is made of the same two mechanisms the
--   library lock is made of ("World.GeneratedLibrary.Lock"):
--
--   * A PIN FILE, @\<token\>.pin-\<pid\>-\<n\>@, created exclusively in
--     the library root and held under a POSIX record lock for as long as
--     the pin is live. The kernel drops the lock when the holding process
--     exits for any reason, so "is this pin live?" is answered by trying
--     to take that lock: refused means a live process holds it; granted
--     means the holder is gone and the pin is abandoned. Nothing needs a
--     pid, a heartbeat or an age threshold — the same proof the lock
--     module gives for transients.
--   * An in-process COUNT per id, because record locks are per process
--     and a second thread probing this process's own pin file would be
--     granted the lock and, on closing, drop it. Cleanup therefore never
--     probes a pin file of an id this process holds — it consults the
--     count first — and pin transitions run under the process mutex so
--     a count can never be observed mid-change.
--
--   Acquisition runs under the LIBRARY lock ('World.GeneratedLibrary.withPinnedReferences'
--   takes it around 'acquirePinsUnlocked'), so a pin cannot come into
--   being while a cleanup anywhere is deciding, and a cleanup cannot
--   begin while a pin is being created: whichever wins the lock runs to
--   completion first. Release needs only the process mutex — removing
--   one's own pin file cannot invalidate a decision a concurrent cleanup
--   has already made, because that cleanup saw the pin held.
module World.GeneratedLibrary.Pins
    ( PinStore
    , newPinStore
    , inProcessPins
    , acquirePinsUnlocked
    , releasePinsUnlocked
    , PinProbe(..)
    , sweepPinFile
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Monad (foldM)
import Control.Exception (IOException, SomeException, try)
import System.FilePath ((</>))
import System.IO (SeekMode(..))
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Posix.IO
    ( OpenMode(..), OpenFileFlags(..), LockRequest(..), closeFd
    , defaultFileFlags, openFd, setLock )
import System.Posix.Types (Fd)
import World.Page.GeneratedId (GeneratedWorldId)
import World.Save.Storage.Durable (rejectSymlinkedPath, removeIfExists)
import World.GeneratedLibrary.Types
import World.GeneratedLibrary.Layout (pinFileName)
import World.GeneratedLibrary.Publish (nextTransientSuffix)

-- | One live pin in this process: how many overlapping holders, and the
--   descriptor whose record lock is the cross-process evidence.
data HeldPin = HeldPin
    { hpCount ∷ !Int
    , hpFd    ∷ !Fd
    , hpPath  ∷ !FilePath
    }

newtype PinStore = PinStore (TVar (Map.Map GeneratedWorldId HeldPin))

newPinStore ∷ IO PinStore
newPinStore = PinStore ⊚ newTVarIO Map.empty

-- | Every id this process currently holds a pin for.
inProcessPins ∷ PinStore → IO (Set.Set GeneratedWorldId)
inProcessPins (PinStore var) = Map.keysSet ⊚ readTVarIO var

-- | Take one more hold on each of @gids@, creating and locking a pin
--   file for each id this process does not hold yet. Caller holds the
--   library lock. All or nothing: a failure part-way releases the pin
--   files this call created and leaves every count as it was.
acquirePinsUnlocked
    ∷ LibraryConfig → PinStore → [GeneratedWorldId] → IO (Either LibraryFailure ())
acquirePinsUnlocked cfg (PinStore var) gids = do
    held0 ← readTVarIO var
    outcome ← go held0 [] gids
    case outcome of
        Right held → do
            atomically (writeTVar var held)
            pure (Right ())
        Left (created, failure) → do
            mapM_ dropPinFile created
            pure (Left failure)
  where
    go held _ [] = pure (Right held)
    go held created (gid : rest) = case Map.lookup gid held of
        Just pin → go (Map.insert gid pin { hpCount = hpCount pin + 1 } held) created rest
        Nothing → do
            made ← createPinFile (lcRoot cfg) gid
            case made of
                Left failure → pure (Left (created, failure))
                Right (fd, path) →
                    go (Map.insert gid (HeldPin 1 fd path) held) ((fd, path) : created) rest

-- | Give back one hold on each of @gids@; the last hold removes the pin
--   file and closes its descriptor, in that order, so no other process
--   ever sees this pin file unlocked. Caller holds the process mutex.
--   An id this process does not hold is ignored.
releasePinsUnlocked ∷ PinStore → [GeneratedWorldId] → IO ()
releasePinsUnlocked (PinStore var) gids = do
    held0 ← readTVarIO var
    held ← foldM release held0 gids
    atomically (writeTVar var held)
  where
    release held gid = case Map.lookup gid held of
        Nothing → pure held
        Just pin
            | hpCount pin > 1 →
                pure (Map.insert gid pin { hpCount = hpCount pin - 1 } held)
            | otherwise → do
                dropPinFile (hpFd pin, hpPath pin)
                pure (Map.delete gid held)

-- | Create @gid@'s pin file exclusively and take its record lock. The
--   name carries this process's id and a counter, and @O_EXCL@ makes a
--   collision a retry rather than a shared file.
createPinFile ∷ FilePath → GeneratedWorldId → IO (Either LibraryFailure (Fd, FilePath))
createPinFile root gid = attempt (0 ∷ Int)
  where
    failure path reason = LibraryFailure LibPin (Just gid) (Just path) reason
    attempt n = do
        (pid, counter) ← nextTransientSuffix
        let path = root </> pinFileName gid pid counter
        opened ← try (openFd path ReadWrite
                        defaultFileFlags { creat = Just 0o644, exclusive = True })
        case opened of
            Left (e ∷ IOException)
                | isAlreadyExistsError e ∧ n < 64 → attempt (n + 1)
                | otherwise → pure (Left (failure path ("cannot create pin file: " <> tshow e)))
            Right fd → do
                locked ← try (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
                case locked of
                    Right () → pure (Right (fd, path))
                    Left (e ∷ IOException) → do
                        dropPinFile (fd, path)
                        pure (Left (failure path ("cannot lock pin file: " <> tshow e)))

-- | Remove a pin file this process owns, then release its lock by closing
--   the descriptor. Best effort: the file may already be gone if a
--   cleanup elsewhere swept it as abandoned after a crash-and-restart of
--   this pid, and that is fine.
dropPinFile ∷ (Fd, FilePath) → IO ()
dropPinFile (fd, path) = do
    _ ← removeIfExists path
    r ← try (closeFd fd)
    case (r ∷ Either SomeException ()) of _ → pure ()

-- | What probing a pin file that is NOT this process's found.
data PinProbe
    = PinHeld
        -- ^ Another live process holds the pin: the id is live.
    | PinAbandoned
        -- ^ Nobody held it — the file has been removed.
    | PinUnreadable !Text
        -- ^ Could not be judged (a symlink, an open failure): treated
        --   as held, since absence of evidence is not evidence of
        --   absence, and reported.
    deriving (Show, Eq)

-- | Probe one foreign pin file under the library lock: try its record
--   lock without waiting. Refused means held. Granted means abandoned,
--   and the file is unlinked BEFORE the descriptor (and with it the lock
--   just taken) is closed, so no other prober can find the file unlocked
--   in between. A file that vanished before it could be opened was
--   released by its owner in the meantime and counts as abandoned with
--   nothing left to remove. NEVER call this on a pin file this process
--   itself holds: a record lock does not conflict within one process, so
--   the probe would be granted and closing its descriptor would drop the
--   real pin's lock.
sweepPinFile ∷ FilePath → IO PinProbe
sweepPinFile path = do
    linkSafe ← rejectSymlinkedPath path
    case linkSafe of
        Left reason → pure (PinUnreadable reason)
        Right () → do
            opened ← try (openFd path ReadWrite defaultFileFlags)
            case opened of
                Left (e ∷ IOException)
                    | isDoesNotExistError e → pure PinAbandoned
                    | otherwise → pure (PinUnreadable ("cannot open pin file "
                                                      <> T.pack path <> ": " <> tshow e))
                Right fd → do
                    locked ← try (setLock fd (WriteLock, AbsoluteSeek, 0, 0))
                    case locked of
                        Left (_ ∷ IOException) → do
                            closeQuietlyFd fd
                            pure PinHeld
                        Right () → do
                            _ ← removeIfExists path
                            closeQuietlyFd fd
                            pure PinAbandoned
  where
    closeQuietlyFd fd = do
        r ← try (closeFd fd)
        case (r ∷ Either SomeException ()) of _ → pure ()
