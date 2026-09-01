{-# LANGUAGE Strict #-}
-- | The shared generated-world library (issue #2024, world-map epic
--   #2017, design slice WML-4; decisions D-5, D-17, D-18).
--
--   One directory — 'libraryDirectory', a sibling of @saves/@ under the
--   resource root — holds one entry per 'GeneratedWorldId', so every
--   save descended from one generated world can share that world's
--   durable artifacts instead of embedding a copy. This module is the
--   library's whole public surface; the mechanics live one module down
--   each and are documented there:
--
--   * "World.GeneratedLibrary.Layout" — what the root may contain, the
--     one-way id-to-directory mapping, the payload-name policy, the
--     record formats.
--   * "World.GeneratedLibrary.Entry" — the single judgement of whether
--     a directory is a complete entry.
--   * "World.GeneratedLibrary.Lock" — the cross-process lock that makes
--     "abandoned" a proof.
--   * "World.GeneratedLibrary.Publish" — the atomic stage-validate-
--     rename transaction and republish-under-the-saved-id.
--   * "World.GeneratedLibrary.Registry" — the index and its
--     reconciliation with the authoritative directory.
--   * "World.GeneratedLibrary.References" — the failure-bearing census
--     of what current saves reference.
--   * "World.GeneratedLibrary.Cleanup" — removal of what is positively
--     proven unreferenced or abandoned, and nothing else.
--
--   === What this slice does not do
--
--   It stores bytes it does not interpret. The map manifest and pages
--   that will fill it are WML-7/WML-8's, and the load-time resolution
--   of a save's id to an entry is WML-9's; nothing here is called by
--   world generation, saving or loading yet. Base-chunk records extend
--   this same library later (D-18) rather than creating a second store.
--
--   === Liveness the filesystem cannot see
--
--   Reference discovery reads SAVES. A session holding a generated
--   world it has not saved yet references that world in memory only,
--   and a cleanup that consulted the filesystem alone would remove its
--   entry. 'withPinnedReferences' is the in-process answer: a
--   process-owned operation pins the ids it is about to depend on, and
--   cleanup retains a pinned id exactly as it retains a referenced one.
--   Pins are shared by every handle opened on one root in this process,
--   and pin transitions use the same process mutex cleanup acquires before
--   taking its filesystem lock. That makes "pin then start the operation"
--   atomic with respect to cleanup: whichever begins first wins the mutex,
--   and cleanup can never act from a pin snapshot that was already stale
--   when its protected work began. The integration slices are responsible
--   for pinning every live page's id around any cleanup they schedule.
module World.GeneratedLibrary
    ( -- * Opening
      Library
    , libraryConfig
    , openLibrary
    , libraryDirectory
    , defaultLibraryConfig
    , LibraryConfig(..)
      -- * Publication
    , publishEntry
    , publishEntryWith
    , PublishHooks(..)
    , noPublishHooks
    , PayloadFile(..)
    , PublishOutcome(..)
    , PublishReport(..)
      -- * Reading
    , entryDirectory
    , lookupEntry
    , listEntries
    , LibraryEntry(..)
    , EntryStatus(..)
    , EntryRecord(..)
    , PayloadDescriptor(..)
    , RegistryRow(..)
    , LibraryInventory(..)
    , RegistrySource(..)
      -- * Maintenance
    , reconcileLibrary
    , ReconcileReport(..)
    , scanReferences
    , ReferenceScan(..)
    , cleanupLibrary
    , CleanupReport(..)
      -- * Liveness
    , withPinnedReferences
    , pinnedReferences
      -- * Failures
    , LibraryPhase(..)
    , LibraryFailure(..)
    , renderLibraryFailure
    ) where

import UPrelude
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Concurrent.STM
    (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (IOException, bracket_, try)
import System.Directory
    (createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import World.Page.GeneratedId (GeneratedWorldId)
import World.Save.Storage.Durable (rejectSymlinkedManagedPath)
import World.GeneratedLibrary.Types
import World.GeneratedLibrary.Layout
import World.GeneratedLibrary.Entry
import World.GeneratedLibrary.Lock
import World.GeneratedLibrary.Publish
import World.GeneratedLibrary.Registry
import World.GeneratedLibrary.References
import World.GeneratedLibrary.Cleanup

type PinCounts = Map.Map GeneratedWorldId Int

-- | An opened library: its configuration plus the process-owned pin set
--   SHARED by every handle opened on the same absolute root. Mutations are
--   serialised by the library lock; pin transitions are serialised by that
--   lock's process mutex.
data Library = Library
    { libraryConfig ∷ !LibraryConfig
    , libraryPins   ∷ !(TVar PinCounts)
    }

-- | Open (creating if absent) the library at 'lcRoot'. Refuses a root,
--   or a parent, that is a symlink, and a root whose parent does not
--   exist — the resource root always does.
openLibrary ∷ LibraryConfig → IO (Either LibraryFailure Library)
openLibrary cfg = do
    let root = lcRoot cfg
    safety ← rejectSymlinkedManagedPath root
    case safety of
        Left reason → pure (Left (LibraryFailure LibUnsafePath Nothing (Just root) reason))
        Right () → do
            created ← try (createDirectoryIfMissing False root)
            case created of
                Left (e ∷ IOException) →
                    pure (Left (LibraryFailure LibRootCreate Nothing (Just root) (tshow e)))
                Right () → do
                    pins ← sharedPinsForRoot root
                    pure (Right (Library cfg pins))

-- Publication ------------------------------------------------------------------

-- | Publish (or republish) the entry for @gid@. See
--   "World.GeneratedLibrary.Publish" for the transaction and every
--   outcome.
publishEntry ∷ Library → GeneratedWorldId → [PayloadFile] → IO (Either LibraryFailure PublishReport)
publishEntry = publishEntryWith noPublishHooks

publishEntryWith
    ∷ PublishHooks → Library → GeneratedWorldId → [PayloadFile]
    → IO (Either LibraryFailure PublishReport)
publishEntryWith hooks lib gid files =
    withLibraryLock (libraryConfig lib) (publishUnlocked (libraryConfig lib) hooks gid files)

-- Reading -------------------------------------------------------------------------

-- | Where @gid@'s entry lives (whether or not it exists): the ONLY path
--   a payload consumer should ever construct for an entry.
entryDirectory ∷ Library → GeneratedWorldId → FilePath
entryDirectory lib gid = lcRoot (libraryConfig lib) </> entryDirectoryName gid

-- | Judge @gid@'s final directory now, without the lock: 'Nothing' when
--   no final directory exists, otherwise its status. A concurrent
--   commit is a rename, so a reader sees the entry before or after it,
--   never in between.
lookupEntry ∷ Library → GeneratedWorldId → IO (Either LibraryFailure (Maybe LibraryEntry))
lookupEntry lib gid = do
    let cfg = libraryConfig lib
        dir = entryDirectory lib gid
    safety ← rejectSymlinkedManagedPath (lcRoot cfg)
    case safety of
        Left reason → pure (Left (LibraryFailure LibUnsafePath (Just gid) (Just (lcRoot cfg)) reason))
        Right () → do
            exists ← doesDirectoryExist dir
            if not exists then pure (Right Nothing) else
                Right . Just . libraryEntryFor (renderName gid) ⊚ readEntryDirectory dir
  where
    renderName = fromString . entryDirectoryName

-- | Which entries exist, from the registry when it is readable and
--   otherwise rebuilt in memory from the entry records (the file itself
--   is only rewritten under the lock, by 'reconcileLibrary',
--   'publishEntry' or 'cleanupLibrary').
listEntries ∷ Library → IO (Either LibraryFailure LibraryInventory)
listEntries lib = do
    let root = lcRoot (libraryConfig lib)
    safety ← rejectSymlinkedManagedPath root
    case safety of
        Left reason → pure (Left (LibraryFailure LibUnsafePath Nothing (Just root) reason))
        Right () → do
            existing ← readRegistryFile root
            case existing of
                RegistryPresent reg → pure (Right (LibraryInventory (rfRows reg) FromRegistryFile))
                other → do
                    let why = case other of
                            RegistryTorn reason → reason
                            _                   → "registry file is absent"
                    rebuilt ← inventoryFromDirectory root
                    pure $ case rebuilt of
                        Left reason → Left (LibraryFailure LibDirectoryList Nothing (Just root) reason)
                        Right entries → Right (LibraryInventory
                            [ registryRowFor digest rec
                            | e ← entries, leStatus e ≡ EntryCommitted
                            , Just rec ← [leRecord e], Just digest ← [leDigest e] ]
                            (RebuiltFromDirectory why))

-- Maintenance -----------------------------------------------------------------------

-- | Bring the registry into agreement with the directory and restore
--   any entry an interrupted republish left displaced. Returns every
--   final's judgement and what was done.
reconcileLibrary ∷ Library → IO (Either LibraryFailure ([LibraryEntry], ReconcileReport))
reconcileLibrary lib =
    withLibraryLock (libraryConfig lib) $ do
        r ← reconcileUnlocked (libraryConfig lib)
        pure ((\(_, entries, _, report) → (entries, report)) ⊚ r)

-- | The reference census on its own, without the lock — for callers
--   that want to know what a cleanup WOULD find.
scanReferences ∷ Library → HS.HashSet Text → IO ReferenceScan
scanReferences lib = scanSaveReferences (lcSavesDirectory (libraryConfig lib))

-- | Remove proven-unreferenced entries and abandoned transients. See
--   "World.GeneratedLibrary.Cleanup". @luaKnownNames@ is the live Lua
--   component registry, exactly as 'World.Save.Serialize.listSaves'
--   receives it.
cleanupLibrary ∷ Library → HS.HashSet Text → IO (Either LibraryFailure CleanupReport)
cleanupLibrary lib luaKnownNames =
    withLibraryLock (libraryConfig lib) $ do
        -- Read INSIDE the process mutex held by 'withLibraryLock'. Pin
        -- transitions take the same mutex, so this snapshot cannot already
        -- be stale when cleanup starts acting on it.
        pins ← pinnedReferences lib
        cleanupUnlocked (libraryConfig lib) luaKnownNames pins

-- Liveness ----------------------------------------------------------------------------

-- | Hold @gids@ live for the duration of @action@: a cleanup through ANY
--   handle on the same root retains them whether or not any save references
--   them yet. Acquisition and release take the library's process mutex, so
--   an action cannot start behind a cleanup that already owns the mutex, and
--   a cleanup that starts behind this pin must observe it. Nested and
--   overlapping pins compose (a count per id), and the pin is released on
--   every exit path.
withPinnedReferences ∷ Library → [GeneratedWorldId] → IO a → IO a
withPinnedReferences lib gids =
    bracket_ (transition 1) (transition (-1))
  where
    transition delta = withLibraryProcessMutex (adjust delta)
    adjust delta = atomically $ modifyTVar' (libraryPins lib) $ \pins →
        foldr (Map.alter (bump delta)) pins gids
    bump delta current =
        let n = fromMaybe 0 current + delta
        in if n ≤ 0 then Nothing else Just n

pinnedReferences ∷ Library → IO (Set.Set GeneratedWorldId)
pinnedReferences lib = Map.keysSet ⊚ readTVarIO (libraryPins lib)

-- | Process-wide per-root pin stores. Opening the same root twice must not
--   create two liveness views: cleanup through either handle protects an
--   operation using the other. The table is intentionally process-lifetime;
--   a root has no close operation, and retaining an empty TVar is harmless.
sharedPinStores ∷ MVar (Map.Map FilePath (TVar PinCounts))
sharedPinStores = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE sharedPinStores #-}

sharedPinsForRoot ∷ FilePath → IO (TVar PinCounts)
sharedPinsForRoot root = do
    key ← makeAbsolute root
    modifyMVar sharedPinStores $ \stores → case Map.lookup key stores of
        Just pins → pure (stores, pins)
        Nothing → do
            pins ← newTVarIO Map.empty
            pure (Map.insert key pins stores, pins)
