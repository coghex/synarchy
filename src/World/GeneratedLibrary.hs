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
--   === Liveness the saves cannot show
--
--   Reference discovery reads SAVES. A session holding a generated
--   world it has not saved yet references that world in memory only,
--   and a cleanup that consulted saves alone would remove its entry.
--   'withPinnedReferences' is the answer, and it is visible to EVERY
--   process on the root, not just this one: a pin is a file in the
--   library root held under a POSIX record lock for as long as the pin
--   is live ("World.GeneratedLibrary.Pins"), created under the library
--   lock so it cannot come into being while a cleanup anywhere is
--   deciding, and probed by cleanup the same way transients are proven
--   abandoned. In-process, pins are shared by every handle opened on one
--   directory and counted under the same process mutex cleanup holds, so
--   this process's own pins are never probed and never observed
--   mid-change. The integration slices are responsible for pinning every
--   live page's id for as long as the page lives.
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
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (IOException, finally, try)
import System.Directory
    (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>), dropTrailingPathSeparator, normalise)
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
import World.GeneratedLibrary.Pins

-- | An opened library: its configuration plus the process-owned pin
--   store SHARED by every handle opened on the same directory. Mutations
--   are serialised by the library lock; pin transitions by that lock's
--   process mutex.
data Library = Library
    { libraryConfig ∷ !LibraryConfig
    , libraryPins   ∷ !PinStore
    }

-- | Open (creating if absent) the library at 'lcRoot'. Refuses a root,
--   or a parent, that is a symlink, and a root whose parent does not
--   exist — the resource root always does.
--
--   The configured root is NORMALISED before anything else looks at it
--   — @.@ segments and doubled separators collapsed, then the trailing
--   separator dropped (in that order: collapsing @a\/b\/.\/@ leaves
--   @a\/b\/@) — and the handle carries the normalised spelling. Two
--   things depend on that. The parent containment check takes the
--   root's 'takeDirectory', and @takeDirectory "a\/b\/"@ is @"a\/b"@:
--   with a trailing separator the "parent" check would re-check the
--   root itself and a symlinked parent would pass. And two handles on
--   one directory must share one pin store, which is keyed by the
--   directory's canonical path (see 'sharedPinsForRoot'), never by the
--   spelling a caller happened to use.
openLibrary ∷ LibraryConfig → IO (Either LibraryFailure Library)
openLibrary cfg0 = do
    let cfg  = cfg0 { lcRoot = dropTrailingPathSeparator (normalise (lcRoot cfg0)) }
        root = lcRoot cfg
    safety ← rejectSymlinkedManagedPath root
    case safety of
        Left reason → pure (Left (LibraryFailure LibUnsafePath Nothing (Just root) reason))
        Right () → do
            created ← try $ do
                createDirectoryIfMissing False root
                sharedPinsForRoot root
            case created of
                Left (e ∷ IOException) →
                    pure (Left (LibraryFailure LibRootCreate Nothing (Just root) (tshow e)))
                Right pins → pure (Right (Library cfg pins))

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
        -- Read INSIDE the lock. Pin acquisition takes the same lock and
        -- release the same process mutex, so this snapshot cannot be
        -- stale while cleanup acts on it; other processes' pins are
        -- probed on disk by the cleanup itself.
        pins ← pinnedReferences lib
        cleanupUnlocked (libraryConfig lib) luaKnownNames pins

-- Liveness ----------------------------------------------------------------------------

-- | Hold @gids@ live for the duration of @action@: a cleanup through ANY
--   handle on the same root, in THIS process or any other, retains them
--   whether or not any save references them yet. Acquisition takes the
--   library lock (so it serialises with every cleanup everywhere and can
--   fail with a structured 'LibLock' or 'LibPin' failure — in which case
--   @action@ never runs); release takes the process mutex. Nested and
--   overlapping pins compose (a count per id), and the pin is released
--   on every exit path. @action@ is free to publish, reconcile or clean
--   up (the lock is not held across it); what may NOT happen is calling
--   this from inside another lock holder's action, such as a publish
--   hook, because the process mutex is not reentrant.
withPinnedReferences
    ∷ Library → [GeneratedWorldId] → IO a → IO (Either LibraryFailure a)
withPinnedReferences lib gids action = do
    acquired ← withLibraryLock cfg (acquirePinsUnlocked cfg (libraryPins lib) gids)
    case acquired of
        Left failure → pure (Left failure)
        Right () → (Right ⊚ action)
            `finally` withLibraryProcessMutex (releasePinsUnlocked (libraryPins lib) gids)
  where
    cfg = libraryConfig lib

-- | The ids THIS process currently holds pins on. Pins held by other
--   processes are visible only on disk, to cleanup's probe.
pinnedReferences ∷ Library → IO (Set.Set GeneratedWorldId)
pinnedReferences lib = inProcessPins (libraryPins lib)

-- | Process-wide per-root pin stores. Opening the same root twice must not
--   create two liveness views: cleanup through either handle protects an
--   operation using the other. The table is intentionally process-lifetime;
--   a root has no close operation, and retaining an empty TVar is harmless.
sharedPinStores ∷ MVar (Map.Map FilePath PinStore)
sharedPinStores = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE sharedPinStores #-}

-- | The pin store for the DIRECTORY @root@ names, whatever spelling names
--   it. Keyed by 'canonicalizePath' — absolute, separator-normalised,
--   every symlink in the ancestry resolved — so @generated-worlds@,
--   @generated-worlds\/@, @.\/generated-worlds@ and a spelling through a
--   symlinked temp directory all reach one 'TVar'. The root itself must
--   already exist (the caller has just created it); a root that cannot be
--   canonicalised is a root that cannot be opened.
sharedPinsForRoot ∷ FilePath → IO PinStore
sharedPinsForRoot root = do
    key ← canonicalizePath root
    modifyMVar sharedPinStores $ \stores → case Map.lookup key stores of
        Just pins → pure (stores, pins)
        Nothing → do
            pins ← newPinStore
            pure (Map.insert key pins stores, pins)
