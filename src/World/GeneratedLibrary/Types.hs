{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Vocabulary of the shared generated-world library (issue #2024,
--   world-map epic #2017, design slice WML-4): the configuration a
--   library is opened with, the payload-neutral unit of content it
--   stores, the on-disk records it writes, and every structured result
--   its operations report.
--
--   The library is PAYLOAD-NEUTRAL by contract (issue requirement 2): it
--   knows that an entry has content, which 'GeneratedWorldId' it belongs
--   to, and enough integrity information to publish it atomically and
--   recognise it later — and knows nothing about map manifests, pixels,
--   pages, base chunks, renderer structs or any gameplay type. That is
--   why the unit of content is a 'PayloadFile' (a name and bytes) and
--   the integrity information is a 'PayloadDescriptor' (name, size,
--   digest): WML-7 fills the payload without this module changing shape.
--
--   Nothing here is a save component. The two records that reach disk
--   ('EntryRecord', 'RegistryFile') are the library's OWN files, written
--   and read only by "World.GeneratedLibrary", and are single-
--   constructor so the enum append-only audit has nothing to guard.
module World.GeneratedLibrary.Types
    ( -- * Configuration
      LibraryConfig(..)
    , libraryDirectory
    , defaultLibraryConfig
      -- * Content
    , PayloadFile(..)
    , PayloadDescriptor(..)
      -- * On-disk records
    , EntryRecord(..)
    , RegistryRow(..)
    , RegistryFile(..)
    , registryRowFor
      -- * Results
    , LibraryPhase(..)
    , LibraryFailure(..)
    , renderLibraryFailure
    , PublishOutcome(..)
    , PublishReport(..)
    , EntryStatus(..)
    , LibraryEntry(..)
    , RegistrySource(..)
    , LibraryInventory(..)
    , ReconcileReport(..)
    , emptyReconcileReport
    , ReferenceScan(..)
    , emptyReferenceScan
    , CleanupReport(..)
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Page.GeneratedId (GeneratedWorldId)
import World.Save.Serialize (savesDirectory)

-- Configuration -----------------------------------------------------------

-- | Where a library lives and how it coordinates.
data LibraryConfig = LibraryConfig
    { lcRoot ∷ !FilePath
        -- ^ The library directory. In production 'libraryDirectory',
        --   resolved relative to the resource root the executable has
        --   chdir'd into (requirement 1: a sibling of @saves/@ that
        --   moves with @--resource-root@ and @SYNARCHY_ROOT@ exactly as
        --   saves do). Tests point it at a scratch tree.
    , lcSavesDirectory ∷ !FilePath
        -- ^ The saves directory reference discovery scans. In
        --   production 'World.Save.Serialize.savesDirectory'.
    , lcLockWaitMicros ∷ !Int
        -- ^ How long a mutating operation waits for the cross-process
        --   library lock before giving up with a 'LibLock' failure.
        --   Bounded rather than indefinite so a lock held by a wedged
        --   or foreign process can never hang a save or a session; a
        --   test uses a short bound to prove the refusal.
    } deriving (Show, Eq)

-- | The library's production location: a sibling of
--   'World.Save.Serialize.savesDirectory' under the resource root.
libraryDirectory ∷ FilePath
libraryDirectory = "generated-worlds"

defaultLibraryConfig ∷ LibraryConfig
defaultLibraryConfig = LibraryConfig
    { lcRoot           = libraryDirectory
    , lcSavesDirectory = savesDirectory
    , lcLockWaitMicros = 10_000_000
    }

-- Content -----------------------------------------------------------------

-- | One file of an entry's payload: a single path component (validated
--   by 'World.GeneratedLibrary.Layout.validatePayloadName' before it
--   ever reaches the filesystem) and its complete bytes. The library
--   never interprets the bytes.
data PayloadFile = PayloadFile
    { pfName  ∷ !Text
    , pfBytes ∷ !BS.ByteString
    } deriving (Show, Eq)

-- | What the library records about one payload file: enough to know it
--   is complete (its size) and unchanged (its SHA-256), without knowing
--   what it is.
data PayloadDescriptor = PayloadDescriptor
    { pdName   ∷ !Text
    , pdSize   ∷ !Word64
    , pdDigest ∷ !BS.ByteString
    } deriving (Show, Eq, Ord, Generic, Serialize)

-- On-disk records ---------------------------------------------------------

-- | The committed-entry record written LAST into a staged entry and
--   read FIRST from a final one: it binds the expected 'GeneratedWorldId'
--   to the complete payload inventory, so a directory is a complete
--   entry exactly when this record decodes, names the id its directory's
--   name carries, and every file it lists is present at its recorded
--   size. A
--   directory without a valid record is never an entry — which is what
--   makes a torn or half-copied directory identifiable rather than
--   dangerous.
data EntryRecord = EntryRecord
    { erId          ∷ !GeneratedWorldId
    , erFiles       ∷ ![PayloadDescriptor]
        -- ^ Ascending by name; 'World.GeneratedLibrary.Layout.inventoryDigest'
        --   is defined over this order so equal inventories digest equally.
    , erPublishedAt ∷ !Text
        -- ^ ISO 8601, diagnostics only. Never compared.
    } deriving (Show, Eq, Generic, Serialize)

-- | One row of the registry: the identity and integrity summary of a
--   committed entry, without its inventory. The registry is an INDEX of
--   the directory, never the authority — 'reconcileLibrary' rebuilds it
--   from entry records whenever the two disagree.
data RegistryRow = RegistryRow
    { rrId              ∷ !GeneratedWorldId
    , rrInventoryDigest ∷ !BS.ByteString
    , rrPayloadBytes    ∷ !Word64
    , rrFileCount       ∷ !Word32
    , rrPublishedAt     ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

data RegistryFile = RegistryFile
    { rfRows ∷ [RegistryRow]
        -- ^ Ascending by id, duplicate-free — the canonical order
        --   'GeneratedWorldId''s 'Ord' exists for.
    } deriving (Show, Eq, Generic, Serialize)

-- | The registry row an entry record summarises to. @digest@ is the
--   inventory digest the caller computed over 'erFiles'.
registryRowFor ∷ BS.ByteString → EntryRecord → RegistryRow
registryRowFor digest rec = RegistryRow
    { rrId              = erId rec
    , rrInventoryDigest = digest
    , rrPayloadBytes    = sum (map pdSize (erFiles rec))
    , rrFileCount       = fromIntegral (length (erFiles rec))
    , rrPublishedAt     = erPublishedAt rec
    }

-- Results -----------------------------------------------------------------

-- | Every phase a library operation can fail in, in the order a
--   publication reaches them; an error names the phase, never just
--   "library failed" (the same discipline as
--   'World.Save.Storage.StoragePhase'). Registry writes and transient
--   removals are deliberately NOT phases: they only ever run past a
--   durability boundary, so their failures are post-commit warnings.
data LibraryPhase
    = LibUnsafePath
        -- ^ A managed path — the root, its parent, an entry, a
        --   transient directory, a record file — is a symlink.
    | LibRootCreate
    | LibLock
        -- ^ The cross-process library lock could not be created or
        --   was still held by another process when the wait bound
        --   expired.
    | LibPayloadIdentity
        -- ^ The payload itself is unacceptable: no files, a name that
        --   is not a safe single path component, a reserved name, or a
        --   duplicate.
    | LibStagingCreate
    | LibPayloadWrite
    | LibPayloadFlush
    | LibPayloadReread
    | LibPayloadValidate
        -- ^ A payload file re-read from disk is not the bytes that were
        --   written.
    | LibRecordWrite
    | LibRecordValidate
        -- ^ The entry record re-read from disk does not decode to the
        --   record that was written.
    | LibStagingSync
    | LibDisplaceExisting
        -- ^ Republishing: the existing complete entry could not be
        --   moved to its recovery name.
    | LibCommitRename
    | LibRootSync
    | LibDirectoryList
    deriving (Show, Eq, Enum, Bounded)

data LibraryFailure = LibraryFailure
    { glfPhase  ∷ !LibraryPhase
    , glfId     ∷ !(Maybe GeneratedWorldId)
    , glfPath   ∷ !(Maybe FilePath)
    , glfReason ∷ !Text
    } deriving (Show, Eq)

renderLibraryFailure ∷ LibraryFailure → Text
renderLibraryFailure f =
    "generated-world library failed during " <> tshow (glfPhase f)
        <> idSuffix <> pathSuffix <> ": " <> glfReason f
  where
    idSuffix   = maybe "" (\g → " for " <> tshow g) (glfId f)
    pathSuffix = maybe "" (\p → " (" <> T.pack p <> ")") (glfPath f)

-- | What a successful publication did.
data PublishOutcome
    = PublishedNew
        -- ^ No entry existed for the id; the staged one is now final.
    | PublishedUnchanged
        -- ^ A complete entry with an identical inventory digest already
        --   existed; the staged copy was discarded and nothing on disk
        --   changed (D-17's "compatible regeneration republishes under
        --   the saved ID" is idempotent for equal content).
    | PublishedReplaced
        -- ^ A complete entry with DIFFERENT content existed; it was
        --   displaced to a recovery name, the staged one committed, and
        --   the displaced copy removed once the registry was durable.
    deriving (Show, Eq, Ord)

data PublishReport = PublishReport
    { prOutcome  ∷ !PublishOutcome
    , prWarnings ∷ ![Text]
        -- ^ Post-commit problems (a registry write, a leftover removal)
        --   that could not fail the publication because the entry was
        --   already durable when they happened.
    } deriving (Show, Eq)

-- | What the library knows about one final directory.
data EntryStatus
    = EntryCommitted
        -- ^ A complete entry: valid record, every listed file present
        --   at its recorded size.
    | EntryUnreadable !Text
        -- ^ A final directory that is NOT a complete entry (missing or
        --   undecodable record, record naming a different id, a listed
        --   file absent or the wrong size, a symlink inside it). Never
        --   deleted, never indexed as committed, always reported.
    deriving (Show, Eq)

data LibraryEntry = LibraryEntry
    { leName   ∷ !Text
        -- ^ The final directory's own name (the canonical id token).
    , leStatus ∷ !EntryStatus
    , leRecord ∷ !(Maybe EntryRecord)
        -- ^ Present iff 'EntryCommitted'.
    , leDigest ∷ !(Maybe BS.ByteString)
        -- ^ The inventory digest, present iff 'EntryCommitted'.
    } deriving (Show, Eq)

-- | Where an inventory's rows came from.
data RegistrySource
    = FromRegistryFile
    | RebuiltFromDirectory !Text
        -- ^ The registry file was absent or torn (reason attached), so
        --   the rows were rebuilt in memory from the entry records. The
        --   file itself is only rewritten by a mutating operation.
    deriving (Show, Eq)

data LibraryInventory = LibraryInventory
    { liRows   ∷ ![RegistryRow]
    , liSource ∷ !RegistrySource
    } deriving (Show, Eq)

-- | Everything reconciliation found and did. Every list is empty on a
--   library whose registry already matched its directory.
data ReconcileReport = ReconcileReport
    { rcAdded      ∷ ![GeneratedWorldId]
        -- ^ Valid finals the registry did not index.
    , rcDropped    ∷ ![GeneratedWorldId]
        -- ^ Registry rows whose final is absent.
    , rcCorrected  ∷ ![GeneratedWorldId]
        -- ^ Registry rows whose summary disagreed with the record.
    , rcRecovered  ∷ ![GeneratedWorldId]
        -- ^ Displaced entries restored to final because an interrupted
        --   republish left no final behind.
    , rcUnreadable ∷ ![(FilePath, Text)]
        -- ^ Final directories that are not complete entries — retained.
    , rcUnfamiliar ∷ ![FilePath]
        -- ^ Names in the root the library does not own — retained.
    , rcRegistryRebuilt ∷ !(Maybe Text)
        -- ^ Why the registry file could not be read, when it couldn't.
    , rcWarnings   ∷ ![Text]
    } deriving (Show, Eq)

emptyReconcileReport ∷ ReconcileReport
emptyReconcileReport = ReconcileReport [] [] [] [] [] [] Nothing []

-- | What reference discovery learned from every save-shaped thing under
--   the saves directory. 'rsIndeterminate' is the load-bearing field:
--   a non-empty list means some slot could not yield a trustworthy
--   complete reference set, and cleanup then deletes no final entry at
--   all (requirement 7: a save that cannot be read licenses deleting
--   nothing).
data ReferenceScan = ReferenceScan
    { rsReferenced    ∷ !(Set.Set GeneratedWorldId)
        -- ^ Every id named by every readable generation of every slot
        --   — authoritative AND retained previous generations, and
        --   legacy flat files — since each protects what it references.
    , rsSourcesRead   ∷ !Int
        -- ^ How many generation files contributed.
    , rsIndeterminate ∷ ![(Text, Text)]
        -- ^ (slot, reason) for every slot whose reference set could not
        --   be trusted: a symlinked slot, an unreadable, corrupt or
        --   incompatible generation.
    } deriving (Show, Eq)

emptyReferenceScan ∷ ReferenceScan
emptyReferenceScan = ReferenceScan Set.empty 0 []

data CleanupReport = CleanupReport
    { crReconcile          ∷ !ReconcileReport
    , crReferences         ∷ !ReferenceScan
    , crRemoved            ∷ ![GeneratedWorldId]
        -- ^ Final entries positively proven unreferenced and DETACHED to
        --   a tombstone — no longer an entry from this moment. The
        --   tombstone is deleted in the same run once the detachment and
        --   the registry are durable; otherwise it waits, identifiable,
        --   for the next run's sweep.
    , crRetainedReferenced ∷ ![GeneratedWorldId]
    , crRetainedPinned     ∷ ![GeneratedWorldId]
        -- ^ Retained because a process-owned operation pinned them.
    , crRetainedUnreadable ∷ ![FilePath]
    , crDeletionSuppressed ∷ !Bool
        -- ^ 'True' iff 'rsIndeterminate' was non-empty: no final entry
        --   was removed this run, whatever the scan otherwise said.
    , crTransientsRemoved  ∷ ![FilePath]
        -- ^ ABANDONED staging, displaced and tombstone directories
        --   plus registry candidate files swept — leftovers of earlier
        --   operations, never this run's own tombstones, which
        --   'crRemoved' accounts for.
    , crWarnings           ∷ ![Text]
    } deriving (Show, Eq)
