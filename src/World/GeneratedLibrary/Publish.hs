{-# LANGUAGE Strict #-}
-- | Atomic publication of one generated-world library entry (issue
--   #2024, requirements 3, 8, 9 and 10; D-17's republish-under-the-
--   saved-id). Built from "World.Save.Storage.Durable"'s primitives in
--   the save transaction's own order: stage, sync, validate from disk,
--   rename, sync, only then clean up.
--
--   === The transaction
--
--   1. Validate the payload (names, uniqueness, non-empty) and the
--      containment of the root and the entry path. Nothing is written
--      before this passes.
--   2. Create a uniquely named STAGING directory beside the final one
--      (same filesystem, so the commit can be one @rename(2)@).
--   3. Write every payload file into it, each flushed with @fsync@.
--      Then RE-READ every file from disk and compare it byte-for-byte
--      with what was meant to be written — an in-memory write success
--      is never sufficient, exactly as the save transaction insists.
--   4. Write the 'EntryRecord' LAST — binding the expected
--      'GeneratedWorldId' to the inventory and its digests — flush it,
--      re-read it, decode it, and compare it with the record intended.
--      Then @fsync@ the staging directory itself.
--   5. Examine the existing final, if any:
--
--      * none — commit (step 6) as 'PublishedNew';
--      * a complete entry with the SAME inventory digest — the staged
--        copy is discarded and nothing on disk changes:
--        'PublishedUnchanged';
--      * anything else (different content, or a final that is not a
--        complete entry) — the existing directory is renamed to a
--        @displaced@ recovery name and the root synced, so the old
--        complete entry survives, under an identifiable name, until
--        the new one AND the registry are durable; then commit as
--        'PublishedReplaced'.
--   6. Commit: rename the staging directory onto the final name — a
--      single atomic filesystem operation that either fully installs
--      the entry or does not happen — then @fsync@ the root. THAT sync
--      returning is the durability boundary; success is never reported
--      before it.
--   7. Past the boundary: update the registry, and only once the
--      registry write succeeded remove the displaced copy. Either
--      failing is a post-commit WARNING, never a failure — the entry is
--      already durable, and reconciliation repairs both from the
--      directory.
--
--   === Interruption at any point
--
--   Before step 6's rename: no final entry for a first publication; the
--   old complete entry either still at final (before step 5's
--   displacement) or at its @displaced@ name (after it), from which
--   reconciliation restores it. A staging directory left behind is
--   identifiable by name and swept by cleanup. From the rename onward:
--   the new complete entry is at final, and a leftover displaced copy
--   is post-commit garbage cleanup removes. There is no interleaving
--   that leaves a hybrid, because no file is ever written into a final
--   directory — a final only ever comes into being by rename.
--
--   === Structured failure
--
--   Every expected filesystem failure returns a 'LibraryFailure' naming
--   its phase; nothing escapes as an exception. On a failure after the
--   staging directory exists, the transaction removes its own staging
--   directory (best effort) and, if it had already displaced the old
--   entry, moves it back (best effort); what it cannot undo it leaves
--   for reconciliation, and never deletes.
module World.GeneratedLibrary.Publish
    ( PublishHooks(..)
    , noPublishHooks
    , publishUnlocked
    , claimTransientName
    , removeTransientDirectory
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.IORef as IORef
import qualified Data.List as L
import qualified Data.Text as T
import Control.Exception (IOException, SomeException, try)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory
    ( createDirectory, doesDirectoryExist, doesPathExist
    , removeDirectoryRecursive, renameDirectory )
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessID)
import World.Page.GeneratedId (GeneratedWorldId)
import World.Save.Storage.Durable
    ( rejectSymlinkedPath, rejectSymlinkedManagedPath, WriteStep(..)
    , writeBytesDurably, syncDirectory )
import World.GeneratedLibrary.Types
import World.GeneratedLibrary.Layout
import World.GeneratedLibrary.Entry
import World.GeneratedLibrary.Registry

-- | Observation points a test uses to interrupt or interfere with a
--   publication at a precise phase. Each receives the path it concerns.
--   Production passes 'noPublishHooks'; a hook that THROWS models a
--   crash (the transaction does not catch it, so what is on disk is
--   exactly what a crash would leave).
data PublishHooks = PublishHooks
    { phAfterPayloadWritten ∷ FilePath → IO ()
        -- ^ Every payload file flushed to the staging directory, before
        --   any is re-read.
    , phAfterStaged         ∷ FilePath → IO ()
        -- ^ The staging directory is complete and synced, before the
        --   existing final is examined.
    , phAfterDisplaced      ∷ FilePath → IO ()
        -- ^ The old entry has been moved to its displaced name, before
        --   the commit rename.
    }

noPublishHooks ∷ PublishHooks
noPublishHooks = PublishHooks (const (pure ())) (const (pure ())) (const (pure ()))

-- | The transaction. Caller holds the library lock.
publishUnlocked
    ∷ LibraryConfig → PublishHooks → GeneratedWorldId → [PayloadFile]
    → IO (Either LibraryFailure PublishReport)
publishUnlocked cfg hooks gid files =
    case validatePayload files of
        Left reason → pure (Left (failure LibPayloadIdentity Nothing reason))
        Right descriptors → do
            rootSafe ← rejectSymlinkedManagedPath root
            finalSafe ← rejectSymlinkedPath finalDir
            case rootSafe ≫ finalSafe of
                Left reason → pure (Left (failure LibUnsafePath (Just root) reason))
                Right () → do
                    staged ← createTransientDirectory root StagingDir gid
                    case staged of
                        Left e → pure (Left (failure LibStagingCreate (Just root) (tshow e)))
                        Right staging → stage descriptors staging
  where
    root     = lcRoot cfg
    finalDir = root </> entryDirectoryName gid
    failure phase path reason = LibraryFailure phase (Just gid) path reason
    newDigest descriptors = inventoryDigest descriptors

    -- Abort with a structured failure, removing the staging directory.
    abort staging f = do
        _ ← removeTransientDirectory staging
        pure (Left f)

    stage descriptors staging = do
        written ← writePayload staging
        case written of
            Left f → abort staging f
            Right () → do
                phAfterPayloadWritten hooks staging
                verified ← verifyPayload staging
                case verified of
                    Left f → abort staging f
                    Right () → do
                        now ← T.pack . iso8601Show ⊚ getCurrentTime
                        let rec = EntryRecord gid descriptors now
                        recorded ← writeRecord staging rec
                        case recorded of
                            Left f → abort staging f
                            Right () → do
                                synced ← try (syncDirectory staging)
                                case synced of
                                    Left (e ∷ SomeException) →
                                        abort staging (failure LibStagingSync (Just staging) (tshow e))
                                    Right () → do
                                        phAfterStaged hooks staging
                                        examineExisting descriptors staging rec

    writePayload staging = firstFailure ⊚ mapM (writeOne staging) files
    writeOne staging f = do
        let path = staging </> T.unpack (pfName f)
        r ← writeBytesDurably path (pfBytes f)
        pure $ case r of
            Left (StepFlush, e) → Left (failure LibPayloadFlush (Just path) (tshow e))
            Left (_, e)         → Left (failure LibPayloadWrite (Just path) (tshow e))
            Right ()            → Right ()

    verifyPayload staging = firstFailure ⊚ mapM (verifyOne staging) files
    verifyOne staging f = do
        let path = staging </> T.unpack (pfName f)
        r ← try (BS.readFile path)
        pure $ case r of
            Left (e ∷ IOException) → Left (failure LibPayloadReread (Just path) (tshow e))
            Right bytes
                | bytes ≢ pfBytes f →
                    Left (failure LibPayloadValidate (Just path)
                            "re-read payload file does not match the bytes written")
                | otherwise → Right ()

    writeRecord staging rec = do
        let path  = staging </> entryRecordFileName
            bytes = encodeEntryRecord rec
        w ← writeBytesDurably path bytes
        case w of
            Left (_, e) →
                pure (Left (failure LibRecordWrite (Just path) (tshow e)))
            Right () → do
                r ← try (BS.readFile path)
                pure $ case r of
                    Left (e ∷ IOException) →
                        Left (failure LibRecordValidate (Just path) (tshow e))
                    Right reread → case decodeEntryRecord reread of
                        Right rec' | rec' ≡ rec → Right ()
                        Right _ → Left (failure LibRecordValidate (Just path)
                                    "re-read entry record differs from the record written")
                        Left why → Left (failure LibRecordValidate (Just path) why)

    examineExisting descriptors staging rec = do
        exists ← doesDirectoryExist finalDir
        if not exists then commit staging rec (newDigest descriptors) Nothing PublishedNew else do
            existing ← readEntryDirectory finalDir
            case existing of
                Right (oldRec, oldDigest) | oldDigest ≡ newDigest descriptors → do
                    discardWarnings ← removeTransientDirectory staging
                    regWarnings ← upsertRegistryRow root (registryRowFor oldDigest oldRec)
                    pure (Right (PublishReport PublishedUnchanged
                                    (discardWarnings ⧺ regWarnings)))
                _ → do
                    displacedPath ← claimTransientName root DisplacedDir gid
                    moved ← try (renameDirectory finalDir displacedPath)
                    case moved of
                        Left (e ∷ IOException) →
                            abort staging (failure LibDisplaceExisting (Just finalDir) (tshow e))
                        Right () → do
                            synced ← try (syncDirectory root)
                            case synced of
                                Left (e ∷ SomeException) → do
                                    restoreDisplaced displacedPath
                                    abort staging (failure LibRootSync (Just root) (tshow e))
                                Right () → do
                                    phAfterDisplaced hooks displacedPath
                                    commit staging rec (newDigest descriptors)
                                           (Just displacedPath) PublishedReplaced

    -- Best effort: put a displaced entry back if the final is free. A
    -- failure here leaves the displaced copy for reconciliation, which
    -- restores it by the same rename.
    restoreDisplaced displacedPath = do
        finalFree ← not ⊚ doesPathExist finalDir
        when finalFree $ do
            r ← try (renameDirectory displacedPath finalDir)
            case (r ∷ Either SomeException ()) of _ → pure ()

    commit staging rec digest mDisplaced outcome = do
        renamed ← try (renameDirectory staging finalDir)
        case renamed of
            Left (e ∷ IOException) → do
                maybe (pure ()) restoreDisplaced mDisplaced
                abort staging (failure LibCommitRename (Just staging) (tshow e))
            Right () → do
                synced ← try (syncDirectory root)
                case synced of
                    -- The rename happened, so the entry is installed but
                    -- not PROVEN durable; report that honestly and
                    -- leave every directory exactly where it is.
                    Left (e ∷ SomeException) →
                        pure (Left (failure LibRootSync (Just root) (tshow e)))
                    Right () → do
                        regWarnings ← upsertRegistryRow root (registryRowFor digest rec)
                        displacedWarnings ← case mDisplaced of
                            Nothing → pure []
                            Just d
                                | null regWarnings → removeTransientDirectory d
                                | otherwise → pure
                                    [ "registry not durable; displaced copy retained at "
                                      <> T.pack d ]
                        pure (Right (PublishReport outcome (regWarnings ⧺ displacedWarnings)))

firstFailure ∷ [Either LibraryFailure ()] → Either LibraryFailure ()
firstFailure results = case [ f | Left f ← results ] of
    (f : _) → Left f
    []      → Right ()

-- Registry update ----------------------------------------------------------------

-- | Insert or replace one row. A registry that cannot be read is
--   rebuilt from the directory rather than trusted. Post-commit: every
--   failure is a warning.
upsertRegistryRow ∷ FilePath → RegistryRow → IO [Text]
upsertRegistryRow root row = do
    existing ← readRegistryFile root
    rowsResult ← case existing of
        RegistryPresent reg → pure (Right (rfRows reg))
        _ → do
            rebuilt ← inventoryFromDirectory root
            pure (committedRows ⊚ rebuilt)
    case rowsResult of
        Left reason → pure [ "registry not updated: " <> reason ]
        Right rows → do
            let merged = L.sortOn rrId (row : filter ((≢ rrId row) . rrId) rows)
            w ← writeRegistryFile root (RegistryFile merged)
            pure (either (: []) (const []) w)
  where
    committedRows entries =
        [ registryRowFor digest rec
        | e ← entries, leStatus e ≡ EntryCommitted
        , Just rec ← [leRecord e], Just digest ← [leDigest e] ]

-- Transient directories -------------------------------------------------------------

-- | Create a fresh transient directory for @gid@ under @root@. The name
--   carries the process id and a process-local counter; @mkdir@ is
--   itself atomic and exclusive, so a collision (another process's
--   counter agreeing with ours by chance) is simply retried with the
--   next number rather than shared.
createTransientDirectory
    ∷ FilePath → TransientKind → GeneratedWorldId → IO (Either IOException FilePath)
createTransientDirectory root kind gid = attempt (0 ∷ Int)
  where
    attempt n = do
        path ← nextTransientPath root kind gid
        r ← try (createDirectory path)
        case r of
            Right () → pure (Right path)
            Left e
                | isAlreadyExistsError e ∧ n < 64 → attempt (n + 1)
                | otherwise → pure (Left e)

-- | Claim a transient NAME that does not currently exist, for an
--   existing directory to be renamed onto. Same numbering as
--   'createTransientDirectory'; only ever called under the library
--   lock, so nothing else is claiming names in this root meanwhile.
claimTransientName ∷ FilePath → TransientKind → GeneratedWorldId → IO FilePath
claimTransientName root kind gid = do
    path ← nextTransientPath root kind gid
    taken ← doesPathExist path
    if taken then claimTransientName root kind gid else pure path

nextTransientPath ∷ FilePath → TransientKind → GeneratedWorldId → IO FilePath
nextTransientPath root kind gid = do
    pid ← getProcessID
    n ← IORef.atomicModifyIORef' transientCounter (\c → (c + 1, c))
    pure (root </> transientDirectoryName kind gid (fromIntegral pid) n)

-- | Remove a transient directory the library owns, reporting (never
--   throwing) a failure. Refuses a symlink: the directory would not be
--   the library's, whatever its name.
removeTransientDirectory ∷ FilePath → IO [Text]
removeTransientDirectory path = do
    linkSafe ← rejectSymlinkedPath path
    case linkSafe of
        Left reason → pure [ "not removing " <> T.pack path <> ": " <> reason ]
        Right () → do
            exists ← doesDirectoryExist path
            if not exists then pure [] else do
                r ← try (removeDirectoryRecursive path)
                pure $ case r of
                    Right () → []
                    Left (e ∷ SomeException) →
                        [ "failed to remove " <> T.pack path <> ": " <> tshow e ]

transientCounter ∷ IORef.IORef Word64
transientCounter = unsafePerformIO (IORef.newIORef 0)
{-# NOINLINE transientCounter #-}
