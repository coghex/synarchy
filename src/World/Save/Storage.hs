{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The atomic save-publication storage transaction (issue #762,
--   persistence-overhaul C1). Prior to this module, saving wrote the
--   already-encoded envelope bytes (issue #759/#760's tagged, checksummed
--   component container) straight to the slot's authoritative
--   @saves/\<slot\>\/world.synworld@ via a single 'Data.ByteString.writeFile'
--   ("World.Save.Serialize"'s old @writeSaveFilesUnsafe@) — a crash, power
--   loss, disk-full condition, or write failure mid-write could truncate
--   or corrupt the ONLY copy of that generation, with no way back.
--
--   This module never reads live gameplay state and never participates in
--   snapshot capture (issue #758's barrier already released by the time
--   'publishGeneration' runs — see "World.Thread.Command.Save.WriteWorld").
--   It receives only already-encoded bytes, the metadata that encode
--   produced, a slot directory, and a slot name for diagnostics, and
--   performs a classic write-validate-publish-rotate transaction:
--
--   1. Write the candidate to a UNIQUELY named temporary file in the SAME
--      slot directory (so the final publish step can use an atomic
--      same-filesystem rename) — never a shared/predictable name two
--      concurrent publishers could collide on
--      ('System.IO.openBinaryTempFile' handles this).
--   2. Flush + durably sync the candidate ('fileSynchronise', POSIX
--      @fsync@) before ever trusting it.
--   3. Re-read the candidate FROM DISK (not the in-memory bytes the
--      encoder produced) and fully decode + deep-force it, applying the
--      same validation bar the load path enforces — an in-memory encode
--      success is never sufcient on its own to publish.
--   4. Only once validated: rotate the CURRENT authoritative generation
--      (if any) into the single retained previous-generation slot via an
--      atomic rename, then atomically rename the validated candidate into
--      the authoritative path. Both renames use
--      'System.Directory.renameFile', which is POSIX @rename(2)@ — a
--      single atomic filesystem operation that either fully replaces the
--      destination or doesn't happen at all; there is no filesystem-level
--      "partway through a rename" state to defend against.
--   5. Sync the containing directory so the rename's directory-entry
--      change is itself durable, THEN report success — never before.
--
--   === Documented durability boundary (requirement 4)
--
--   \"Durable\" here means: the candidate's data + the directory entries
--   for both the rotate-to-previous rename and the publish rename have
--   been handed to 'fileSynchronise' (@fsync@) and that call returned
--   without error. On Linux (ext4/xfs/btrfs) this is a real durability
--   guarantee against a subsequent crash or power loss. On macOS, the
--   kernel's plain @fsync(2)@ does NOT guarantee the drive's own write
--   cache has been flushed to physical media — only @F_FULLFSYNC@ does,
--   and it is dramatically slower (typically 10-100x), so this module
--   deliberately uses the ordinary POSIX @fsync@ (via
--   'System.Posix.Unistd.fileSynchronise') on both platforms rather than
--   pay that cost on every save. This is the same trade-off essentially
--   every desktop application (this one is a game engine, not a
--   database) makes; a save is protected against process crashes,
--   application bugs, and disk-full/permission failures either way — the
--   narrower residual risk this accepts is a genuine OS-level crash or
--   power loss landing in the exact window between a macOS @fsync@
--   returning and the drive's own cache actually reaching platter.
--   Windows is not a supported platform (see CLAUDE.md Platform Notes)
--   and is out of scope.
--
--   === Crash-window recoverability (requirement 6)
--
--   Every phase above is ordered so that an interruption at ANY point —
--   before the temp file exists, mid-write, after write but before
--   validation, after validation but before rotation, mid-rotation,
--   mid-publish, after publish but before the directory syncs, or during
--   the final best-effort cleanup sweep — always leaves AT LEAST ONE of
--   the old or the new generation fully intact and selectable
--   ('selectLoadGeneration'), NEVER a partial/hybrid result:
--
--   * Anything before the publish rename completes: the authoritative
--     path is untouched (or, if the rotate rename already ran, the OLD
--     generation is sitting at the previous-generation path instead) —
--     either way a complete old generation is selectable.
--   * Anything from the publish rename onward: the authoritative path
--     already holds the complete NEW generation (a rename is atomic; it
--     cannot be observed half-applied).
--
--   A leftover, unpublished temporary file is always harmless dead
--   weight — never consulted by 'selectLoadGeneration' or
--   "World.Save.Serialize"'s listing (it lives under a name other than
--   @world.synworld@) — and is swept on the NEXT successful publish to
--   this slot, or left for a future explicit cleanup if none ever comes.
module World.Save.Storage
    ( authoritativeFileName
    , previousGenerationFileName
    , StoragePhase(..)
    , PublishFailure(..)
    , renderPublishFailure
    , publishGeneration
    , GenerationSource(..)
    , LoadSelection(..)
    , selectLoadGeneration
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import Control.Exception (IOException, SomeException, try, finally)
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, removeFile, listDirectory
    , renameFile )
import System.FilePath ((</>))
import System.IO (Handle, openBinaryTempFile, hFlush, hClose)
import System.Posix.IO
    (OpenMode(..), closeFd, defaultFileFlags, handleToFd, openFd)
import System.Posix.Unistd (fileSynchronise)
import World.Save.Types (SaveData, SaveMetadata(..), checkWorldCount)
import World.Save.Envelope
    ( decodeSessionEnvelope, decodeSessionEnvelopeClassified
    , GenerationFailure(..), renderGenerationFailure )
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)

-- File naming ----------------------------------------------------------

-- | The slot's authoritative generation — the same name #759/#760 always
--   used; only how it gets WRITTEN changes here.
authoritativeFileName ∷ FilePath
authoritativeFileName = "world.synworld"

-- | The slot's single retained previous generation (requirement 5). Not
--   named @world.synworld@ and not laid out as a sibling slot directory,
--   so it is naturally invisible to ordinary listing/loading unless
--   explicitly consulted as a fallback ('selectLoadGeneration',
--   requirements 2 and 8).
previousGenerationFileName ∷ FilePath
previousGenerationFileName = "world.synworld.prev"

-- | Every candidate this transaction ever creates is opened through
--   'System.IO.openBinaryTempFile' with this template — the RTS mixes in
--   its own per-call unique suffix (retried under an exclusive create on
--   collision), so concurrent publishers to the same slot can never
--   predictably collide on a shared temp name (requirement 2).
candidateTemplate ∷ String
candidateTemplate = "world.synworld.tmp"

-- | The pre-#759 companion file. #759 requirement 5 made it
--   non-authoritative and unused; requirement 13 here lets a successful
--   publication remove a stale leftover copy belonging to THIS slot.
staleGenYamlFileName ∷ FilePath
staleGenYamlFileName = "world_gen.yaml"

-- Publication ------------------------------------------------------------

-- | Every phase 'publishGeneration' can fail in, in the order a real
--   publication reaches them (requirement 10: an error names the storage
--   phase, never just "save failed").
data StoragePhase
    = PhaseDirectoryCreate
    | PhaseCandidateCreate
    | PhaseCandidateWrite
    | PhaseCandidateFlush
    | PhaseCandidateReread
    | PhaseCandidateValidate
    | PhaseRotatePrevious
    | PhasePublishRename
    | PhaseDirectorySync
    deriving (Show, Eq, Enum, Bounded)

data PublishFailure = PublishFailure
    { pfPhase  ∷ !StoragePhase
    , pfSlot   ∷ !Text
    , pfPath   ∷ !(Maybe FilePath)
    , pfReason ∷ !Text
    } deriving (Show, Eq)

renderPublishFailure ∷ PublishFailure → Text
renderPublishFailure f =
    "save storage failed for slot '" <> pfSlot f <> "' during "
        <> T.pack (show (pfPhase f)) <> pathSuffix <> ": " <> pfReason f
  where
    pathSuffix = maybe "" (\p → " (" <> T.pack p <> ")") (pfPath f)

showT ∷ Show a ⇒ a → Text
showT = T.pack . show

-- | The complete storage transaction (requirement 1). Receives ONLY: the
--   slot directory (created if missing), a slot name (diagnostics only —
--   NOT re-sanitized here, the caller already owns that), the metadata
--   this candidate is expected to decode back to, and its complete,
--   already-encoded envelope bytes. Never reads live gameplay state and
--   never participates in snapshot capture.
--
--   On success, returns any NON-FATAL cleanup warnings — by the time
--   cleanup runs the documented durability boundary (the publish rename
--   plus directory sync) has already completed, so a cleanup failure is
--   reported for diagnostics but never turned into an overall save
--   failure (requirement 10's "cleanup failure" case).
publishGeneration
    ∷ FilePath        -- ^ slot directory
    → Text             -- ^ slot name (diagnostics only)
    → SaveMetadata      -- ^ metadata this candidate must decode back to
    → BS.ByteString      -- ^ complete, already-encoded envelope bytes
    → IO (Either PublishFailure [Text])
publishGeneration dir slotName expectedMeta encoded = do
    dirResult ← try (createDirectoryIfMissing True dir)
    case dirResult of
        Left (e ∷ IOException) →
            pure (Left (failure PhaseDirectoryCreate (Just dir) (showT e)))
        Right () → do
            created ← try (openBinaryTempFile dir candidateTemplate)
            case created of
                Left (e ∷ IOException) →
                    pure (Left (failure PhaseCandidateCreate (Just dir) (showT e)))
                Right (tempPath, h) →
                    writeValidateAndPublish dir slotName expectedMeta encoded
                        tempPath h
                        `finally` cleanupLeftoverTemp tempPath
  where
    failure = publishFailureFor slotName

-- | Build a 'PublishFailure' for a given slot. A plain top-level
--   constructor application — kept as a named helper (rather than a
--   local @where@-bound closure duplicated at every call site) so every
--   phase below reads as one line.
publishFailureFor ∷ Text → StoragePhase → Maybe FilePath → Text → PublishFailure
publishFailureFor slotName phase path reason =
    PublishFailure phase slotName path reason

writeValidateAndPublish
    ∷ FilePath → Text → SaveMetadata → BS.ByteString → FilePath → Handle
    → IO (Either PublishFailure [Text])
writeValidateAndPublish dir slotName expectedMeta encoded tempPath h = do
    writeResult ← try (BS.hPut h encoded)
    case writeResult of
        Left (e ∷ IOException) → do
            closeQuietly h
            pure (Left (fail' PhaseCandidateWrite (Just tempPath) (showT e)))
        Right () → do
            flushResult ← try (durableFlush h)
            case flushResult of
                Left (e ∷ SomeException) →
                    pure (Left (fail' PhaseCandidateFlush (Just tempPath) (showT e)))
                Right () → do
                    rereadResult ← try (BS.readFile tempPath)
                    case rereadResult of
                        Left (e ∷ IOException) →
                            pure (Left (fail' PhaseCandidateReread (Just tempPath) (showT e)))
                        Right rereadBytes →
                            case validateCandidate expectedMeta rereadBytes of
                                Left reason →
                                    pure (Left (fail' PhaseCandidateValidate
                                                    (Just tempPath) reason))
                                Right () →
                                    publishValidated dir slotName tempPath
  where
    fail' = publishFailureFor slotName

-- | Flush the RTS-level write buffer, then durably sync the underlying
--   file descriptor (POSIX @fsync@ — see the module haddock's durability
--   boundary section) before this candidate is trusted for anything.
--   'handleToFd' takes ownership of (and closes) the Haskell-level
--   'Handle'; the caller must not use @h@ again after this returns,
--   success or failure.
durableFlush ∷ Handle → IO ()
durableFlush h = do
    hFlush h
    fd ← handleToFd h
    fileSynchronise fd `finally` closeFd fd

closeQuietly ∷ Handle → IO ()
closeQuietly h = do
    r ← try (hClose h)
    case (r ∷ Either SomeException ()) of _ → pure ()

-- | Fully decode the re-read candidate bytes and confirm they describe
--   exactly the intended save request (requirement 3): the SAME
--   validation bar the load path enforces — complete envelope structure,
--   checksums, required components, component decode/migrate/validate,
--   and 'checkWorldCount' — PLUS a re-read-metadata-matches-request
--   check the load path has no analogue for (loading never has an
--   "expected" name/timestamp to compare against; publishing does). An
--   in-memory encode success alone is never sufficient — only what
--   'BS.hPut' actually put on disk, read back, counts.
validateCandidate ∷ SaveMetadata → BS.ByteString → Either Text ()
validateCandidate expectedMeta bytes = do
    (meta, snap) ← decodeSessionEnvelope bytes
    when (smName meta ≢ smName expectedMeta
          ∨ smTimestamp meta ≢ smTimestamp expectedMeta) $
        Left "re-read candidate metadata does not match the intended save \
             \request (slot name/timestamp mismatch)"
    let req = SaveRequestMeta { srmSlotName  = smName meta
                              , srmTimestamp = smTimestamp meta }
    sd ← checkWorldCount (snapshotToSaveData req snap)
    -- Deep-force the reconstructed SaveData: decoding alone only forces
    -- each field to WHNF (Strict/HashMap.Strict force the outer shape,
    -- not every nested leaf — the same caveat #758's own capture-side
    -- forcing documents). Walking a derived Show instance's full spine
    -- forces every scalar leaf transitively, since you cannot know a
    -- value's printed length without evaluating it — the same
    -- full-traversal guarantee 'Data.Serialize.encode' gives the write
    -- side, achieved here without adding NFData instances across the
    -- entire save-data tree.
    length (show sd) `seq` Right ()

-- | Rotate the current authoritative generation (if any) into the
--   previous-generation slot, then atomically publish the validated
--   candidate as the new authoritative generation, then sync the
--   directory. Called only after 'validateCandidate' has already
--   accepted the candidate — never a candidate that merely encoded
--   successfully in memory.
publishValidated ∷ FilePath → Text → FilePath → IO (Either PublishFailure [Text])
publishValidated dir slotName tempPath = do
    let authPath = dir </> authoritativeFileName
        prevPath = dir </> previousGenerationFileName
    authExists ← doesFileExist authPath
    rotateResult ←
        if authExists
          then try (renameFile authPath prevPath)
          else pure (Right ())
    case rotateResult of
        Left (e ∷ IOException) →
            pure (Left (fail' PhaseRotatePrevious (Just authPath) (showT e)))
        Right () → do
            publishResult ← try (renameFile tempPath authPath)
            case publishResult of
                Left (e ∷ IOException) →
                    pure (Left (fail' PhasePublishRename (Just tempPath) (showT e)))
                Right () → do
                    syncResult ← try (syncDirectory dir)
                    case syncResult of
                        Left (e ∷ SomeException) →
                            pure (Left (fail' PhaseDirectorySync (Just dir) (showT e)))
                        Right () →
                            Right ⊚ cleanupStaleArtifacts dir
  where
    fail' = publishFailureFor slotName

syncDirectory ∷ FilePath → IO ()
syncDirectory dir = do
    fd ← openFd dir ReadOnly defaultFileFlags
    fileSynchronise fd `finally` closeFd fd

-- | Best-effort removal of every leftover temporary candidate belonging
--   to this slot (from this or an earlier interrupted publish — the just
--   -published one is gone already, having been renamed away) plus a
--   stale pre-#759 @world_gen.yaml@ companion (requirement 13). Runs
--   only AFTER the documented durability boundary has already completed
--   (the caller), so any failure here is reported as a warning, never as
--   an overall publish failure.
cleanupStaleArtifacts ∷ FilePath → IO [Text]
cleanupStaleArtifacts dir = do
    staleTemps ← listStaleTempFiles dir
    tempWarnings ← concat ⊚ mapM removeIfExists staleTemps
    yamlWarnings ← removeIfExists (dir </> staleGenYamlFileName)
    pure (tempWarnings ⧺ yamlWarnings)

listStaleTempFiles ∷ FilePath → IO [FilePath]
listStaleTempFiles dir = do
    listed ← try (listDirectory dir)
    case (listed ∷ Either SomeException [FilePath]) of
        Left _        → pure []
        Right entries → pure
            [ dir </> e | e ← entries, candidateTemplate `L.isPrefixOf` e ]

removeIfExists ∷ FilePath → IO [Text]
removeIfExists path = do
    exists ← doesFileExist path
    if not exists then pure [] else do
        r ← try (removeFile path)
        case r of
            Right () → pure []
            Left (e ∷ IOException) →
                pure [ "failed to remove stale artifact " <> T.pack path
                     <> ": " <> showT e ]

-- | Best-effort: remove the ORIGINAL temp file if the transaction ended
--   (success or failure) without renaming it away. A successful publish
--   already moved it to the authoritative path, so this is a no-op then;
--   an unpublished candidate is harmless dead weight either way (never
--   consulted by 'selectLoadGeneration' or listing) and gets swept by
--   'cleanupStaleArtifacts' on this slot's next successful publish if
--   this removal itself fails or never runs.
cleanupLeftoverTemp ∷ FilePath → IO ()
cleanupLeftoverTemp tempPath = do
    exists ← doesFileExist tempPath
    when exists $ do
        r ← try (removeFile tempPath)
        case (r ∷ Either SomeException ()) of _ → pure ()

-- Load-source selection ----------------------------------------------

data GenerationSource = FromAuthoritative | FromPrevious
    deriving (Show, Eq)

data LoadSelection = LoadSelection
    { lsSource   ∷ !GenerationSource
    , lsDetail   ∷ !Text          -- ^ human-readable "why" (requirement 7)
    , lsSaveData ∷ !SaveData
    } deriving (Show)

-- | Load-source selection (requirement 7). A fully valid authoritative
--   generation always wins. A STORAGE-corrupt authoritative (absent,
--   truncated, bad framing, or a checksum failure — see
--   'World.Save.Envelope.GenerationFailure') falls back to a fully valid
--   previous generation. A coherent-but-INCOMPATIBLE authoritative
--   (unsupported schema version, failed content/gameplay validation) is
--   reported directly and NEVER triggers a fallback — silently rolling
--   back to an older generation would hide a real compatibility problem
--   instead of reporting it. Never combines components across
--   generations: exactly one generation's 'SaveData' is ever returned.
selectLoadGeneration ∷ FilePath → Text → IO (Either Text LoadSelection)
selectLoadGeneration dir slotName = do
    let authPath = dir </> authoritativeFileName
        prevPath = dir </> previousGenerationFileName
    authExists ← doesFileExist authPath
    if not authExists
      then fallbackToPrevious prevPath "authoritative save file is missing"
      else do
        authResult ← decodeGenerationFile authPath
        case authResult of
            Right sd →
                pure (Right (LoadSelection FromAuthoritative
                        "authoritative generation" sd))
            Left (GenerationIncompatible reason) →
                pure (Left (incompatibleMessage reason))
            Left (GenerationCorrupt reason) →
                fallbackToPrevious prevPath reason
  where
    incompatibleMessage reason =
        "save '" <> slotName <> "' authoritative generation is present but \
        \incompatible with this build (not a storage-corruption fallback \
        \case): " <> reason
    fallbackToPrevious prevPath authReason = do
        prevExists ← doesFileExist prevPath
        if not prevExists
          then pure (Left ("save '" <> slotName
                <> "' authoritative generation is unreadable (" <> authReason
                <> ") and no previous generation exists to recover from"))
          else do
            prevResult ← decodeGenerationFile prevPath
            case prevResult of
                Right sd →
                    pure (Right (LoadSelection FromPrevious
                        ("authoritative generation unreadable (" <> authReason
                         <> "); recovered from the previous generation") sd))
                Left prevFail →
                    pure (Left ("save '" <> slotName
                        <> "' authoritative generation is unreadable ("
                        <> authReason
                        <> ") and the previous generation is also unusable ("
                        <> renderGenerationFailure prevFail <> ")"))

decodeGenerationFile ∷ FilePath → IO (Either GenerationFailure SaveData)
decodeGenerationFile path = do
    readResult ← try (BS.readFile path)
    case readResult of
        Left (e ∷ IOException) →
            pure (Left (GenerationCorrupt ("cannot read: " <> showT e)))
        Right bytes → pure $ do
            (meta, snap) ← decodeSessionEnvelopeClassified bytes
            let req = SaveRequestMeta (smName meta) (smTimestamp meta)
            either (Left . GenerationCorrupt) Right
                   (checkWorldCount (snapshotToSaveData req snap))
